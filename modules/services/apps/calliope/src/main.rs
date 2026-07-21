use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};

mod domain;
mod parser;
mod chunker;
mod llm;
mod tts;
mod config;

/// Calliope - An AI-powered audiobook creator
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to a TOML configuration file
    #[arg(long, default_value = "config.toml")]
    config: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Convert an ebook to an audiobook
    Convert {
        /// Path to the input EPUB file
        #[arg(short, long)]
        input: String,

        /// Path to the output directory
        #[arg(short, long)]
        output: String,
        
        /// Language code (e.g. "en", "ja")
        #[arg(short, long)]
        language: Option<String>,

        /// Do a dry run (parse only, no audio generation)
        #[arg(long)]
        dry_run: bool,
        
        /// LLM API Base URL (OpenAI compatible)
        #[arg(long)]
        llm_base_url: Option<String>,
        
        /// LLM Model to use for emotion and character tagging
        #[arg(long)]
        llm_model: Option<String>,
        
        /// Emotion strategy: "metadata" (global per line) or "inline" (tags inside text)
        #[arg(long)]
        emotion_strategy: Option<String>,
        
        /// TTS Engine to use (e.g., openai, mock)
        #[arg(long)]
        tts_engine: Option<String>,
        
        /// TTS API Base URL
        #[arg(long)]
        tts_base_url: Option<String>,

        /// TTS Model Name
        #[arg(long)]
        tts_model: Option<String>,

        /// LLM API Key (optional for local endpoints)
        #[arg(long)]
        llm_api_key: Option<String>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Convert { input, output, language, dry_run, llm_base_url, llm_model, emotion_strategy, tts_engine, tts_base_url, tts_model, llm_api_key } => {
            println!("Starting conversion for {}...", input);
            
            // Load configuration
            let cfg = config::CalliopeConfig::load(&cli.config)?;
            if cli.config.exists() {
                println!("Loaded configuration from {:?}", cli.config);
            }
            
            // Layer resolution
            let final_llm_base = llm_base_url.clone().or(cfg.llm.base_url).unwrap_or_else(|| "http://localhost:11434/v1".to_string());
            let final_llm_model = llm_model.clone().or(cfg.llm.model).unwrap_or_else(|| "llama3".to_string());
            let final_llm_api_key = llm_api_key.clone().or(cfg.llm.api_key).unwrap_or_else(|| "".to_string());
            let final_emotion_strat = emotion_strategy.clone().or(cfg.llm.emotion_strategy).unwrap_or_else(|| "inline".to_string());
            
            // For LLM, default to "Italian" if passed via CLI or config, else default to "auto"
            let final_llm_lang = language.clone().or(cfg.llm.language).unwrap_or_else(|| "Italian".to_string());
            
            // Initialize LLM Client
            let is_inline = final_emotion_strat.to_lowercase() == "inline";
            let llm_client = llm::LlmClient::new(&final_llm_base, &final_llm_api_key, &final_llm_model, is_inline, &final_llm_lang);
            println!("Connected to LLM backend at {} using model {}", final_llm_base, final_llm_model);
            if is_inline {
                println!("Using INLINE emotion tagging strategy.");
            } else {
                println!("Using METADATA emotion tagging strategy.");
            }
            
            // TTS
            let final_tts_engine = tts_engine.clone().or(cfg.tts.engine).unwrap_or_else(|| "mock".to_string());
            if final_tts_engine == "openai" {
                let final_tts_model = tts_model.clone().or(cfg.tts.model).unwrap_or_else(|| "tts-1".to_string());
                let final_tts_base = tts_base_url.clone().or(cfg.tts.base_url).unwrap_or_else(|| "http://localhost:8080/v1".to_string());
                let final_tts_voices = cfg.tts.voices.unwrap_or_default();
                
                // For TTS, only send language if explicitly set in tts config (since auto inference might be better)
                let final_tts_lang = cfg.tts.language.clone();
                
                println!("Using OpenAI-compatible TTS Engine at {} (Model: {})", final_tts_base, final_tts_model);
                let tts_client = tts::OpenAiTtsEngine::new(&final_tts_base, &final_tts_model, final_tts_voices, final_tts_lang);
                process_book(&input, &output, *dry_run, &llm_client, &tts_client).await?;
            } else {
                println!("Warning: Unknown TTS engine '{}', falling back to MOCK.", final_tts_engine);
                let tts_client = tts::MockTtsEngine;
                process_book(input, output, *dry_run, &llm_client, &tts_client).await?;
            }
        }
    }

    Ok(())
}

async fn process_book<T: tts::TtsEngine>(
    input: &String,
    output_dir: &String,
    dry_run: bool,
    llm_client: &llm::LlmClient,
    tts_client: &T,
) -> Result<()> {
    let out_path = std::path::Path::new(output_dir);
    std::fs::create_dir_all(out_path).context("Failed to create output directory")?;

    let mut parser = parser::EpubParser::new(input)?;
    let book = parser.parse_book()?;
    
    println!("Found {} chapters in '{}'.", book.chapters.len(), book.title);
    let chunker = chunker::TextChunker::new(1500);
    
    struct ChapterData {
        title: String,
        path: std::path::PathBuf,
        duration_ms: u64,
    }
    let mut chapter_files: Vec<ChapterData> = Vec::new();
    
    let m = MultiProgress::new();
    let sty_book = ProgressStyle::with_template(
        "[{elapsed_precise}] {bar:20.cyan/blue} {pos:>2}/{len:2} | {msg}"
    ).unwrap().progress_chars("██░");
    
    let book_pb = m.add(ProgressBar::new(book.chapters.len() as u64));
    book_pb.set_style(sty_book);

    let sty_chapter = ProgressStyle::with_template(
        "[{elapsed_precise}] {bar:20.yellow/blue} {pos:>2}/{len:2} | Chunk {msg} | {eta}"
    ).unwrap().progress_chars("██░");
    
    let sty_spinner = ProgressStyle::with_template("{spinner:.green} {msg}")
        .unwrap()
        .tick_chars("⠁⠂⠄⡀⢀⠠⠐⠈ ");
        
    let chapter_pb = m.add(ProgressBar::new(0));
    chapter_pb.set_style(sty_chapter);
    
    let spinner_pb = m.add(ProgressBar::new_spinner());
    spinner_pb.set_style(sty_spinner);
    spinner_pb.enable_steady_tick(std::time::Duration::from_millis(100));
    
    for (chapter_idx, chapter) in book.chapters.iter().enumerate() {
        let title_short: String = book.title.chars().take(15).collect();
        book_pb.set_message(format!("{} Ch {}", title_short, chapter_idx + 1));
        let chapter_path = out_path.join(format!("chapter_{:03}.wav", chapter_idx + 1));
        
        if chapter_path.exists() && !dry_run {
            if let Ok(reader) = hound::WavReader::open(&chapter_path) {
                let spec = reader.spec();
                let duration_ms = (reader.duration() as u64 * 1000) / spec.sample_rate as u64;
                chapter_files.push(ChapterData {
                    title: chapter.title.clone(),
                    path: chapter_path.clone(),
                    duration_ms,
                });
            }
            book_pb.inc(1);
            continue;
        }

        let chunks = chunker.chunk(&chapter.content);
        
        chapter_pb.set_length(chunks.len() as u64);
        chapter_pb.set_position(0);
        chapter_pb.reset_eta();
        
        let mut global_line_idx = 1;
        let mut previous_context = String::new();
        
        let mut master_samples: Vec<i16> = Vec::new();
        let mut master_spec: Option<hound::WavSpec> = None;
        
        for (chunk_idx, chunk_text) in chunks.iter().enumerate() {
            chapter_pb.set_message(format!("{}/{}", chunk_idx + 1, chunks.len()));
            spinner_pb.set_message("LLM is annotating emotions...");
            
            match llm_client.annotate_chunk(&previous_context, chunk_text).await {
                Ok(response) => {
                    for line in response.lines.iter() {
                        let text_preview: String = line.text.chars().take(25).collect();
                        spinner_pb.set_message(format!("{:?} [{:?}]: {}...", line.speaker.0, line.emotion, text_preview));
                            
                        if !dry_run {
                            match tts_client.generate_audio(line).await {
                                Ok(audio_bytes) => {
                                    let cursor = std::io::Cursor::new(audio_bytes);
                                    if let Ok(mut reader) = hound::WavReader::new(cursor) {
                                        if master_spec.is_none() {
                                            master_spec = Some(reader.spec());
                                        }
                                        
                                        let mut samples: Vec<i16> = reader.samples::<i16>().filter_map(Result::ok).collect();
                                        
                                        let fade_duration = 0.03;
                                        let sample_rate = reader.spec().sample_rate as f32;
                                        let fade_samples = (fade_duration * sample_rate) as usize;
                                        let len = samples.len();
                                        
                                        for i in 0..fade_samples.min(len) {
                                            let multiplier = i as f32 / fade_samples as f32;
                                            samples[i] = (samples[i] as f32 * multiplier) as i16;
                                        }
                                        
                                        for i in 0..fade_samples.min(len) {
                                            let multiplier = i as f32 / fade_samples as f32;
                                            let idx = len - 1 - i;
                                            samples[idx] = (samples[idx] as f32 * multiplier) as i16;
                                        }
                                        
                                        master_samples.extend(samples);
                                    }
                                },
                                Err(e) => spinner_pb.set_message(format!("TTS generation failed: {}", e))
                            }
                        }
                        global_line_idx += 1;
                    }
                },
                Err(e) => spinner_pb.set_message(format!("LLM Request Failed: {:?}", e)),
            }
            previous_context = chunk_text.clone();
            chapter_pb.inc(1);
            
            // To prevent massive loops during testing, allow dry_run to abort early
            if dry_run { break; }
        }
        
        if dry_run {
            println!("Dry run mode enabled. Exiting before full generation.");
            return Ok(());
        }
        
        if let Some(spec) = master_spec {
            let mut writer = hound::WavWriter::create(&chapter_path, spec)?;
            for sample in &master_samples {
                writer.write_sample(*sample)?;
            }
            writer.finalize()?;
            
            let duration_ms = (master_samples.len() as u64 * 1000) / (spec.sample_rate as u64 * spec.channels as u64);
            chapter_files.push(ChapterData {
                title: chapter.title.clone(),
                path: chapter_path.clone(),
                duration_ms,
            });
        }
        book_pb.inc(1);
    }
    
    spinner_pb.finish_and_clear();
    chapter_pb.finish_and_clear();
    book_pb.finish_with_message("Complete!");
    
    // M4B Packaging
    if !chapter_files.is_empty() {
        println!("\nPackaging {} chapters into M4B audiobook...", chapter_files.len());
        let mut metadata = String::from(";FFMETADATA1\n");
        metadata.push_str(&format!("title={}\n", book.title));
        
        let mut current_time_ms: u64 = 0;
        let mut concat_txt = String::new();
        
        for data in &chapter_files {
            if let Ok(abs_path) = std::fs::canonicalize(&data.path) {
                concat_txt.push_str(&format!("file '{}'\n", abs_path.display()));
            }
            
            let end_time_ms = current_time_ms + data.duration_ms;
            
            metadata.push_str("\n[CHAPTER]\n");
            metadata.push_str("TIMEBASE=1/1000\n");
            metadata.push_str(&format!("START={}\n", current_time_ms));
            metadata.push_str(&format!("END={}\n", end_time_ms));
            metadata.push_str(&format!("title={}\n", data.title));
            
            current_time_ms = end_time_ms;
        }
        
        let meta_path = out_path.join("metadata.txt");
        std::fs::write(&meta_path, metadata)?;
        
        let concat_path = out_path.join("concat.txt");
        std::fs::write(&concat_path, concat_txt)?;
        
        let m4b_path = out_path.join(format!("{}.m4b", book.title.replace("/", "_").replace(" ", "_")));
        
        let mut cover_path_opt = None;
        if let Some((cover_bytes, mime)) = &book.cover_image {
            let ext = if mime.contains("png") { "png" } else { "jpg" };
            let cover_path = out_path.join(format!("cover.{}", ext));
            if let Err(e) = std::fs::write(&cover_path, cover_bytes) {
                println!("Warning: Failed to save cover image: {}", e);
            } else {
                cover_path_opt = Some(cover_path);
            }
        }

        let mut ffmpeg_args = vec![
            "-y".to_string(),
            "-f".to_string(), "concat".to_string(),
            "-safe".to_string(), "0".to_string(),
            "-i".to_string(), concat_path.to_str().unwrap().to_string(),
            "-i".to_string(), meta_path.to_str().unwrap().to_string(),
        ];
        
        if let Some(cover_path) = &cover_path_opt {
            ffmpeg_args.push("-i".to_string());
            ffmpeg_args.push(cover_path.to_str().unwrap().to_string());
            ffmpeg_args.push("-map".to_string());
            ffmpeg_args.push("0:a".to_string());
            ffmpeg_args.push("-map".to_string());
            ffmpeg_args.push("2:v".to_string());
            ffmpeg_args.push("-c:v".to_string());
            ffmpeg_args.push("mjpeg".to_string());
            ffmpeg_args.push("-disposition:v".to_string());
            ffmpeg_args.push("attached_pic".to_string());
        } else {
            ffmpeg_args.push("-map".to_string());
            ffmpeg_args.push("0:a".to_string());
        }
        
        ffmpeg_args.extend(vec![
            "-map_metadata".to_string(), "1".to_string(),
            "-c:a".to_string(), "aac".to_string(),
            "-b:a".to_string(), "64k".to_string(),
            m4b_path.to_str().unwrap().to_string()
        ]);
        
        let status = std::process::Command::new("ffmpeg")
            .args(ffmpeg_args)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()?;
            
        if status.success() {
            println!("Successfully created audiobook: {}", m4b_path.display());
        } else {
            println!("Warning: FFmpeg failed to create M4B.");
        }
    }
    
    Ok(())
}
