use unicode_segmentation::UnicodeSegmentation;

pub struct TextChunker {
    /// Maximum characters per chunk
    pub max_chars: usize,
}

impl TextChunker {
    pub fn new(max_chars: usize) -> Self {
        Self { max_chars }
    }

    /// Chunks a larger string of text into a vector of chunks that each
    /// stay approximately under `max_chars` by splitting cleanly along sentence boundaries.
    pub fn chunk(&self, text: &str) -> Vec<String> {
        let mut chunks = Vec::new();
        let mut current_chunk = String::new();

        // `unicode_sentences()` safely handles language-aware sentence boundaries
        for sentence in text.unicode_sentences() {
            let sentence = sentence.trim();
            if sentence.is_empty() {
                continue;
            }

            // If adding the next sentence exceeds our limit, push the current chunk
            if current_chunk.len() + sentence.len() + 1 > self.max_chars {
                if !current_chunk.is_empty() {
                    chunks.push(current_chunk.trim().to_string());
                    current_chunk.clear();
                }
            }

            if !current_chunk.is_empty() {
                current_chunk.push(' ');
            }
            current_chunk.push_str(sentence);
        }

        // Push any remaining text as the final chunk
        if !current_chunk.is_empty() {
            chunks.push(current_chunk.trim().to_string());
        }

        chunks
    }
}
