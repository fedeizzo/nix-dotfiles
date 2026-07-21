use anyhow::Result;
use epub::doc::EpubDoc;
use regex::Regex;
use scraper::{Html, Selector};
use std::fs::File;
use std::io::BufReader;
use crate::domain::book::{Book, Chapter};

pub struct EpubParser {
    doc: EpubDoc<BufReader<File>>,
    space_regex: Regex,
}

impl EpubParser {
    pub fn new(path: &str) -> Result<Self> {
        let doc = EpubDoc::new(path)
            .map_err(|e| anyhow::anyhow!("Failed to open EPUB at {}: {:?}", path, e))?;
        let space_regex = Regex::new(r"\s+").unwrap();

        Ok(Self { doc, space_regex })
    }

    /// Parses the entire EPUB and returns a domain Book entity
    pub fn parse_book(&mut self) -> Result<Book> {
        let mut chapters = Vec::new();
        
        let title = self.doc.mdata("title")
            .map(|m| m.value.clone())
            .unwrap_or_else(|| "Unknown Title".to_string());
        
        // Start from the first item
        self.doc.set_current_chapter(0);

        loop {
            if let Some(chapter) = self.process_current_chapter(chapters.len()) {
                chapters.push(chapter);
            }

            if !self.doc.go_next() {
                break;
            }
        }
        
        let cover_image = self.doc.get_cover();

        Ok(Book {
            title,
            chapters,
            cover_image,
        })
    }

    fn process_current_chapter(&mut self, current_chapter_count: usize) -> Option<Chapter> {
        let (content_bytes, _mime) = self.doc.get_current()?;
        let content_html = String::from_utf8_lossy(&content_bytes).to_string();

        let document = Html::parse_document(&content_html);

        let extracted_text = Self::extract_text_from_html(&document);
        let cleaned_text = self.clean_whitespace(&extracted_text);

        // Ignore empty chapters
        if cleaned_text.trim().is_empty() {
            return None;
        }

        let title = Self::derive_title(&document, &cleaned_text, current_chapter_count);

        Some(Chapter {
            title,
            content: cleaned_text.trim().to_string(),
        })
    }

    fn extract_text_from_html(document: &Html) -> String {
        let body_selector = Selector::parse("body").unwrap();

        if let Some(body) = document.select(&body_selector).next() {
            body.text().collect::<Vec<_>>().join(" ")
        } else {
            document.root_element().text().collect::<Vec<_>>().join(" ")
        }
    }

    fn clean_whitespace(&self, text: &str) -> String {
        self.space_regex.replace_all(text, " ").to_string()
    }

    fn derive_title(document: &Html, cleaned_text: &str, current_chapter_count: usize) -> String {
        let title_selector = Selector::parse("title").unwrap();
        let h1_selector = Selector::parse("h1").unwrap();
        let h2_selector = Selector::parse("h2").unwrap();

        if let Some(node) = document.select(&title_selector).next() {
            return node.text().collect::<Vec<_>>().join(" ").trim().to_string();
        }
        if let Some(node) = document.select(&h1_selector).next() {
            return node.text().collect::<Vec<_>>().join(" ").trim().to_string();
        }
        if let Some(node) = document.select(&h2_selector).next() {
            return node.text().collect::<Vec<_>>().join(" ").trim().to_string();
        }

        let text_len = cleaned_text.chars().count();
        if text_len > 0 {
            cleaned_text
                .chars()
                .take(60)
                .collect::<String>()
                .trim()
                .to_string()
        } else {
            format!("Chapter {}", current_chapter_count + 1)
        }
    }
}
