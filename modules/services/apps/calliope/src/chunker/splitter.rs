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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk() {
        let chunker = TextChunker::new(100);
        let text = "
            Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam egestas libero at purus ultrices ornare. Fusce consectetur libero et semper tempor. Duis ac felis vitae lacus convallis convallis sed in massa. Pellentesque at venenatis nunc. Fusce at ligula ex. Sed sed nisl et orci blandit pellentesque nec pulvinar magna. Sed eu sem ac ante faucibus maximus. Integer laoreet ligula feugiat accumsan luctus.

           Vivamus efficitur finibus porttitor. Sed malesuada, ligula et tristique pretium, urna metus interdum lacus, rhoncus elementum lectus ante eu enim. Donec arcu est, cursus rhoncus lorem vel, condimentum accumsan nulla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Sed mi odio, ullamcorper fermentum laoreet sit amet, dignissim in lacus. Fusce bibendum nulla commodo erat efficitur, ac facilisis felis efficitur. Duis augue purus, tempor ac metus non, feugiat congue turpis. Duis rutrum neque dolor, id scelerisque sem vehicula a. Proin sed neque id elit tempus sodales. Nam eleifend nisl risus, id rutrum eros dignissim eu.

           Sed ultrices eros tortor, sed lacinia ante porta eget. Nunc sem erat, pulvinar nec sodales vel, sagittis quis ipsum. Proin dolor felis, iaculis id posuere sed, interdum vel augue. Donec vitae ex quis lacus interdum finibus id eu ante. Etiam libero nunc, maximus eu elit sit amet, mollis maximus enim. Aenean vitae accumsan dui, quis congue lacus. Quisque rutrum facilisis tempor. Ut maximus varius viverra. Nam nibh odio, euismod a rutrum vel, euismod fringilla turpis.

           Nullam lacinia porta nisi tincidunt pellentesque. Mauris pharetra sapien sed gravida cursus. Nam lacinia lectus eget mi imperdiet placerat. Donec laoreet porttitor turpis. Nullam eget volutpat massa. Nullam quis mi quis felis vulputate lobortis in auctor turpis. Pellentesque pharetra felis sed fermentum fringilla. Integer tempor euismod quam.

           Donec ac elit mattis, tristique nibh a, pharetra neque. Fusce eu magna vitae ligula porta tempus et sed quam. Integer aliquam libero nec eleifend iaculis. Fusce elementum velit vitae sollicitudin bibendum. Vestibulum congue enim vel auctor convallis. Aliquam erat volutpat. Aliquam tristique lacinia eros ac hendrerit. Donec aliquet gravida augue eu vestibulum. Nullam aliquet egestas libero. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Ut tincidunt elit ut eros molestie, sed porta risus porta. Suspendisse mattis fringilla quam eget faucibus. Integer at elementum nisl, a placerat nisi. Nam bibendum ex vitae mi eleifend maximus
        ";

        let chunks = chunker.chunk(text);

        assert_eq!(chunks.len(), 33);
        assert_eq!(
            chunks[0],
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        );
    }
}
