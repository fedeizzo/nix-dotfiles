#[derive(Debug, Clone)]
pub struct Chapter {
    pub title: String,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct Book {
    pub title: String,
    pub chapters: Vec<Chapter>,
    pub cover_image: Option<(Vec<u8>, String)>,
}
