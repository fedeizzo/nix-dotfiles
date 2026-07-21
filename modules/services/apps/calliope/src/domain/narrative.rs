use serde::{Deserialize, Serialize};

/// Represents an emotion tag given to a line.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Emotion {
    Neutral,
    Happy,
    Sad,
    Angry,
    Fearful,
    Surprised,
    Disgusted,
    Whispering,
    Shouting,
    Inline,
}

/// A value object representing a specific speaker's identity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SpeakerId(pub String);

impl From<&str> for SpeakerId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

/// A core domain entity: a single segment of speech annotated with narrative context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnnotatedLine {
    /// The actual text to be spoken
    pub text: String,
    
    /// The identified speaker
    pub speaker: SpeakerId,
    
    /// The emotion or tone to apply to the TTS
    pub emotion: Emotion,
}
