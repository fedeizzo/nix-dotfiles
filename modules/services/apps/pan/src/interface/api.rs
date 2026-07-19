use axum::{
    routing::post,
    Router, Json, extract::State,
};
use std::sync::Arc;
use serde::{Deserialize, Serialize};
use tower_http::services::{ServeDir, ServeFile};

use crate::domain::chat::ChatProvider;

#[derive(Clone)]
pub struct AppState {
    pub rig: Arc<crate::infrastructure::ai::Rig>,
}

#[derive(Deserialize)]
pub struct ChatRequest {
    pub message: String,
    pub conversation_id: Option<String>,
}

#[derive(Serialize)]
pub struct ChatResponse {
    pub response: String,
    pub conversation_id: String,
}

pub fn create_router(state: AppState) -> Router {
    Router::new()
        .route("/api/chat", post(chat_handler))
        .fallback_service(ServeDir::new("frontend/dist").not_found_service(ServeFile::new("frontend/dist/index.html")))
        .with_state(state)
}

async fn chat_handler(
    State(state): State<AppState>,
    Json(payload): Json<ChatRequest>,
) -> Json<ChatResponse> {
    let conversation_id = payload.conversation_id.unwrap_or_else(|| uuid::Uuid::new_v4().to_string());

    match state.rig.prompt(&conversation_id, &payload.message).await {
        Ok(response) => Json(ChatResponse { response, conversation_id }),
        Err(e) => Json(ChatResponse { response: format!("Error: {}", e), conversation_id }),
    }
}
