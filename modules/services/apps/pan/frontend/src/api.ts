import type { ChatRequest, ChatResponse } from "./types";

/**
 * Sends a message to the Rust Axum API and retrieves the AI's response.
 */
export async function sendChatMessage(request: ChatRequest): Promise<ChatResponse> {
    const res = await fetch("/api/chat", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(request),
    });

    if (!res.ok) {
        throw new Error(`HTTP error! status: ${res.status}`);
    }

    return (await res.json()) as ChatResponse;
}
