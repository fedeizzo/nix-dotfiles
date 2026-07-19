export interface ChatRequest {
    message: string;
    conversation_id?: string;
}

export interface ChatResponse {
    response: string;
    conversation_id: string;
}

// TODO(human): Add strict types for User, Account, Category, Tag, and Transaction if you plan to fetch them directly via REST later.
