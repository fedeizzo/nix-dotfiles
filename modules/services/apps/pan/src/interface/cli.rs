use crate::domain::chat::ChatProvider;
use anyhow::Result;
use std::io::{self, Write};

pub async fn run_chat_loop(provider: &impl ChatProvider) -> Result<()> {
    println!("Type 'exit' or 'quit' to leave the chat.");
    let mut input = String::new();

    loop {
        print!("> ");
        io::stdout().flush()?; // Ensure the prompt prints before waiting for input

        input.clear();
        io::stdin().read_line(&mut input)?;

        let trimmed = input.trim();
        if trimmed == "exit" || trimmed == "quit" {
            break;
        }
        if trimmed.is_empty() {
            continue;
        }

        // Pass the input to your awesome new memory-backed provider!
        let response = provider.prompt("cli-session-1", trimmed).await?;
        println!("🤖: {response}\n");
    }

    Ok(())
}
