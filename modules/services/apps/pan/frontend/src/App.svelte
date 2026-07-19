<script lang="ts">
  import { sendChatMessage } from "./api";
  import type { ChatResponse } from "./types";
  import { marked } from "marked";
  import DOMPurify from "dompurify";

  function renderMarkdown(text: string): string {
      const parsed = marked.parse(text);
      return DOMPurify.sanitize(parsed as string);
  }

  type Message = { role: "user" | "ai", text: string };
  type Conversation = { id: string, title: string, messages: Message[] };

  let conversations: Conversation[] = [];
  let activeConversationId: string | null = null;
  
  let inputMessage = "";
  let isLoading = false;

  $: activeConversation = conversations.find(c => c.id === activeConversationId);
  $: currentMessages = activeConversation?.messages || [];

  function selectConversation(id: string) {
      activeConversationId = id;
  }

  function startNewChat() {
      activeConversationId = null;
      inputMessage = "";
  }

  async function handleSend() {
    if (!inputMessage.trim()) return;

    const currentInput = inputMessage;
    inputMessage = "";
    isLoading = true;

    // Optimistically update UI
    if (!activeConversation) {
        // First message of a new conversation
        const tempId = "temp-" + Date.now();
        conversations = [{
            id: tempId,
            title: currentInput.slice(0, 30) + (currentInput.length > 30 ? "..." : ""),
            messages: [{ role: "user", text: currentInput }]
        }, ...conversations];
        activeConversationId = tempId;
    } else {
        // Append to existing conversation
        conversations = conversations.map(c => 
            c.id === activeConversationId 
                ? { ...c, messages: [...c.messages, { role: "user", text: currentInput }] }
                : c
        );
    }

    try {
        const payload = activeConversationId && !activeConversationId.startsWith("temp-")
            ? { message: currentInput, conversation_id: activeConversationId }
            : { message: currentInput };

        const response = await sendChatMessage(payload);

        // Update with true conversation_id from backend
        conversations = conversations.map(c => 
            c.id === activeConversationId 
                ? { ...c, id: response.conversation_id, messages: [...c.messages, { role: "ai", text: response.response }] }
                : c
        );
        activeConversationId = response.conversation_id;
    } catch (e) {
        conversations = conversations.map(c => 
            c.id === activeConversationId 
                ? { ...c, messages: [...c.messages, { role: "ai", text: `Error: ${e}` }] }
                : c
        );
    } finally {
        isLoading = false;
    }
  }
</script>

<main class="app-layout">
  <aside class="sidebar glass-panel">
    <div class="sidebar-header">
      <h2>Pan AI</h2>
      <button class="new-chat-btn" on:click={startNewChat}>+ New Chat</button>
    </div>
    <div class="chat-history">
      <div class="history-label">RECENT CHATS</div>
      {#each conversations as chat}
        <button 
          class="history-item {chat.id === activeConversationId ? 'selected' : ''}"
          on:click={() => selectConversation(chat.id)}
        >
          {chat.title}
        </button>
      {/each}
      {#if conversations.length === 0}
        <div class="empty-history">No recent chats</div>
      {/if}
    </div>
  </aside>

  <section class="main-content chat-container">
    <div class="glass-panel">
      <div class="chat-header">
        <h1>{activeConversation ? activeConversation.title : 'New Conversation'}</h1>
      </div>
      
      <div class="message-list">
        {#each currentMessages as msg}
          <div class="message-wrapper {msg.role}">
             <div class="bubble {msg.role === 'ai' ? 'markdown-body' : ''}">
               {#if msg.role === 'ai'}
                 {@html renderMarkdown(msg.text)}
               {:else}
                 {msg.text}
               {/if}
             </div>
          </div>
        {/each}
        {#if isLoading}
          <div class="message-wrapper ai">
             <div class="bubble loading">Thinking...</div>
          </div>
        {/if}
        {#if currentMessages.length === 0}
            <div class="empty-state">
                <p>Welcome to Pan. How can I assist with your finances today?</p>
            </div>
        {/if}
      </div>

      <form on:submit|preventDefault={handleSend} class="input-area">
        <input 
          type="text" 
          bind:value={inputMessage} 
          placeholder="Ask Pan about your finances..." 
          disabled={isLoading}
        />
        <button type="submit" disabled={isLoading || !inputMessage.trim()}>
          Send
        </button>
      </form>
    </div>
  </section>
</main>
