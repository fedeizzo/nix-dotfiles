{ lib, ... }:

{
  services.n8n = {
    enable = true;

    environment = {
      GENERIC_TIMEZONE = "Europe/Paris";

      N8N_PORT = "22553";

      N8N_DIAGNOSTICS_ENABLED = "false";
      N8N_VERSION_NOTIFICATIONS_ENABLED = "false";
      N8N_HIRING_BANNER_ENABLED = "false";
      N8N_PUBLIC_API_SWAGGERUI_DISABLED = "true";

      # N8N_AI_ASSISTANT_BASE_URL = "https://ai-assistant.n8n.io";
    };
  };
  services.redis.servers.n8n = {
    enable = true;
    port = 52232;
  };
  # for consistent backup
  systemd.services.n8n.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "n8n";
    Group = "n8n";
  };
  nixpkgs.overlays = [
    (self: super: {
      n8n = super.n8n.overrideAttrs (oldAttrs: {
        postPatch = (oldAttrs.postPatch or "") + ''
          TARGET_FILE="packages/@n8n/nodes-langchain/nodes/llms/LMChatOpenAi/LmChatOpenAi.node.ts"
          if [ -f "$TARGET_FILE" ]; then
            echo "Patching LmChatOpenAi.node.ts..."
            substituteInPlace "$TARGET_FILE" \
              --replace "callbacks: [new N8nLlmTracing(this)]," "callbacks: [],"
          fi
        '';
      });
    })
  ];
}
