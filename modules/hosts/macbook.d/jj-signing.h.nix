# Work Mac only: sign jj commits on push with the ssh key git would use for
# the repository. The key choice (and allowed-signers file) stays in the
# user's own git config, which may pick a different key per repository
# (includeIf); jj has no equivalent, so `jj-sign-ssh` stands in for
# ssh-keygen and asks git at signing time. See jj-sign-ssh.sh.
{
  flake.modules.homeManager.macbook-jj-signing = { lib, pkgs, ... }:
    let
      jj-sign-ssh = pkgs.writeShellApplication {
        name = "jj-sign-ssh";
        runtimeInputs = [ pkgs.git pkgs.openssh ];
        text = builtins.readFile ./jj-sign-ssh.sh;
      };
    in
    {
      home.packages = [ jj-sign-ssh ];
      programs.jujutsu.settings = {
        signing = {
          # Don't sign every rewrite; sign mutable commits once, on push.
          behavior = "drop";
          backend = "ssh";
          # jj insists on both values; jj-sign-ssh replaces them per repository.
          key = "resolved-by-jj-sign-ssh";
          backends.ssh = {
            program = lib.getExe jj-sign-ssh;
            allowed-signers = "resolved-by-jj-sign-ssh";
          };
        };
        git.sign-on-push = true;
        # nix-dotfiles is personal: never sign it. Matches its secondary
        # workspaces too, since their repository path is the main one.
        "--scope" = [
          {
            "--when".repositories = [ "~/nix-dotfiles" "~/nix-dotfiles.worktrees" ];
            git.sign-on-push = false;
          }
        ];
      };
    };
}
