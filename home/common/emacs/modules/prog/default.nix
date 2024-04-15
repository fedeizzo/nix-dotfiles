{ pkgs, epkgs }:

{
  packages = with epkgs; [
    go-mode
    gotest # run tests inside emacs
    go-gen-test # generates unit tests with gotests
    pkgs.gotests # generate unit tests
    gorepl-mode # repl mode for golang
    go-tag # tags for structs
    go-impl
    flycheck-golangci-lint
    nix-mode
    python-pytest
    pyvenv
    markdown-toc
    rustic
    yaml-mode
    magit
    magit-delta
    git-gutter-fringe
    lsp-mode
    lsp-ui # ui tools
    lsp-treemacs # integration with treesitter
    lsp-origami # integration with origami for folding
    consult-lsp # integration with consult
    dap-mode # debugger
    sidecar-locals # per project variables
    typescript-mode
    # tree-sitter
    # tree-sitter-langs
    treesit-auto
    auctex
    tempel
    tempel-collection
    projectile
    smartparens
    terraform-mode
    docker
    bazel
    protobuf-mode
    lsp-java
    dockerfile-mode
  ];
}
