# Herdr-app mods

The Home Manager activation copies pinned mods into real folders so Herdr can
hot-reload them. `main-jj` uses the jj Changes panel and a patched spaces sidebar.
Select it with `herdr-app do layout.switch '{"name":"main-jj"}'`.
Its Files button opens `filetree`, with Material Icon Theme assets from
`file-icons`. The standalone `file-icons` panel is also installed. Filetree's
optional Claude activity hooks are not installed; Tree and previews work without
them, but Recent/activity highlighting requires configuring those hooks.
Open With uses **Ctrl+Alt+O**, preserving Ctrl+Shift+O for splitting panes.

## Updates

From the repository root:

```sh
nix flake update herdr-app-jj herdr-kanso herdr-mods
```

Then apply the configuration with your usual NixOS/Home Manager rebuild.
Activation copies changed files; there is no separate mod update command.
For just one repository, pass only its input name. Review `jj diff flake.lock`
before applying. The Kanso input supplies only `open-with`, not the whole layout.
The `herdr-mods` input updates Filetree and File Icons together.

`spaces-jj` and `main-jj.json` are local clones of Herdr-app 0.1.13, with the
sidebar changes from herdr-app-jj revision
`48be7c658c2f0a47e1652046bf93542a26ade135`. They do not update automatically:
review the upstream sidebar patch when updating jj, and refresh the clones when
changing Herdr's layout/sidebar behavior. The other jj mods update from the input.

The installer owns these mod source files and `layouts/main-jj.json`; edit their
repository copies, since activation reapplies them. Keys are merged; app state
and generated type files are preserved. Removing a mod from the installer does
not delete a previously copied folder; remove that folder separately.

Validate updates with `herdr-app check` for `jj-changes`, `jj-menu`,
`jj-workspace`, `spaces-jj`, `open-with`, `filetree`, and `file-icons`, then
inspect `herdr-app status`.
