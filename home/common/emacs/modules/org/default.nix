{ epkgs }:

{
  packages = with epkgs; [
    org
    org-contrib
    org-roam
    org-roam-ui
    org-download
    org-cliplink
    org-super-agenda
    org-ref
    org-remark
    async
    ox-epub
    ox-hugo
    org-modern
    org-fragtog
    org-noter
    zetteldesk
  ];
}
