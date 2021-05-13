{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    ueberzug
    ffmpegthumbnailer
    imagemagick
    poppler_utils
  ];
  programs.lf = {
    enable = true;
    settings = {
      dirfirst = true;
      drawbox = true;
      icons = true;
    };
    keybindings = {
      o = "$(dragon -a -x \"$f\")";
      gt = "cd /tmp";
      f = "betterFind";
      F = "fzf_jump";
      m = null;
      md = "mkdir";
    };
      # %{{
      #   w=$(tput cols)
      #   if [ $w -le 80 ]; then
      #       lf -remote "send $id set ratios 1:2"
      #   elif [ $w -le 160 ]; then
      #       lf -remote "send $id set ratios 1:2:3"
      #   else
      #       lf -remote "send $id set ratios 1:2:3"
      #   fi
      # }}
    extraConfig = ''
      # Change column amount
      set ratios 1:2:3
      # Change default open function
      cmd open ''${{
          case $(file --mime-type "$f" -bL) in
              text/*|applicattion/json) $EDITOR "$f";;
              video/*) devour vlc "$f";;
              application/pdf) devour zathura "$f";;
              image/*) devour sxiv "$f";;
              *) devour xdg-open "$f";;
          esac
      }}

      cmd on-cd &{{
          printf "\033]0; $(pwd | sed "s|$HOME|~|") - lf\007" > /dev/tty
      }}

      cmd mkdir ''${{
          printf "Directory name: "
          read dir
          mkdir $dir
      }}

      cmd betterFind ''${{
          res="$(find . -maxdepth 1 -type f | fzf --reverse --header='Open file')"
          case $(file --mime-type "$res" -bL) in
              text/*|applicattion/json) $EDITOR "$res";;
              video/*) devour vlc "$res";;
              application/pdf) devour zathura "$res";;
              image/*) devour sxiv "$res";;
              *) xdg-open "$res";;
          esac
      }}

      cmd fzf_jump ''${{
          res="$(find . -maxdepth 1 | fzf --reverse --header='Jump to location')"
          if [ -f "$res" ]; then
           cmd="select"
          elif [ -d "$res" ]; then
           cmd="cd"                                                       
          fi
          lf -remote "send $id $cmd \"$res\""
      }}
      on-cd
      set mouse
      set previewer ~/.sources/lfpreview
      set cleaner ~/.sources/lfcleaner
    '';
  };
}
