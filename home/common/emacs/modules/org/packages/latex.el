(use-package tex-site
  :config
  (setq TeX-parse-self t
        TeX-auto-save t))
(with-eval-after-load 'ox-latex
  (setq org-latex-classes nil)
  (add-to-list 'org-latex-classes
	       '("personal"
                 "\\documentclass[a4paper,11pt,notitlepage,margin=2.5cm]{article}
				\\usepackage[utf8]{inputenc}
				\\usepackage[T1]{fontenc}
				\\usepackage{textcomp}
				\\usepackage{url}
				\\usepackage{graphicx}
				\\usepackage{hyperref}
				\\usepackage{float}
				\\usepackage{parskip}
				\\usepackage{xcolor}
				\\usepackage{amsmath, amsfonts, mathtools, amsthm, amssymb}
				\\usepackage{chngcntr} % for figures, table, equation numbers that follow sections
				\\usepackage{enumitem}
				\\setlist[itemize]{noitemsep}
				\\usepackage{geometry}
				\\geometry{
				a4paper,
				total={170mm,257mm},
				left=20mm,
				top=20mm,
				}
				% for svg images from tex files
				\\usepackage{import}
				\\usepackage{xifthen}
				\\usepackage{pdfpages}
				\\usepackage{transparent}
				\\newcommand{\\incfig}[1]{%
				\\def\\svgwidth{\\columnwidth}
				\\import{.}{#1.pdf_tex}
				}
				% Polar Night
				\\definecolor{NordDarkBlack}{HTML}{2E3440}     % nord0
				\\definecolor{NordBlack}{HTML}{3B4252}         % nord1
				\\definecolor{NordMediumBlack}{HTML}{434C5e}   % nord2
				\\definecolor{NordBrightBlack}{HTML}{4C566A}   % nord3
				% Snow Storm
				\\definecolor{NordWhite}{HTML}{D8DEE9}         % nord4
				\\definecolor{NordBrighterWhite}{HTML}{E5E9F0}         % nord5
				\\definecolor{NordBrightestWhite}{HTML}{ECEFF4}   % nord6
				% Frost
				\\definecolor{NordCyan}{HTML}{8FBCBB}          % nord7
				\\definecolor{NordBrightCyan}{HTML}{88C0D0}    % nord8
				\\definecolor{NordBlue}{HTML}{81A1C1}          % nord9
				\\definecolor{NordBrightBlue}{HTML}{5E81AC}    % nord10
				% Aurora
				\\definecolor{NordRed}{HTML}{BF616A}           % nord11
				\\definecolor{NordOrange}{HTML}{D08770}        % nord12
				\\definecolor{NordYellow}{HTML}{EBCB8B}        % nord13
				\\definecolor{NordGreen}{HTML}{A3BE8C}         % nord14
				\\definecolor{NordMagenta}{HTML}{B48EAD}       % nord15
				\\hypersetup{
				colorlinks=true,
				linkcolor=NordBrightBlue,
				filecolor=NordBrightBlue,
				urlcolor=NordBrightBlue,
				citecolor=NordBrightBlue,
				}
				\\urlstyle{same}
				\\renewcommand\\contentsname{
				~\\hfill {\\LARGE Table of contents}\\\\
				\\rule{\\textwidth}{0.4pt}
				}
				\\counterwithin{table}{section}
				\\counterwithin{equation}{section}
				\\counterwithin{table}{section}
				"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       '("article"
		 "\\documentclass[11pt,a4paper]{article}
				\\usepackage[utf8]{inputenc}
				\\usepackage[T1]{fontenc}
				\\usepackage{fixltx2e}
				\\usepackage{graphicx}
				\\usepackage{longtable}
				\\usepackage{float}
				\\usepackage{wrapfig}
				\\usepackage{rotating}
				\\usepackage[normalem]{ulem}
				\\usepackage{amsmath}
				\\usepackage{textcomp}
				\\usepackage{marvosym}
				\\usepackage{wasysym}
				\\usepackage{amssymb}
				\\usepackage{hyperref}
				\\usepackage{mathpazo}
				\\usepackage{color}
				\\usepackage{enumerate}
				\\definecolor{bg}{rgb}{0.95,0.95,0.95}
				\\tolerance=1000
				[NO-DEFAULT-PACKAGES]
				[PACKAGES]
				[EXTRA]
				\\linespread{1.1}
				\\hypersetup{pdfborder=0 0 0}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (setq org-latex-default-class "personal"))

(setq org-format-latex-options '(
                                 :foreground default
                                 :background default
                                 :scale 1.0
                                 :html-foreground "Black"
                                 :html-background "Transparent"
                                 :html-scale 1.0
                                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-latex-title-command "
									    \\begin{titlepage}
									    \\raggedleft
									    \\vspace*{\\baselineskip}
									    {\\Large %a}\\\\
									    \\vspace*{0.167\\textheight}
									    \\textbf{\\LARGE Personal notes of}\\\\[\\baselineskip]
									    {{\\color{NordMediumBlack}{\\Huge %t}}\\\\[\\baselineskip]}
									    {\\Large \\textit{%s}}
									    \\vfill
									    {\\large $\\mathcal{FI}$}
									    \\vspace*{3\\baselineskip}
									    \\end{titlepage}
									    ")
(setq org-latex-toc-command "
									    {
									    \\hypersetup{linkcolor=black}
									    \\tableofcontents
									    \\clearpage
									    }
									    ")
(setq org-export-headline-levels 5)
(setq org-startup-with-latex-preview t)
(setq org-latex-pdf-process
      (let
          ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
                        " --synctex=1"
                        " -output-directory %o %f")))
        (list cmd
	      "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
	      "cd %o; bibtex %b"
	      cmd
	      cmd)))
