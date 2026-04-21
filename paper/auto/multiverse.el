;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "multiverse"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("sn-jnl" "pdflatex" "sn-mathphys-num")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("graphicx" "") ("multirow" "") ("amsmath" "") ("amssymb" "") ("amsfonts" "") ("amsthm" "") ("mathrsfs" "") ("appendix" "title") ("xcolor" "") ("textcomp" "") ("manyfoot" "") ("booktabs" "") ("algorithm" "") ("algorithmicx" "") ("algpseudocode" "") ("listings" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "sn-jnl"
    "sn-jnl10"
    "graphicx"
    "multirow"
    "amsmath"
    "amssymb"
    "amsfonts"
    "amsthm"
    "mathrsfs"
    "appendix"
    "xcolor"
    "textcomp"
    "manyfoot"
    "booktabs"
    "algorithm"
    "algorithmicx"
    "algpseudocode"
    "listings")
   (TeX-add-symbols
    '("paul" 1)
    '("david" 1))
   (LaTeX-add-labels
    "sec:intro"
    "sec:framework"
    "eq:outcome_equation"
    "sec:evalues"
    "def:evariable"
    "ssec:universal_evariables"
    "eq:univ_evalue_mixture"
    "eq:uip"
    "ssec:fdr_control"
    "sec:results"
    "sec:discussion"
    "secA1")
   (LaTeX-add-bibliographies
    "references")
   (LaTeX-add-amsthm-newtheorems
    "theorem"
    "proposition"
    "example"
    "remark"
    "definition"))
 :latex)

