;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "reply"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("geometry" "") ("xcolor" "") ("amsmath" "") ("amssymb" "") ("hyperref" "") ("bm" "") ("enumitem" "") ("natbib" "numbers")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "geometry"
    "xcolor"
    "amsmath"
    "amssymb"
    "hyperref"
    "bm"
    "enumitem"
    "natbib")
   (TeX-add-symbols
    '("paul" 1)
    '("david" 1)
    "bgamma"
    "bGamma"
    "bpi"
    "btheta"
    "bomega"
    "R"
    "T"
    "bx"
    "by"
    "bz"
    "bA"
    "bD"
    "bP"
    "bU"
    "bV"
    "bX"
    "bZ")
   (LaTeX-add-bibliographies
    "../references"))
 :latex)

