;;; jai-mode.el --- Major mode for JAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023  Kristoffer Grönlund
;; Copyright (C) 2023-2026  Valentin Ignatev

;; Initial author: Kristoffer Grönlund <k@ziran.se>
;; Current maintainer: Valentin Ignatev <mail@valigo.gg>
;; URL: https://github.com/valignatev/jai-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for JAI
;;

;;; Code:

(require 'rx)
(require 'js)
(require 'compile)

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table))


;; This is needed for correct directive indentation.
;; This jai-mode uses js-mode indentation, and it defines a bunch of C++-compatible crap
;; for whatever reason. You could ask "what does javascript mode have to do with C++ preprocessor?",
;; or "why does jai-mode use javascript indentation?".
;; And my answer to that is - senator, I don't know, I just work here.
;; This hack essentially turns those into "impossible" regexes
;; (a word boundary at the beginning (`\\_<`) immediately followed by a word boundary at the end (`\\_>`))
;; bypassing that codepath in js-mode entirely.
(defconst cpp-font-lock-keywords-source-directives "\\_<\\_>")
(defconst js--opt-cpp-start "\\_<\\_>")
(defconst js--macro-decl-re "\\_<\\_>")

(defconst jai-builtins
  '("it" "it_index"))

(defconst jai-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "code_of" "initializer_of" "size_of" "type_of" "cast"  "type_info"
    "null" "true" "false" "xx" "context" "operator" "push_context" "is_constant"
    "enum_flags" "union" "interface"))

(defconst jai-typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "bool" "void"))

(defun jai-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun jai-keywords-rx (keywords)
  "build keyword regexp"
  (jai-wrap-word-rx (regexp-opt keywords t)))

(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))
(defconst jai-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defun jai-syntax-propertize-function (start end)
  "Mark all heredoc regions as strings in the buffer."
  (goto-char start)
  ;; If we're already inside a herestring we have to take care of that one first
  (when-let* ((ppss (syntax-ppss))
         (inside (eq t (nth 3 ppss)))
         (start-pos (nth 8 ppss))
         (tag (get-text-property start-pos 'here-string-marker)))
    (when (re-search-forward (concat "^[[:space:]]*" (regexp-quote tag) ";?$") end 'move)
      (let ((end (match-end 0)))
        (put-text-property (1- end) end 'syntax-table (string-to-syntax "|"))
        )
      ))
  (while (re-search-forward "#string +\\([a-zA-Z_][a-zA-Z0-9_]+\\)" end 'move)
    (unless (nth 4 (syntax-ppss))
      (let ((tag (match-string 1))
          (beg (match-beginning 1)))
      (unless (string= tag "CODE")
        (put-text-property beg (1+ beg) 'here-string-marker tag)
        (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "|"))
        (when (re-search-forward (concat "^[[:space:]]*" "\\(" (regexp-quote tag) "\\)" "\\([[:space:]]*[[:punct:]]*;?$\\)") end 'move)
          ;; Apply string syntax to everything between the start and end of heredoc
          (let ((end (match-end 1)))
            (put-text-property (1- end) end 'syntax-table (string-to-syntax "|"))
            ))))

      )
    )
  )

(defun jai-postfix-cast-syntax (limit)
  "Font-lock matcher for cast syntax: foo.(Type).
Matches the .( and the closing ) with balanced parens in between,
and highlights the type name inside."
  (let ((found nil))
    (while (and (not found) (re-search-forward "\\.(" limit t))
      (let ((open-paren-pos (1- (point)))
            (cast-start (match-beginning 0)))
        (save-excursion
          (goto-char open-paren-pos)
          ;; Use forward-sexp to find the matching closing paren
          (condition-case nil
              (progn
                (forward-sexp 1)
                (let ((close-paren-pos (point))
                      (inner-start (1+ open-paren-pos))
                      (inner-end (1- (point)))
                      (type-start nil)
                      (type-end nil))
                  ;; Look for the last word in the cast expression
                  (goto-char inner-start)
                  (while (re-search-forward "\\([[:word:]]+\\)" inner-end t)
                    (setq type-start (match-beginning 1))
                    (setq type-end (match-end 1)))
                  ;; Store the positions for font-lock
                  (set-match-data (list cast-start close-paren-pos
                                        cast-start (+ cast-start 2)  ; subgroup 1: .(
                                        (1- close-paren-pos) close-paren-pos  ; subgroup 2: )
                                        type-start type-end))  ; subgroup 3: Type (or nil if no word found)
                  (setq found t)))
            (error nil)))))  ; If forward-sexp fails, continue searching
    found))

(defconst jai-font-lock-defaults
  `(;; .() Cast syntax - put this FIRST so it has priority
    ;; Matching the type inside of .() is hacky.
    ;; It breaks apart with things like foo.(get_type(baz=bark))
    (jai-postfix-cast-syntax
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-type-face))

    ;; Keywords
    (,(jai-keywords-rx jai-keywords) 1 font-lock-keyword-face)

    ;; single quote characters
    ("\\('[[:word:]]\\)\\>" 1 font-lock-constant-face)

    ;; Variables
    (,(jai-keywords-rx jai-builtins) 1 font-lock-variable-name-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; At notes
    ("@\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(jai-wrap-word-rx jai-number-rx) . font-lock-constant-face)

    ;; Procedure names
    ("\\([[:word:]]+\\)[[:space:]]*:[[:space:]]*:?[[:space:]]*\\(inline\\|#type\\)?[[:space:]]*\(" 1 font-lock-function-name-face)

    ;; Types

    (,(jai-keywords-rx jai-typenames) 1 font-lock-type-face)

    (,jai-dollar-type-rx 1 font-lock-type-face)

    ;; Foo.{} and bar.[] matching
    ("\\([[:word:]]+\\)\\.\\(\{\\|\\[\\)" 1 font-lock-type-face)

    ("\\([[:word:]]+\\)[[:space:]]*:[[:space:]]*:[[:space:]]*\\(struct\\|enum\\|union\\|#type,\\)" 1 font-lock-type-face)
    ;; TODO: This detects false-positives in case of `for it_index, it: foo`, it thinks that foo is a type.
    ;; Emacs regexes do not support negative lookaheads, so I'd need to add proper logic to jai-syntax-propertize-function
    ;; but it's too hard for now. Oh well!
    ("[[:word:]]+[[:space:]]*:[[:space:]]*\\**\\(\\[[[:word:]]*\\]\\|\\[\\.\\.\\]\\)?*[[:space:]]*\\**[[:space:]]*\\([[:word:]]+\\)" 2 font-lock-type-face)

    ("---" . font-lock-constant-face)
    ))

;; add setq-local for older emacs versions
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun ()
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (when (jai-line-is-defun)
    (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'jai-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;; imenu hookup
(add-hook 'jai-mode-hook
          (lambda ()
            (setq imenu-generic-expression
                  '(("type" "^\\(.*:*.*\\) : " 1)
                    ("function" "^\\(.*\\) :: " 1)
                    ("struct" "^\\(.*\\) *:: *\\(struct\\)\\(.*\\){" 1)))))

;; NOTE: taken from the scala-indent package and modified for Jai.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun jai--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun jai--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'jai--indent-on-parentheses))

;;;###autoload
(define-derived-mode jai-mode jai-parent-mode "Jai"
  :syntax-table jai-mode-syntax-table
  :group 'jai
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-start "//")
  (setq-local block-comment-start "/*")
  (setq-local block-comment-end "*/")
  (setq-local indent-line-function 'js-indent-line)
  (setq-local font-lock-defaults '(jai-font-lock-defaults))
  (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
  (setq-local end-of-defun-function 'jai-end-of-defun)
  (setq-local syntax-propertize-function 'jai-syntax-propertize-function)
  ;; add indent functionality to some characters
  (jai--add-self-insert-hooks)

  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(defconst jai--error-regexp
  "\\([^ \n:]+.*\.jai\\):\\([0-9]+\\),\\([0-9]+\\):")
(push `(jai ,jai--error-regexp 1 2 3 2) compilation-error-regexp-alist-alist)
(push 'jai compilation-error-regexp-alist)

(provide 'jai-mode)
;;; jai-mode.el ends here
