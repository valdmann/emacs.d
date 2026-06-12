;;; -*- lexical-binding: t -*-

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defvar jinja-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    table)
  "Syntax table for `jinja-ts-mode'.")

(defvar jinja-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Comments: {# ... #}
   :language 'jinja
   :feature 'comment
   '((comment) @font-lock-comment-face @spell)

   ;; Strings
   :language 'jinja
   :feature 'string
   '((string_literal) @font-lock-string-face)

   ;; Numbers
   :language 'jinja
   :feature 'number
   '([(number_literal) (float_literal)] @font-lock-number-face)

   ;; Booleans
   :language 'jinja
   :feature 'boolean
   '((boolean_literal) @font-lock-boolean-face)

   ;; Null / None
   :language 'jinja
   :feature 'constant
   '((null_literal) @font-lock-constant-face)

   ;; Operators
   :language 'jinja
   :feature 'operator
   '((binary_operator) @font-lock-operator-face)

   ;; Built-in tests (is even, is defined, is string, ...)
   :language 'jinja
   :feature 'test
   '((builtin_test) @font-lock-type-face)

   ;; Directive delimiters: {{ }}, {% %}
   :language 'jinja
   :feature 'directive
   '([
      "{{" "{{-" "{{+" "+}}" "-}}" "}}"
      "{%" "{%-" "{%+" "+%}" "-%}" "%}"
      ] @font-lock-preprocessor-face)

   ;; Keywords inside control blocks
   :language 'jinja
   :feature 'keyword
   '([
      "for" "in" "endfor" "continue" "break"
      "if" "else" "elif" "endif"
      "block" "endblock"
      "macro" "endmacro"
      "filter" "endfilter"
      "with" "endwith"
      "set" "endset"
      "trans" "endtrans"
      "pluralize"
      "autoescape" "endautoescape"
      "call" "endcall"
      "required"
      "include" "import" "from" "extends" "as"
      "do"
      ] @font-lock-keyword-face)

   ;; Attribute modifiers: ignore missing, with/without context, recursive
   :language 'jinja
   :feature 'attribute
   '([
      (attribute_ignore) (attribute_context)
      "recursive"
      ] @font-lock-type-face)

   ;; Function calls
   :language 'jinja
   :feature 'function
   '((function_call (identifier) @font-lock-function-name-face))

   ;; Function parameters
   :language 'jinja
   :feature 'parameter
   '((arg (identifier) @font-lock-function-argument-face))

   ;; Variables / identifiers
   :language 'jinja
   :feature 'variable
   '((identifier) @font-lock-variable-name-face)

   ;; Raw blocks
   :language 'jinja
   :feature 'raw
   '((raw_body) @font-lock-string-face @nospell)

   ;; Brackets
   :language 'jinja
   :feature 'bracket
   '(["(" ")" "[" "]"] @font-lock-bracket-face)

   ;; Delimiters
   :language 'jinja
   :feature 'delimiter
   '(["," "." ":" "|"] @font-lock-delimiter-face)

   ;; Content (plain text outside jinja tags)
   :language 'jinja
   :feature 'content
   :override t
   '((content) @spell))
  "Tree-sitter font-lock settings for `jinja-ts-mode'.")

(defvar jinja-ts-mode--font-lock-feature-list
  '((comment)
    (string keyword variable function)
    (number boolean constant operator test directive attribute parameter
     raw bracket delimiter content))
  "Tree-sitter font-lock feature list for `jinja-ts-mode'.")

;;;###autoload
(define-derived-mode jinja-ts-mode prog-mode "Jinja2"
  "Major mode for editing Jinja2 templates, powered by tree-sitter."
  :group 'jinja

  (when (treesit-ready-p 'jinja)
    (setq treesit-primary-parser (treesit-parser-create 'jinja))

    ;; Comments: {# ... #}
    (setq-local comment-start "{# "
                comment-end " #}")

    ;; Font-lock.
    (setq-local treesit-font-lock-settings jinja-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list jinja-ts-mode--font-lock-feature-list)

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'jinja)
    (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . jinja-ts-mode))
  (add-to-list 'magic-mode-alist '("<!DOCTYPE html>" . jinja-ts-mode)))

(provide 'jinja-ts-mode)

;;; jinja-ts-mode.el ends here
