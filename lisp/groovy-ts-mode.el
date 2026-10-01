;;; groovy-ts-mode.el --- tree-sitter support for Groovy  -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)

(defcustom groovy-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `groovy-ts-mode'."
  :type 'integer
  :group 'groovy)

(defvar groovy-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?^  "."  table)
    (modify-syntax-entry ?!  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?~  "."  table)
    (modify-syntax-entry ?@  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table))

(defvar groovy-ts-mode--indent-rules
  `((groovy
     ((parent-is "source_file") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "closure") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "parameter_list") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "argument_list") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "list") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "map") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "switch_block") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "case") parent-bol groovy-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol groovy-ts-mode-indent-offset)
     (no-node parent-bol groovy-ts-mode-indent-offset))))

(defvar groovy-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'groovy
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'groovy
   :feature 'keyword
   '(["class" "extends"
      "import" "package"
      "def"
      "if" "else"
      "for" "in" "while" "do"
      "switch" "case" "default"
      "try" "catch" "finally"
      "return" "assert"
      "new" "instanceof" "as"] @font-lock-keyword-face
     (modifier) @font-lock-keyword-face
     (access_modifier) @font-lock-keyword-face
     (break) @font-lock-keyword-face
     (continue) @font-lock-keyword-face
     ;; enum, trait, throw, var are parsed as identifiers by this grammar
     ((identifier) @font-lock-keyword-face
      (:match "\\`\\(?:enum\\|trait\\|throw\\|var\\|implements\\)\\'" @font-lock-keyword-face)))

   :language 'groovy
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'groovy
   :feature 'interpolation
   :override t
   '((interpolation) @font-lock-variable-use-face)

   :language 'groovy
   :feature 'type
   '((builtintype) @font-lock-type-face
     (class_definition name: (identifier) @font-lock-type-face)
     (class_definition superclass: (identifier) @font-lock-type-face)
     (declaration type: (identifier) @font-lock-type-face)
     (function_definition type: (identifier) @font-lock-type-face)
     (function_declaration type: (identifier) @font-lock-type-face)
     (parameter type: (identifier) @font-lock-type-face)
     (parameter type: (array_type (identifier) @font-lock-type-face))
     (try_statement catch_exception: (declaration type: (identifier) @font-lock-type-face)))

   :language 'groovy
   :feature 'function
   '((function_definition function: (identifier) @font-lock-function-name-face)
     (function_declaration function: (identifier) @font-lock-function-name-face)
     (function_call function: (identifier) @font-lock-function-call-face)
     (function_call function: (dotted_identifier (identifier) @font-lock-function-call-face :anchor))
     (juxt_function_call function: (identifier) @font-lock-function-call-face)
     (juxt_function_call function: (dotted_identifier (identifier) @font-lock-function-call-face :anchor)))

   :language 'groovy
   :feature 'constant
   '((boolean_literal) @font-lock-constant-face
     (null) @font-lock-constant-face)

   :language 'groovy
   :feature 'number
   '((number_literal) @font-lock-number-face)

   :language 'groovy
   :feature 'annotation
   '((annotation (identifier) @font-lock-constant-face)
     (annotation "@" @font-lock-constant-face))

   :language 'groovy
   :feature 'variable
   '((declaration name: (identifier) @font-lock-variable-name-face)
     (parameter name: (identifier) @font-lock-variable-name-face)
     (assignment (dotted_identifier) @font-lock-variable-use-face)
     (for_in_loop variable: (identifier) @font-lock-variable-name-face))

   :language 'groovy
   :feature 'property
   '((map_item key: (identifier) @font-lock-property-use-face))

   :language 'groovy
   :feature 'package
   '((groovy_package (qualified_name) @font-lock-constant-face)
     (groovy_import (qualified_name) @font-lock-constant-face)
     (wildcard_import) @font-lock-constant-face)

   :language 'groovy
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'groovy
   :feature 'delimiter
   '((["." "," ";" ":"]) @font-lock-delimiter-face)

   :language 'groovy
   :feature 'operator
   '((["=" "+" "-" "*" "/" "%" "!" ">" "<" ">=" "<=" "==" "!="
      "&&" "||" "&" "|" "^" "~" "<<" ">>" ">>>"
      "+=" "-=" "*=" "/=" "%="
      "?:" "?." "*." ".." "..<"
      "=~" "==~" "<=>"
      "++" "--"
      "?" "**"]) @font-lock-operator-face)))

;;;###autoload
(define-derived-mode groovy-ts-mode prog-mode "Groovy"
  "Major mode for editing Groovy, powered by tree-sitter."
  :group 'groovy
  :syntax-table groovy-ts-mode--syntax-table

  (when (treesit-ready-p 'groovy)
    (treesit-parser-create 'groovy)

    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")

    (setq-local treesit-font-lock-settings groovy-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword type function constant)
                  (annotation number variable property package interpolation)
                  (bracket delimiter operator)))

    (setq-local treesit-simple-indent-rules groovy-ts-mode--indent-rules)

    (setq-local treesit-simple-imenu-settings
                '(("Class" "\\`class_definition\\'" nil nil)
                  ("Function" "\\`function_definition\\'" nil nil)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'groovy t)
    (progn
      (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.gant\\'" . groovy-ts-mode))
      (add-to-list 'auto-mode-alist '("/Jenkinsfile\\'" . groovy-ts-mode))))

(provide 'groovy-ts-mode)

;;; groovy-ts-mode.el ends here
