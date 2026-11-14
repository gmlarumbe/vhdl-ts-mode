;;; vhdl-ts-mode.el --- VHDL Tree-sitter major mode -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ts-mode
;; Version: 0.4.0
;; Keywords: VHDL, IDE, Tools
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode to navigate and edit VHDL files with tree-sitter.

;; Provides tree-sitter based implementations for the following features:
;; - Syntax highlighting
;; - Indentation
;; - `imenu'
;; - `which-func'
;; - Navigation functions
;; - Completion at point

;; Contributions:
;;   This major mode is still under active development!
;;   Check contributing guidelines:
;;     - https://github.com/gmlarumbe/vhdl-ts-mode#contributing

;;; Code:

;;; Requirements
(require 'treesit)
(require 'imenu)
(require 'vhdl-mode)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")


;;; Customization
(defgroup vhdl-ts nil
  "VHDL tree-sitter mode."
  :group 'languages)

(defcustom vhdl-ts-indent-level 4
  "Tree-sitter indentation of VHDL statements with respect to containing block."
  :group 'vhdl-ts
  :type 'integer)

(defcustom vhdl-ts-file-extension-re "\\.vhdl?\\'"
  "VHDL file extensions.
Defaults to .vhd and .vhdl."
  :group 'vhdl-ts
  :type 'string)

(defcustom vhdl-ts-imenu-style 'tree
  "Style of generated Imenu index for current VHDL buffer.

- Simple: default basic grouping into categories.
- Tree: tree-like structure without further processing.
- Tree-group: tree-like structure with some processing to group same type
              elements into subcategories (useful for display with third party
              packages such as `imenu-list')."
  :type '(choice (const :tag "simple" simple)
                 (const :tag "tree" tree)
                 (const :tag "tree-group" tree-group))
  :group 'vhdl-ts)

(defcustom vhdl-ts-which-func-style 'custom
  "Style of `which-func' display for current VHDL buffer.

- Simple: show last element of current Imenu entry
- Breadcrumb: display hierarchy of current Imenu entry
- Custom: use custom `vhdl-ts-mode' implementation for `which-func':
    - Format A:B
    - A represents the type of current node, B its name
    - For instances, A is the instance name and B the instance type."
  :type '(choice (const :tag "simple" simple)
                 (const :tag "breadcrumb" breadcrumb)
                 (const :tag "custom" custom))
  :group 'vhdl-ts)

(defcustom vhdl-ts-beautify-align-ports-and-params nil
  "Align all ports and params of instances when beautifying."
  :type 'boolean
  :group 'vhdl-ts)

(defcustom vhdl-ts-linter-enable t
  "Non-nil means enable tree-sitter based linting."
  :type 'boolean
  :group 'vhdl-ts)


;;; Utils
;;;; Core
(defconst vhdl-ts-identifier-re
  (eval-when-compile
    (regexp-opt
     '("identifier"
       "label"
       "attribute_identifier"
       "library_attribute"
       "library_constant"
       "library_constant_boolean"
       "library_constant_character"
       "library_constant_debug"
       "library_constant_env"
       "library_constant_standard"
       "library_constant_unit"
       "library_function"
       "library_namespace"
       "library_type"
       "operator_symbol")
     'symbols)))
(defconst vhdl-ts-instance-re "\\_<component_instantiation_statement\\_>")
(defconst vhdl-ts-generate-re "\\_<\\(for\\|if\\|case\\)_generate_statement\\_>")

(defun vhdl-ts--node-at-point (&optional bound)
  "Return tree-sitter node at point.

If optional BOUND is non-nil, return nil if point is not over a symbol."
  (let* ((pos (point))
         (node (treesit-node-at pos 'vhdl)))
    (if bound
        (when (and (>= pos (treesit-node-start node))
                   (<= pos (treesit-node-end node)))
          node)
      node)))

(defun vhdl-ts--highest-node-at-pos (pos)
  "Return highest node starting at POS in the parsed tree.

Only might work as expected if point is at the beginning of a symbol.

Snippet fetched from `treesit--indent-1'."
  (let* ((smallest-node (vhdl-ts--node-at-point))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq pos (treesit-node-start node))))))
    node))

(defun vhdl-ts--highest-node-at-point (&optional bound)
  "Return highest node at point.

If optional BOUND is non-nil, return nil if point is not over a symbol."
  (vhdl-ts--highest-node-at-pos (treesit-node-start (vhdl-ts--node-at-point bound))))

(defun vhdl-ts--node-at-bol ()
  "Return node at first non-blank character of current line.
Snippet fetched from `treesit--indent-1'."
  (let* ((bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (smallest-node
          (cond ((null (treesit-parser-list)) nil)
                ((eq 1 (length (treesit-parser-list)))
                 (treesit-node-at bol))
                ((treesit-language-at (point))
                 (treesit-node-at bol (treesit-language-at (point))))
                (t (treesit-node-at bol))))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq bol (treesit-node-start node))))))
    node))

(defun vhdl-ts--node-has-parent-recursive (node node-type)
  "Return non-nil if NODE is part of NODE-TYPE in the parsed tree."
  (treesit-parent-until
   node
   (lambda (node) ; Third argument must be a function
     (string-match node-type (treesit-node-type node)))))

(defun vhdl-ts--node-has-child-recursive (node node-type)
  "Return first node of type NODE-TYPE that is a child of NODE in the parsed tree.
If none is found, return nil."
  (treesit-search-subtree node node-type))

(defun vhdl-ts--node-parents-list (node node-type)
  "Return NODE parents that match NODE-TYPE as a list of nodes."
  (let (parent parent-list)
    (while (setq parent (vhdl-ts--node-has-parent-recursive node node-type))
      (push parent parent-list)
      (setq node parent))
    (nreverse parent-list)))

(defun vhdl-ts--node-identifier-name (node)
  "Return identifier name of NODE."
  (when node
    (cond (;; Instances
           (string-match vhdl-ts-instance-re (treesit-node-type node))
           (let ((comp-node (treesit-node-child-by-field-name node "component"))
                 temp-node)
             (if comp-node
                 ;; Component instantiation
                 (progn
                   (setq temp-node (or (treesit-node-child comp-node -1) comp-node)) ; Try to get latest child in case there are library prefixes
                   (treesit-node-text (treesit-search-subtree temp-node "identifier") :no-prop))
               ;; Entity instantiation
               (setq temp-node (treesit-node-child-by-field-name (treesit-search-subtree node "instantiated_unit") "entity"))
               (setq temp-node (or (treesit-search-subtree temp-node "selection")
                                   (treesit-search-subtree temp-node "name")))
               (treesit-node-text (treesit-search-subtree temp-node "identifier") :no-prop))))
          ;; Generate blocks
          ((string-match vhdl-ts-generate-re (treesit-node-type node))
           (treesit-node-text (treesit-search-subtree node "\\_<label\\_>") :no-prop))
          ;; Procedures/functions
          ((string-match "procedure_specification" (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "procedure") :no-prop))
          ((string-match "function_specification" (treesit-node-type node))
           (treesit-node-text (treesit-node-child-by-field-name node "function") :no-prop))
          ((string-match "subprogram_definition" (treesit-node-type node))
           (vhdl-ts--node-identifier-name (treesit-search-subtree node "\\(function\\|procedure\\)_specification")))
          ;; Default
          (t
           (treesit-node-text (treesit-search-subtree node vhdl-ts-identifier-re) :no-prop)))))

(defun vhdl-ts--node-instance-name (node)
  "Return identifier name of NODE.

Node must be of type `vhdl-ts-instance-re'.  Otherwise return nil."
  (unless (and node
               (string-match vhdl-ts-instance-re (treesit-node-type node)))
    (error "Wrong node type: %s" (treesit-node-type node)))
  (treesit-node-text (treesit-search-subtree node "\\_<label\\_>") :no-props))


;;;; Context
(defconst vhdl-ts-block-at-point-re
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_definition"
       "package_declaration"
       "package_definition"
       "process_statement"
       "subprogram_definition"
       "procedure_specification"
       "function_specification"
       "for_generate_statement"
       "if_generate_statement"
       "case_generate_statement"
       "block_statement"
       "component_instantiation_statement")
     'symbols)))

(defun vhdl-ts-nodes (pred &optional start)
  "Return current buffer NODES that match PRED.

If optional arg START is non-nil, use it as the initial node to search in the
tree."
  (let ((root-node (or start (treesit-buffer-root-node))))
    (mapcar #'car (cdr (treesit-induce-sparse-tree root-node pred)))))

(defun vhdl-ts-nodes-props (pred &optional start)
  "Return nodes and properties that satisfy PRED in current buffer.

If optional arg START is non-nil, use it as the initial node to search in the
tree.

Returned properties are a property list that include node name, start position
and end position."
  (mapcar (lambda (node)
            `(,node :name ,(vhdl-ts--node-identifier-name node)
                    :start-pos ,(treesit-node-start node)
                    :end-pos ,(treesit-node-end node)))
          (vhdl-ts-nodes pred start)))

(defun vhdl-ts-entity-at-point ()
  "Return node of entity at point."
  (let ((entity (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) "entity_declaration"))
        (pos (point)))
    (when (and entity
               (>= pos (treesit-node-start entity))
               (<= pos (treesit-node-end entity)))
      entity)))

(defun vhdl-ts-arch-at-point ()
  "Return node of architecture at point."
  (let ((arch (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) "architecture_definition"))
        (pos (point)))
    (when (and arch
               (>= pos (treesit-node-start arch))
               (<= pos (treesit-node-end arch)))
      arch)))

(defun vhdl-ts-instance-at-point ()
  "Return node of instance at point."
  (let ((instance (vhdl-ts--node-has-parent-recursive (vhdl-ts--node-at-point) vhdl-ts-instance-re))
        (pos (point)))
    (when (and instance
               (>= pos (treesit-node-start instance))
               (<= pos (treesit-node-end instance)))
      instance)))

(defun vhdl-ts--block-at-point (regex)
  "Return deepest node of block at point that matches REGEX."
  (let ((blocks (vhdl-ts--node-parents-list (vhdl-ts--node-at-point) regex))
        (pos (point)))
    (catch 'block
      (mapc (lambda (block)
              (when (and block
                         (>= pos (treesit-node-start block))
                         (<= pos (treesit-node-end block)))
                (throw 'block block)))
            blocks)
      nil)))

(defun vhdl-ts-block-at-point ()
  "Return node of block at point."
  (vhdl-ts--block-at-point vhdl-ts-block-at-point-re))

(defun vhdl-ts-nodes-block-at-point (pred)
  "Return block at point NODES that match PRED."
  (mapcar #'car (cdr (treesit-induce-sparse-tree (vhdl-ts-block-at-point) pred))))

(defun vhdl-ts-search-node-block-at-point (pred &optional backward all)
  "Search forward for node matching PRED inside block at point.

By default, only search for named nodes, but if ALL is non-nil, search
for all nodes.  If BACKWARD is non-nil, search backwards."
  (treesit-search-forward (vhdl-ts--node-at-point)
                          (lambda (node)
                            (and (string-match pred (treesit-node-type node))
                                 (< (treesit-node-end node) (treesit-node-end (vhdl-ts-block-at-point)))))
                          backward
                          all))

;; Some examples using previous API
(defun vhdl-ts-entity-declarations-nodes-current-buffer ()
  "Return entity declaration nodes of current file."
  (vhdl-ts-nodes "entity_declaration"))

(defun vhdl-ts-entity-declarations-current-buffer ()
  "Return entity declaration names of current file."
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "entity_declaration")))

(defun vhdl-ts-arch-body-nodes-current-buffer ()
  "Return architecture body nodes of current file."
  (vhdl-ts-nodes "architecture_definition"))

(defun vhdl-ts-arch-body-current-buffer ()
  "Return architecture body names of current file."
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "architecture_definition")))

(defun vhdl-ts-arch-instances-nodes (arch-node)
  "Return instance nodes of ARCH-NODE."
  (unless (and arch-node (string= "architecture_definition" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (vhdl-ts-nodes vhdl-ts-instance-re arch-node))

(defun vhdl-ts-arch-instances (arch-node)
  "Return instances of ARCH-NODE."
  (unless (and arch-node (string= "architecture_definition" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props vhdl-ts-instance-re arch-node)))

(defun vhdl-ts-arch-process-blocks (arch-node)
  "Return process blocks of ARCH-NODE."
  (unless (and arch-node (string= "architecture_definition" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "process_statement" arch-node)))

(defun vhdl-ts-arch-concurrent-assignments (arch-node)
  "Return concurrent assignments of ARCH-NODE."
  (unless (and arch-node (string= "architecture_definition" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (mapcar (lambda (node-and-props)
            (plist-get (cdr node-and-props) :name))
          (vhdl-ts-nodes-props "concurrent_simple_signal_assignment" arch-node)))

(defun vhdl-ts-arch-entity-name (arch-node)
  "Return associated entity name of ARCH-NODE."
  (unless (and arch-node (string= "architecture_definition" (treesit-node-type arch-node)))
    (error "Wrong arch-node: %s" arch-node))
  (treesit-node-text (treesit-node-child-by-field-name arch-node "entity") :no-props))


;;; Font-lock
;;;; Faces
(defgroup vhdl-ts-faces nil
  "VHDL-ts faces."
  :group 'vhdl-ts)

(defvar vhdl-ts-font-lock-then-face 'vhdl-ts-font-lock-then-face)
(defface vhdl-ts-font-lock-then-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for if-else grouping keyword: then."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-punctuation-face 'vhdl-ts-font-lock-punctuation-face)
(defface vhdl-ts-font-lock-punctuation-face
  '((t (:inherit font-lock-punctuation-face)))
  "Face for punctuation symbols:
!,;:?'=<>*"
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-operator-face 'vhdl-ts-font-lock-operator-face)
(defface vhdl-ts-font-lock-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face for operator symbols, such as &^~+-/|."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-parenthesis-face 'vhdl-ts-font-lock-parenthesis-face)
(defface vhdl-ts-font-lock-parenthesis-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for parenthesis ()."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-brackets-content-face 'vhdl-ts-font-lock-brackets-content-face)
(defface vhdl-ts-font-lock-brackets-content-face
  '((t (:inherit font-lock-number-face)))
  "Face for content between brackets: arrays, bit vector width and indexing."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-port-connection-face 'vhdl-ts-font-lock-port-connection-face)
(defface vhdl-ts-font-lock-port-connection-face
  '((t (:inherit font-lock-constant-face)))
  "Face for port connections in instances.
portA => signalA,
portB => signalB
);"
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-entity-face 'vhdl-ts-font-lock-entity-face)
(defface vhdl-ts-font-lock-entity-face
  '((t (:inherit font-lock-function-call-face)))
  "Face for entity names."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-instance-face 'vhdl-ts-font-lock-instance-face)
(defface vhdl-ts-font-lock-instance-face
  '((t (:inherit font-lock-variable-use-face)))
  "Face for instance names."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-instance-lib-face 'vhdl-ts-font-lock-instance-lib-face)
(defface vhdl-ts-font-lock-instance-lib-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for instances library prefix."
  :group 'vhdl-ts-faces)

(defvar vhdl-ts-font-lock-translate-off-face 'vhdl-ts-font-lock-translate-off-face)
(defface vhdl-ts-font-lock-translate-off-face
  '((t (:slant italic)))
  "Face for pragmas between comments, e.g:
* translate_off / * translate_on"
  :group 'vhdl-ts-faces)

;;;; Functions
(defun vhdl-ts--fontify-error (node _override start end &rest _)
  "Fontify a syntax error with a red wavy underline.

For NODE,OVERRIDE, START, END, and ARGS, see `treesit-font-lock-rules'."
  (when vhdl-ts-linter-enable
    (treesit-fontify-with-override (treesit-node-start node)
                                   (treesit-node-end node)
                                   '(:underline (:style wave :color "Red1"))
                                   'append
                                   start end)))

;;;; Keywords
(defconst vhdl-ts-keywords
  '("abs" "access" "after" "alias" "and" "architecture" "array" "assert"
    "attribute" "begin" "block" "body" "buffer" "bus" "case" "component"
    "configuration" "constant" "context" "disconnect" "downto" "else" "elsif" "end"
    "entity" "exit" "file" "for" "force" "function" "generate" "generic" "group"
    "guarded" "if" "impure" "in" "inertial" "inout" "is" "label" "library" "linkage"
    "literal" "loop" "map" "mod" "nand" "new" "next" "nor" "not" "null" "of" "on"
    "or" "out" "package" "parameter" "port" "postponed" "procedure" "process"
    "property" "protected" "private" "pure" "range" "record" "register" "reject"
    "release" "rem" "report" "return" "rol" "ror" "select" "sequence" "severity"
    "signal" "shared" "sla" "sll" "sra" "srl" "subtype" "then" "to" "transport"
    "type" "unaffected" "units" "until" "use" "variable" "view" "vunit" "wait"
    "when" "while" "with" "xnor" "xor"))

;;;; Treesit-settings
(defvar vhdl-ts--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'vhdl
   '((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :feature 'string
   :language 'vhdl
   '([(string_literal)
      (bit_string_literal)
      (character_literal)
      (operator_symbol)
      (library_constant_std_logic)
      (string_literal_std_logic)]
     @font-lock-string-face)

   :feature 'keyword
   :language 'vhdl
   `((["downto" "to"] @vhdl-ts-font-lock-instance-lib-face)
     (["then"] @vhdl-ts-font-lock-then-face)
     ([,@vhdl-ts-keywords (OTHERS) (OPEN) (ALL)] @font-lock-keyword-face)
     ((attribute) @font-lock-builtin-face))

   :feature 'punctuation
   :language 'vhdl
   `(([";" ":" "," "'" "|" "." "?"] @vhdl-ts-font-lock-punctuation-face)
     (["(" ")" "[" "]"] @vhdl-ts-font-lock-parenthesis-face))

   :feature 'operator
   :language 'vhdl
   `(([(unary_operator)
       (logical_operator)
       (shift_operator)
       (sign)
       (adding_operator)
       (multiplying_operator)
       (exponentiate)]
      @vhdl-ts-font-lock-operator-face)
     ([(relational_operator)
       (variable_assignment)
       (signal_assignment)
       "=>"]
      @vhdl-ts-font-lock-punctuation-face))

   :feature 'builtin
   :language 'vhdl
   `(((library_constant) @font-lock-constant-face)
     ((library_constant_debug) @font-lock-constant-face)
     ((library_function) @font-lock-builtin-face)
     ((library_type) @font-lock-type-face))

   :feature 'declaration
   :language 'vhdl
   '(;; Entity
     (entity_declaration
      entity: (identifier) @font-lock-function-name-face)
     ;; Architecture
     (architecture_definition
      architecture: (identifier) @font-lock-function-name-face
      entity: (name (identifier) @font-lock-function-name-face))
     ;; Component
     (component_declaration
      component: (identifier) @font-lock-function-name-face)
     ;; Package
     (package_declaration
      package: (identifier) @font-lock-function-name-face)
     (package_definition
      package: (identifier) @font-lock-function-name-face)
     ;; Subprogram
     (procedure_specification
      procedure: (identifier) @font-lock-function-name-face)
     (function_specification
      function: (identifier) @font-lock-function-name-face)
     ;; Constants
     (constant_declaration
      (identifier_list
       constant: (identifier) @font-lock-constant-face))
     ;; Alias
     (alias_declaration
      (identifier) @font-lock-constant-face))

   :feature 'type
   :language 'vhdl
   `((type_declaration
      type: (identifier) @font-lock-type-face)
     (subtype_declaration
      type: (identifier) @font-lock-type-face)
     (subtype_indication
      (name
       (identifier) @font-lock-type-face))
     (subtype_indication
      (name
       (library_type) @font-lock-type-face)))

   :feature 'instance-1
   :language 'vhdl
   '(;; Entity instantiation
     (component_instantiation_statement
      (label_declaration (label) @vhdl-ts-font-lock-instance-face)
      (instantiated_unit
       entity: (name
                (identifier) @vhdl-ts-font-lock-instance-lib-face
                (selection
                 (identifier) @vhdl-ts-font-lock-entity-face))))
     (component_instantiation_statement
      (label_declaration (label) @vhdl-ts-font-lock-instance-face)
      (instantiated_unit
       library: (library_namespace) @vhdl-ts-font-lock-instance-lib-face
       entity: (name
                (identifier) @vhdl-ts-font-lock-entity-face)))
     ;; Component instantiation
     (component_instantiation_statement
      (label_declaration (label) @vhdl-ts-font-lock-instance-face)
      component: (name (identifier) @vhdl-ts-font-lock-entity-face)))

   :feature 'instance-2
   :language 'vhdl
   '(;; Entity instantiation without library prefix.
     ;; - Needed in another feature group to make it have less precedence
     ;;   than entity instantiation with library prefix
     (component_instantiation_statement
      (label_declaration (label) @vhdl-ts-font-lock-instance-face)
      (instantiated_unit
       entity: (name
                (identifier) @vhdl-ts-font-lock-entity-face))))

   :feature 'array
   :language 'vhdl
   :override t
   '((simple_range
      (simple_expression) @vhdl-ts-font-lock-brackets-content-face))

   :feature 'misc
   :language 'vhdl
   '(;; Library
     (library_clause
      (logical_name_list
       (library_namespace) @font-lock-builtin-face))
     (library_clause
      (logical_name_list
       (identifier) @font-lock-builtin-face))
     (use_clause
      (selected_name_list
       (selected_name
        library: (library_namespace) @font-lock-function-name-face
        package: (identifier) @font-lock-function-name-face)))
     (use_clause
      (selected_name_list
       (selected_name
        library: (identifier) @font-lock-function-name-face
        package: (identifier) @font-lock-function-name-face)))
     (use_clause
      (selected_name_list
       (selected_name
        library: (identifier) @font-lock-function-name-face)))
     ;; Generate
     (if_generate_statement
      (label_declaration
       (label) @font-lock-constant-face))
     (for_generate_statement
      (label_declaration
       (label) @font-lock-constant-face))
     (case_generate_statement
      (label_declaration
       (label) @font-lock-constant-face))
     ;; Block
     (block_statement
      (label_declaration
       (label) @font-lock-constant-face))
     ;; Process
     (process_statement
      (label_declaration
       (label) @font-lock-constant-face))
     (process_statement
      (sensitivity_specification
       (sensitivity_list
        (name
         (identifier) @font-lock-constant-face))))
     ;; Port connections
     (association_list
      (association_element
       (name
        (identifier) @vhdl-ts-font-lock-instance-lib-face
        (selection
         (identifier) @vhdl-ts-font-lock-port-connection-face))))
     (association_list
      (association_element
       (name
        (identifier) @vhdl-ts-font-lock-port-connection-face)
       "=>"))
     (association_list
      (association_element
       (name
        (identifier) @vhdl-ts-font-lock-port-connection-face
        (parenthesis_group
         (association_or_range_list) @vhdl-ts-font-lock-brackets-content-face))
       "=>"))
     (association_or_range_list
      (association_element
       (name
        (identifier) @vhdl-ts-font-lock-port-connection-face)
       "=>"))
     ;; Enum labels
     (enumeration_type_definition
      (enumeration_literal
       constant: (identifier) @font-lock-constant-face))
     ;; Record elements
     (name
      (identifier) @vhdl-ts-font-lock-instance-lib-face
      (selection
       (identifier)))
     ;; Label fallback
     (label_declaration
      (label) @font-lock-constant-face))

   :feature 'error
   :language 'vhdl
   :override t
   '((ERROR) @vhdl-ts--fontify-error)))

;;; Indent
;;;; Matchers
(defun vhdl-ts--matcher-grandparent-is (type)
  "A tree-sitter simple indent matcher for node TYPE."
  (lambda (_node parent _bol)
    (let ((grandparent (treesit-node-parent parent)))
      (if type
          (and grandparent
               (string-match-p type (treesit-node-type grandparent)))
        (null grandparent)))))

(defun vhdl-ts--matcher-blank-line (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Matches if point is at a blank line."
  (unless node
    t))

(defun vhdl-ts--matcher-generic-or-port (node &rest _)
  "A tree-sitter simple indent matcher for NODE.
Matches if point is at generic/port declaration."
  (let* ((node-type (treesit-node-type node))
         (entity-or-comp-node (vhdl-ts--node-has-parent-recursive node "\\(entity\\|component\\)_declaration")))
    (when (and entity-or-comp-node
               (string-match "\\(\\(generic\\|port\\)_clause\\|\\(entity\\|component\\)_head\\)" node-type))
      (treesit-node-start entity-or-comp-node))))

(defun vhdl-ts--matcher-default (&rest _)
  "A tree-sitter simple indent matcher."
  t)


;;;; Anchors
(defun vhdl-ts--anchor-grand-parent-bol (_node parent _bol)
  "Return the beginning of indentation of the grandparent node."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun vhdl-ts--anchor-subprogram-port (node &rest _)
  "A tree-sitter simple indent anchor for NODE."
  (let ((spec-node (vhdl-ts--node-has-parent-recursive node "\\(procedure\\|function\\)_specification")))
    (treesit-node-start spec-node)))

(defun vhdl-ts--anchor-if-statement-block (_node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let* ((grandparent (treesit-node-parent parent)))
    (cond ((string-match "\\_<if_statement_body\\_>" (treesit-node-type parent))
           (treesit-node-start parent))
          ((string-match "\\_<loop_body\\_>" (treesit-node-type parent))
           (+ (treesit-node-start grandparent) vhdl-ts-indent-level))
          (t
           (+ (treesit-node-start parent) vhdl-ts-indent-level)))))

(defun vhdl-ts--anchor-association-element (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let* ((grandparent (treesit-node-parent parent))
         (prev-line-node-bol (save-excursion
                               (goto-char (treesit-node-start grandparent))
                               (vhdl-ts--node-at-bol)))
         proc-call-node)
    (cond ((setq proc-call-node (vhdl-ts--node-has-parent-recursive node "\\_<procedure_call_statement\\_>"))
           (+ (treesit-node-start proc-call-node) vhdl-ts-indent-level))
          ((string-match-p "\\(generic\\|port\\)_map_aspect" (treesit-node-type prev-line-node-bol))
           (+ (treesit-node-start grandparent) vhdl-ts-indent-level))
          (t
           (+ (treesit-node-start prev-line-node-bol) vhdl-ts-indent-level)))))

(defun vhdl-ts--anchor-generate-block (node _parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (treesit-node-start (vhdl-ts--node-has-parent-recursive node "\\(for\\|if\\|case\\)_generate_statement")))

(defun vhdl-ts--anchor-enumeration-literal (node parent &rest _)
  "A tree-sitter simple indent anchor for NODE and PARENT."
  (let ((grandparent (treesit-node-parent parent)))
    (cond ((treesit-node-eq node (treesit-node-child parent 1)) ; 1st child of enumeration_type_definition, skip open parenthesis
           (+ (treesit-node-start grandparent) vhdl-ts-indent-level))
          (t
           (treesit-node-start (treesit-node-child parent 1))))))


;;;; Rules
(defconst vhdl-ts--indent-zero-parent-node-re
  (eval-when-compile
    (regexp-opt '("design_file" "design_unit" "library_clause" "use_clause" "context_reference") 'symbols)))

(defconst vhdl-ts--treesit-indent-rules
  `((vhdl
     ;; Comments
     ((and (parent-is "\\(line\\|block\\)_comment") (vhdl-ts--matcher-grandparent-is "design_\\(file\\|unit\\)")) column-0 0)
     ((and (parent-is "\\(line\\|block\\)_comment") (vhdl-ts--matcher-grandparent-is "\\(interface_list\\|if_statement_body\\)")) grand-parent 0)
     ((parent-is "\\(line\\|block\\)_comment") vhdl-ts--anchor-grand-parent-bol vhdl-ts-indent-level)
     ;; Zero-indent
     ((or (parent-is "design_unit") ; snippets outside entities/architectures/packages
          (node-is "\\(library\\|use\\)_clause")
          (node-is "design_unit") ; architecture_definition
          (node-is "entity_declaration")
          (node-is "package_\\(declaration\\|definition\\)"))
      parent-bol 0)
     ;; Entity generic/ports
     ((node-is "interface_list") parent-bol vhdl-ts-indent-level)
     ((and (node-is "interface_declaration") (parent-is "interface_list")) parent 0)
     ((and (node-is "interface_\\(constant\\|signal\\|variable\\|file\\)_declaration") (parent-is "interface_list")) vhdl-ts--anchor-subprogram-port vhdl-ts-indent-level)
     (vhdl-ts--matcher-generic-or-port grand-parent vhdl-ts-indent-level)
     ((node-is "component_body") parent-bol vhdl-ts-indent-level)
     ;; Declarations
     ((or (node-is "\\(component\\|signal\\|constant\\|element\\|variable\\|type\\|subprogram\\|alias\\|attribute\\)_declaration")
          (node-is "attribute_specification"))
      grand-parent vhdl-ts-indent-level)
     ;; Concurrent & generate
     ((node-is "concurrent_\\(simple\\|conditional\\|selected\\)_signal_assignment") parent-bol vhdl-ts-indent-level)
     ((node-is "\\(if\\|for\\|case\\)_generate_statement") parent-bol vhdl-ts-indent-level)
     ((node-is "generate_block") vhdl-ts--anchor-generate-block vhdl-ts-indent-level)
     ;; Processes
     ((node-is "process_statement") grand-parent vhdl-ts-indent-level)
     ((node-is "process_head") parent-bol vhdl-ts-indent-level)
     ((node-is "subprogram_definition") grand-parent vhdl-ts-indent-level)
     ;; Instances
     ((node-is "component_instantiation_statement") grand-parent vhdl-ts-indent-level)
     ((node-is "\\(generic\\|port\\)_map_aspect") parent-bol vhdl-ts-indent-level)
     ((node-is "association_\\(element\\|or_range_list\\)") vhdl-ts--anchor-association-element 0)
     ;; Procedural
     ((node-is "if_statement_block") vhdl-ts--anchor-if-statement-block 0)
     ((node-is "if_statement_body") parent vhdl-ts-indent-level)
     ((parent-is "sequential_block") parent-bol vhdl-ts-indent-level)
     ((parent-is "loop_body") parent-bol vhdl-ts-indent-level)
     ;; Concurrent
     ((and (node-is "waveform") (parent-is "else_waveform")) grand-parent 0)
     ((and (node-is "waveform") (parent-is "selected_waveforms")) parent 0)
     ((node-is "selected_waveforms") parent vhdl-ts-indent-level)
     ((and (node-is "name") (parent-is "select_target")) grand-parent vhdl-ts-indent-level)
     ;; Case statements
     ((node-is "case_statement_alternative") grand-parent vhdl-ts-indent-level)
     ((parent-is "case_statement_alternative") parent vhdl-ts-indent-level)
     ;; Others
     ((node-is "enumeration_literal") vhdl-ts--anchor-enumeration-literal 0)
     ((node-is "element_association_list") parent-bol vhdl-ts-indent-level)
     ((node-is "element_association") parent 0)
     ((node-is "block_\\(head\\|statement\\)") parent-bol vhdl-ts-indent-level)
     ;; Opening & closing
     ((node-is "\\(begin\\|end\\|)\\)") parent-bol 0)
     ;; Blank lines
     ((and vhdl-ts--matcher-blank-line (parent-is ,vhdl-ts--indent-zero-parent-node-re)) parent-bol 0)
     (vhdl-ts--matcher-blank-line parent-bol vhdl-ts-indent-level)
     ;; Fallbacks/default
     (vhdl-ts--matcher-default parent 0))))


;;; Imenu
(defconst vhdl-ts-imenu-create-index-re
  (eval-when-compile
    (regexp-opt
     '(;; 3.2 Entity declarations
       "entity_declaration"
       ;; 3.3 Architecture bodies
       "architecture_definition"
       ;; 3.4 Configuration declarations
       "configuration_declaration"
       ;; 3.4.3 Component configuration
       "component_configuration"
       ;; 4.2.1 Subprogram declarations
       "procedure_specification"
       "interface_procedure_specification"
       "function_specification"
       "interface_function_specification"
       ;; 4.7 Package declarations
       "package_declaration"
       ;; 4.8 Package bodies
       "package_definition"
       ;; 6.8 Component declarations
       "component_declaration"
       ;; 11 Concurrent statements
       "block_statement"
       "process_statement"
       "component_instantiation_statement"
       "for_generate_statement"
       "if_generate_statement"
       "case_generate_statement")
     'symbols)))

(defvar vhdl-ts-imenu-format-item-label-function
  'vhdl-ts-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar vhdl-ts-imenu-format-parent-item-label-function
  'vhdl-ts-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar vhdl-ts-imenu-format-parent-item-jump-label-function
  'vhdl-ts-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun vhdl-ts-imenu-format-item-label (_type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s" name))

(defun vhdl-ts-imenu-format-parent-item-label (_type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s" name))

(defun vhdl-ts-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (format "*%s*" type))

(defun vhdl-ts-imenu-treesit-create-index-tree (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\".

Copied from Python's `python--imenu-treesit-create-index-1' and adapted to
VHDL parser."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'vhdl-ts-imenu-treesit-create-index-tree
                           children))
         (type (treesit-node-type ts-node))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (vhdl-ts--node-identifier-name ts-node)
                     "Anonymous")))
         (pos (treesit-node-start ts-node))
         (marker (when ts-node
                   (if imenu-use-markers
                       (set-marker (make-marker) pos)
                     pos))))
    (cond
     ;; Root node
     ((null ts-node)
      subtrees)
     ;; Non-leaf node
     (subtrees
      (let ((parent-label (funcall vhdl-ts-imenu-format-parent-item-label-function type name))
            (jump-label (funcall vhdl-ts-imenu-format-parent-item-jump-label-function type name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     ;; Leaf node
     (t (let ((label (funcall vhdl-ts-imenu-format-item-label-function type name)))
          (list (cons label marker)))))))

(defun vhdl-ts--imenu-treesit-create-index-tree-group-process (subtree)
  "Utility function to process SUBTREE and group leaves into categories."
  (let (instances processes procedures functions components generates default)
    (mapc
     (lambda (elm)
       (if (and (listp elm) (listp (cdr elm)) (listp (cddr elm)) ; Basic checks due to custom imenu entry format for grouping
                (or (numberp (cadr elm)) (markerp (cadr elm)))   ; Element can be grouped because it was added ...
                (stringp (caddr elm))) ; ... a third field, indicating tree-sitter type
           (let ((type (caddr elm))
                 (entry (cons (car elm) (cadr elm))))
             (pcase type
               ("component_instantiation_statement" (push entry instances))
               ("process_statement" (push entry processes))
               ((or "procedure_specification" "interface_procedure_specification") (push entry procedures))
               ((or "function_specification" "interface_function_specification") (push entry functions))
               ("component_declaration" (push entry components))
               ((or "for_generate_statement" "if_generate_statement" "case_generate_statement") (push entry generates))
               (_ (push entry default))))
         ;; Otherwise entry cannot be grouped because it already was, or because it was a leaf node
         (push elm default)))
     subtree)
    ;; Populate return value
    (if (or processes instances procedures functions components generates) ; Avoid processing when no grouping is required
        (progn
          (when instances
            (setq instances (nreverse instances))
            (setq default `(("Instances" ,@instances) ,@default)))
          (when processes
            (setq processes (nreverse processes))
            (setq default `(("Processes" ,@processes) ,@default)))
          (when procedures
            (setq procedures (nreverse procedures))
            (setq default `(("Procedures" ,@procedures) ,@default)))
          (when functions
            (setq functions (nreverse functions))
            (setq default `(("Functions" ,@functions) ,@default)))
          (when components
            (setq components (nreverse components))
            (setq default `(("Components" ,@components) ,@default)))
          (when generates
            (setq generates (nreverse generates))
            (setq default `(("Generates" ,@generates) ,@default)))
          default)
      ;; Else it might be processing of the leaf nodes of top subtree and reordering is required
      (nreverse default))))

(defun vhdl-ts-imenu-treesit-create-index-tree-group (node)
  "Given a sparse tree, create an imenu alist.

NODE is the root node of the tree returned by
`treesit-induce-sparse-tree' (not a tree-sitter node, its car is
a tree-sitter node).  Walk that tree and return an imenu alist.

Return a list of ENTRY where

ENTRY := (NAME . MARKER)
       | (NAME . ((JUMP-LABEL . MARKER)
                  ENTRY
                  ...)

NAME is the function/class's name, JUMP-LABEL is like \"*function
definition*\".

Copied from Python's `python--imenu-treesit-create-index-1' and adapted to
VHDL parser."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'vhdl-ts-imenu-treesit-create-index-tree-group
                           children))
         (type (treesit-node-type ts-node))
         ;; The root of the tree could have a nil ts-node.
         (name (when ts-node
                 (or (vhdl-ts--node-identifier-name ts-node)
                     "Anonymous")))
         (pos (treesit-node-start ts-node))
         (marker (when ts-node
                   (if imenu-use-markers
                       (set-marker (make-marker) pos)
                     pos))))
    (cond
     ;; Root node
     ((null ts-node)
      (vhdl-ts--imenu-treesit-create-index-tree-group-process subtrees))
     ;; Non-leaf node
     (subtrees
      (let ((parent-label (funcall vhdl-ts-imenu-format-parent-item-label-function type name))
            (jump-label (funcall vhdl-ts-imenu-format-parent-item-jump-label-function type name)))
        `((,parent-label
           ,(cons jump-label marker)
           ,@(vhdl-ts--imenu-treesit-create-index-tree-group-process subtrees)))))
     ;; Leaf node
     (t (let ((label (funcall vhdl-ts-imenu-format-item-label-function type name)))
          (if (member type '("component_instantiation_statement"
                             "process_statement"
                             "procedure_specification"
                             "interface_procedure_specification"
                             "function_specification"
                             "interface_function_specification"
                             "component_declaration"
                             "for_generate_statement"
                             "if_generate_statement"
                             "case_generate_statement"))
              (list (list label marker type))
            (list (cons label marker))))))))

(defun vhdl-ts--imenu-create-index (func &optional node)
  "Imenu index builder function for `vhdl-ts-mode'.

FUNC is the function that traverses the syntax tree and builds the index suited
for Imenu.

NODE is the root node of the subtree you want to build an index
of.  If nil, use the root node of the whole parse tree.

Copied from Python's `python-imenu-treesit-create-index' and adapted
to VHDL parser."
  (let* ((node (or node (treesit-buffer-root-node 'vhdl)))
         (tree (treesit-induce-sparse-tree
                node
                vhdl-ts-imenu-create-index-re
                nil 1000)))
    (funcall func tree)))

(defun vhdl-ts-imenu-create-index-tree ()
  "Create `imenu' index alist with a tree structure."
  (vhdl-ts--imenu-create-index #'vhdl-ts-imenu-treesit-create-index-tree))

(defun vhdl-ts-imenu-create-index-tree-group ()
  "Create `imenu' index alist with a tree structure with subgroups."
  (vhdl-ts--imenu-create-index #'vhdl-ts-imenu-treesit-create-index-tree-group))

(defun vhdl-ts-imenu-setup ()
  "Setup `imenu' based on value of `vhdl-ts-imenu-style'."
  (pcase vhdl-ts-imenu-style
    ('simple
     (setq-local treesit-simple-imenu-settings
                 `(("Entity" "\\`entity_declaration\\'")
                   ("Architecture" "\\`architecture_definition\\'")
                   ("Package" "\\`package_\\(declaration\\|definition\\)\\'")
                   ("Component" "\\`component_declaration\\'")
                   ("Process" "\\`process_statement\\'")
                   ("Procedure" "\\`\\(interface_\\)?procedure_specification\\'")
                   ("Function" "\\`\\(interface_\\)?function_specification\\'")
                   ("Block" "\\`block_statement\\'")
                   ("Generate" "\\`\\(for\\|if\\|case\\)_generate_statement\\'")
                   ("Instance" "\\`component_instantiation_statement\\'")))
     (setq-local treesit-defun-name-function #'vhdl-ts--node-identifier-name))
    ('tree
     (setq-local imenu-create-index-function #'vhdl-ts-imenu-create-index-tree))
    ('tree-group
     (setq-local imenu-create-index-function #'vhdl-ts-imenu-create-index-tree-group))
    (_ (error "Wrong value for `vhdl-ts-imenu-style': set to simple, tree or tree-group"))))


;;; Which-func
(defconst vhdl-ts-which-func-re
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_definition"
       "configuration_declaration"
       "component_configuration"
       "subprogram_definition"
       "package_declaration"
       "package_definition"
       "component_declaration"
       "block_statement"
       "process_statement"
       "component_instantiation_statement"
       "for_generate_statement"
       "if_generate_statement"
       "case_generate_statement")
     'symbols))
  "Same as `vhdl-ts-imenu-create-index-re' but using subprogram_definition.")

(defvar-local vhdl-ts-which-func-extra nil
  "Variable to hold extra information for `which-func'.")

(defvar vhdl-ts-which-func-format-function 'vhdl-ts-which-func-format-shorten
  "Variable of the function to be called to return the type in `which-func'.

It must have one argument (tree-sitter node) and must return a string with the
type.")

(defun vhdl-ts-which-func-format-simple (node)
  "Return tree-sitter type of current NODE."
  (treesit-node-type node))

(defun vhdl-ts-which-func-format-shorten (node)
  "Return shortened name of NODE if possible."
  (pcase (treesit-node-type node)
    ("entity_declaration"                "ent")
    ("architecture_definition"           "arch")
    ("package_definition"                "pkg")
    ("process_statement"                 "proc")
    ("block_statement"                   "blk")
    ("if_generate_statement"             "gen")
    ("for_generate_statement"            "gen")
    ("case_generate_statement"           "gen")
    ("component_instantiation_statement" (vhdl-ts--node-identifier-name node))
    ("subprogram_definition" (cond ((treesit-search-subtree node "procedure_specification")
                                    "pcd")
                                   ((treesit-search-subtree node "function_specification")
                                    "fun")
                                   (t
                                    (error "Unexpected choice"))))
    (_ (treesit-node-type node))))

(defun vhdl-ts-which-func-function ()
  "Retrieve `which-func' candidates."
  (let ((node (vhdl-ts--block-at-point vhdl-ts-which-func-re)))
    (if node
        (progn
          (setq vhdl-ts-which-func-extra (if (string= (treesit-node-type node) "component_instantiation_statement")
                                             (vhdl-ts--node-instance-name node)
                                           (vhdl-ts--node-identifier-name node)))
          (funcall vhdl-ts-which-func-format-function node))
      (setq vhdl-ts-which-func-extra nil)
      "n/a")))

(defun vhdl-ts-which-func-setup ()
  "Hook for `vhdl-ts-mode' to enable `which-func'."
  (pcase vhdl-ts-which-func-style
    ('simple
      (setq-local which-func-functions nil)
      (setq-local which-func-imenu-joiner-function (lambda (x) (car (last x)))))
    ('breadcrumb
     (setq-local which-func-functions nil)
     (setq-local which-func-imenu-joiner-function (lambda (x) (string-join x "."))))
    ('custom
     (setq-local which-func-functions '(vhdl-ts-which-func-function))
     (setq-local which-func-format
                 `("["
                   (:propertize which-func-current
                    face (which-func :weight bold)
                    mouse-face mode-line-highlight)
                   ":"
                   (:propertize vhdl-ts-which-func-extra
                    face which-func
                    mouse-face mode-line-highlight)
                   "]")))
    (_ (error "Wrong value for `vhdl-ts-which-func-style': set to default/breadcrumb/custom"))))


;;; Navigation
(defconst vhdl-ts-defun-re
  (eval-when-compile
    (regexp-opt
     '("entity_declaration"
       "architecture_definition"
       "package_declaration"
       "package_definition"
       "process_statement"
       "subprogram_declaration"
       "subprogram_definition"
       "procedure_declaration"
       "procedure_specification"
       "function_declaration"
       "function_specification"
       "component_declaration"
       "configuration_declaration"
       "context_declaration"
       "loop_statement"
       "if_generate_statement"
       "for_generate_statement"
       "case_generate_statement"
       "block_statement"
       "record_type_definition")
     'symbols)))

(defun vhdl-ts-forward-sexp (&optional arg)
  "Move forward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (let* ((node (vhdl-ts--node-at-point))
         (node-type (treesit-node-type node))
         (highest-node (treesit-parent-while ; Same as `vhdl-ts--highest-node-at-point' but ignoring design_unit
                        node
                        (lambda (loop-node)
                          (and (eq (treesit-node-start node) (treesit-node-start loop-node))
                               (not (string-match "\\_<design_unit\\_>" (treesit-node-type loop-node)))))))
         (highest-node-type (treesit-node-type highest-node)))
    (cond (;; Defuns/if
           (or (string-match vhdl-ts-defun-re highest-node-type)
               (string= "if_statement_block" highest-node-type))
           (if (and arg (< arg 0))
               (goto-char (treesit-node-start highest-node))
             (goto-char (treesit-node-end highest-node))))
          (;; if/else/elsif/then
           (string-match "\\_<\\(if_statement_body\\|\\(else\\|elsif\\)_statement\\|then\\)\\_>" highest-node-type)
           (let ((parent (vhdl-ts--node-has-parent-recursive node "\\_<if_statement_block\\_>")))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-start parent))
               (goto-char (treesit-node-end parent)))))
          (;; generate
           (string-match "\\_<\\(\\(if\\|for\\|case\\)_generate\\|generate_body\\)\\_>" highest-node-type)
           (let ((parent (vhdl-ts--node-has-parent-recursive node "\\_<\\(if\\|for\\|case\\)_generate_statement\\_>")))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-start parent))
               (goto-char (treesit-node-end parent)))))
          (;; for/loop
           (string-match "\\_<\\(for\\|loop\\)\\_>" node-type)
           (let ((parent (vhdl-ts--node-has-parent-recursive node "\\_<loop_statement\\_>")))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-start parent))
               (goto-char (treesit-node-end parent)))))
          (;; procedure/functions and begin/is
           (string-match "\\_<\\(function\\|procedure\\|begin\\|is\\)\\_>" node-type)
           (let ((grand-parent (treesit-node-parent (treesit-node-parent node))))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-start grand-parent))
               (goto-char (treesit-node-end grand-parent)))))
          (;; labeled process
           (string-match "\\_<process\\_>" node-type)
           (let ((parent (treesit-node-parent node)))
             (if (and arg (< arg 0))
                 (goto-char (treesit-node-start parent))
               (goto-char (treesit-node-end parent)))))
          ;; Default
          (t
           (if (and arg (< arg 0))
               (backward-sexp arg)
             (forward-sexp arg))))))

(defun vhdl-ts-backward-sexp (&optional arg)
  "Move backward across S-expressions.

With `prefix-arg', move ARG expressions."
  (interactive "p")
  (let* ((node (vhdl-ts--node-at-point))
         (parent-node (treesit-node-parent node))
         (parent-node-type (treesit-node-type parent-node)))
    (cond (;; Defuns/if-else-elsif
           (or (string-match vhdl-ts-defun-re parent-node-type)
               (string= "if_statement_block" parent-node-type))
           (let ((beg (treesit-node-start parent-node))
                 (end (treesit-node-end parent-node)))
             (if (and arg (< arg 0))
                 (goto-char end)
               (goto-char beg))))
          (;; Ending blocks
           (string-match "\\(\\_<end_\\|_end\\_>\\)" parent-node-type)
           (let* ((grandparent-node (treesit-node-parent parent-node))
                  (beg (treesit-node-start grandparent-node))
                  (end (treesit-node-end grandparent-node)))
             (if (and arg (< arg 0))
                 (goto-char end)
               (goto-char beg))))
          ;; Default
          (t
           (if (and arg (< arg 0))
               (forward-sexp arg)
             (backward-sexp arg))))))

(defun vhdl-ts-find-function-procedure (&optional bwd)
  "Search for a VHDL function/procedure declaration or definition.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "\\(function\\|procedure\\)_specification" t bwd))

(defun vhdl-ts-find-function-procedure-fwd ()
  "Search forward for a VHDL function/procedure definition."
  (vhdl-ts-find-function-procedure))

(defun vhdl-ts-find-function-procedure-bwd ()
  "Search backward for a VHDL function/procedure definition."
  (vhdl-ts-find-function-procedure :bwd))

(defun vhdl-ts-find-block (&optional bwd)
  "Search for a VHDL block regexp, determined by `vhdl-ts-block-at-point-re'.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) vhdl-ts-block-at-point-re t bwd))

(defun vhdl-ts-find-block-fwd ()
  "Search forward for a VHDL block regexp."
  (vhdl-ts-find-block))

(defun vhdl-ts-find-block-bwd ()
  "Search backwards for a VHDL block regexp."
  (vhdl-ts-find-block :bwd))

(defun vhdl-ts-find-entity-instance (&optional bwd)
  "Search for a VHDL module/instance.

If optional arg BWD is non-nil, search backwards."
  (treesit-search-forward-goto (vhdl-ts--node-at-point) vhdl-ts-instance-re t bwd))

(defun vhdl-ts-find-entity-instance-fwd ()
  "Search forwards for a VHDL module/instance."
  (interactive)
  (vhdl-ts-find-entity-instance))

(defun vhdl-ts-find-entity-instance-bwd ()
  "Search backwards for a VHDL module/instance."
  (interactive)
  (vhdl-ts-find-entity-instance :bwd))

(defun vhdl-ts-goto-next-error ()
  "Move point to next tree-sitter parsing error."
  (interactive)
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "ERROR" t))

(defun vhdl-ts-goto-prev-error ()
  "Move point to previous tree-sitter parsing error."
  (interactive)
  (treesit-search-forward-goto (vhdl-ts--node-at-point) "ERROR" t :bwd))

;;; Beautify
(defconst vhdl-ts-align-alist
  '(;; after some keywords
    (vhdl-ts-mode "^\\s-*\\(across\\|constant\\|quantity\\|signal\\|subtype\\|terminal\\|through\\|type\\|variable\\)[ \t]"
                  "^\\s-*\\(across\\|constant\\|quantity\\|signal\\|subtype\\|terminal\\|through\\|type\\|variable\\)\\([ \t]+\\)" 2)
    ;; before ':'
    (vhdl-ts-mode ":[^=]" "\\([ \t]*\\):[^=]")
    ;; after direction specifications
    (vhdl-ts-mode ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\>"
                  ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\([ \t]+\\)" 2)
    ;; before "==", ":=", "=>", and "<="
    (vhdl-ts-mode "=>" "\\([ \t]*\\)=>" 1)
    (vhdl-ts-mode "[<:=]=" "\\([ \t]*\\)\\??[<:=]=" 1) ; since "=> ... <=" can occur
    ;; before some keywords
    (vhdl-ts-mode "[ \t]after\\>" "[^ \t]\\([ \t]+\\)after\\>" 1)
    (vhdl-ts-mode "[ \t]when\\>" "[^ \t]\\([ \t]+\\)when\\>" 1)
    (vhdl-ts-mode "[ \t]else\\>" "[^ \t]\\([ \t]+\\)else\\>" 1)
    (vhdl-ts-mode "[ \t]across\\>" "[^ \t]\\([ \t]+\\)across\\>" 1)
    (vhdl-ts-mode "[ \t]through\\>" "[^ \t]\\([ \t]+\\)through\\>" 1)
    ;; before "=>" since "when/else ... =>" can occur
    (vhdl-ts-mode "=>" "\\([ \t]*\\)=>" 1)))

(defun vhdl-ts-beautify--align-params-ports-nap ()
  "Align ports and params of instance of node at point."
  (let* ((re "\\(\\s-*\\)=>")
         (node (vhdl-ts-block-at-point))
         params-node ports-node)
    (when (setq params-node (vhdl-ts--node-has-child-recursive node "generic_map_aspect"))
      (align-regexp (treesit-node-start params-node) (treesit-node-end params-node) re 1 1 nil))
    (setq node (vhdl-ts-block-at-point)) ; Refresh outdated node after `align-regexp' for parameter list
    (when (setq ports-node (vhdl-ts--node-has-child-recursive node "port_map_aspect"))
      (align-regexp (treesit-node-start ports-node) (treesit-node-end ports-node) re 1 1 nil))))

(defun vhdl-ts-beautify-block-at-point ()
  "Beautify/indent block at point.

If block is an instance, also align parameters and ports."
  (interactive)
  (let ((node (vhdl-ts-block-at-point))
        (start (make-marker))
        (end (make-marker))
        type name)
    (unless node
      (user-error "Not inside a block"))
    (save-excursion
      (set-marker start (treesit-node-start node))
      (set-marker end (treesit-node-end node))
      (setq type (treesit-node-type node))
      (setq name (vhdl-ts--node-identifier-name node))
      (indent-region start end)
      ;; Refresh outdated node after `indent-region'
      (goto-char start)
      (skip-chars-forward " \t")
      (setq node (vhdl-ts-block-at-point))
      (set-marker start (treesit-node-start node))
      (set-marker end (treesit-node-end node))
      (vhdl-align-region start end)
      (when (and vhdl-ts-beautify-align-ports-and-params
                 (string-match vhdl-ts-instance-re type))
        (vhdl-ts-beautify--align-params-ports-nap)))
    (message "%s : %s" type name)))

(defun vhdl-ts-beautify-buffer ()
  "Beautify current buffer:
- Indent whole buffer
- Untabify and delete trailing whitespace
- Align"
  (interactive)
  (let (node)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))
    (vhdl-align-buffer)
    (when vhdl-ts-beautify-align-ports-and-params
      (save-excursion
        (goto-char (point-min))
        (while (setq node (treesit-search-forward (vhdl-ts--node-at-point) vhdl-ts-instance-re))
          (goto-char (treesit-node-start node))
          (vhdl-ts-beautify--align-params-ports-nap)
          (setq node (treesit-search-forward (vhdl-ts--node-at-point) vhdl-ts-instance-re))
          (goto-char (treesit-node-end node))
          (when (not (eobp))
            (forward-char)))))
    (message "Beautified: %s" buffer-file-name)))

;;; Completion
(defconst vhdl-ts-capf-excluded-keywords
  '("all" "assume" "cover" "default" "fairness" "open" "others" "restrict"
    "strong" "vmode" "vpkg" "vprop"))
(defconst vhdl-ts-capf-types (append vhdl-02-types vhdl-08-types vhdl-math-types))
(defconst vhdl-ts-capf-types-regexp (regexp-opt vhdl-ts-capf-types 'symbols))
(defconst vhdl-ts-capf-attributes (append vhdl-02-attributes vhdl-08-attributes))
(defconst vhdl-ts-capf-enum-values vhdl-02-enum-values)
(defconst vhdl-ts-capf-constants vhdl-math-constants)
(defconst vhdl-ts-capf-functions (append vhdl-02-functions vhdl-08-functions vhdl-math-functions))
(defconst vhdl-ts-capf-functions-regexp (regexp-opt vhdl-ts-capf-functions 'symbols))
(defconst vhdl-ts-capf-packages (append vhdl-02-packages vhdl-08-packages vhdl-math-packages))
(defconst vhdl-ts-capf-directives vhdl-08-directives)

(defun vhdl-ts-completion-at-point ()
  "VHDL tree-sitter powered completion at point.

Complete with keywords and current buffer identifiers."
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds))
         candidates)
    (setq candidates (remove (thing-at-point 'symbol :no-props)
                             (append (mapcar (lambda (node-and-props)
                                               (plist-get (cdr node-and-props) :name))
                                             (vhdl-ts-nodes-props vhdl-ts-identifier-re))
                                     vhdl-ts-keywords
                                     vhdl-ts-capf-excluded-keywords
                                     vhdl-ts-capf-types
                                     vhdl-ts-capf-types-regexp
                                     vhdl-ts-capf-attributes
                                     vhdl-ts-capf-enum-values
                                     vhdl-ts-capf-constants
                                     vhdl-ts-capf-functions
                                     vhdl-ts-capf-functions-regexp
                                     vhdl-ts-capf-packages
                                     vhdl-ts-capf-directives)))
    (list start end candidates . nil)))


;;; Major-mode
;;;; Setup
;;;###autoload
(defun vhdl-ts-install-grammar ()
  "Install VHDL tree-sitter grammar.

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and in PATH."
  (interactive)
  (let ((url "https://github.com/jpt13653903/tree-sitter-vhdl"))
    (add-to-list 'treesit-language-source-alist `(vhdl ,url))
    (treesit-install-language-grammar 'vhdl)))

(defvar-keymap vhdl-ts-mode-map
  :doc "Keymap for VHDL language with tree-sitter"
  :parent vhdl-mode-map
  "TAB"     #'indent-for-tab-command
  "C-M-a"   #'beginning-of-defun ; Unmap `vhdl-beginning-of-defun' from `vhdl-mode'
  "C-M-e"   #'end-of-defun       ; Unmap `vhdl-end-of-defun' from `vhdl-mode'
  "C-M-h"   #'mark-defun         ; Unmap `vhdl-mark-defun' from `vhdl-mode'
  "C-M-f"   #'vhdl-ts-forward-sexp
  "C-M-b"   #'vhdl-ts-backward-sexp
  "C-M-d"   #'vhdl-ts-find-entity-instance-fwd
  "C-M-u"   #'vhdl-ts-find-entity-instance-bwd
  "C-c C-b" #'vhdl-ts-beautify-buffer
  "C-c e n" #'vhdl-ts-goto-next-error
  "C-c e p" #'vhdl-ts-goto-prev-error)

(defvar vhdl-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\"     table)
    (modify-syntax-entry ?+  "."      table)
    (modify-syntax-entry ?-  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?%  "."      table)
    (modify-syntax-entry ?<  "."      table)
    (modify-syntax-entry ?>  "."      table)
    (modify-syntax-entry ?&  "."      table)
    (modify-syntax-entry ?|  "."      table)
    (modify-syntax-entry ?`  "."      table)
    (modify-syntax-entry ?_  "_"      table)
    (modify-syntax-entry ?\' "."      table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    table)
  "Syntax table used in VHDL mode buffers.")

;;;###autoload
(define-derived-mode vhdl-ts-mode vhdl-mode "VHDL"
  "Major mode for editing VHDL files, using tree-sitter library."
  :syntax-table vhdl-mode-syntax-table
  ;; Treesit
  (when (treesit-ready-p 'vhdl)
    (treesit-parser-create 'vhdl)
    ;; Font-lock.
    (setq font-lock-defaults nil) ; Disable `vhdl-mode' font-lock/indent config
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword operator)
                  (type declaration instance-2 instance-1 builtin misc error)
                  (punctuation array)))
    (setq-local treesit-font-lock-settings vhdl-ts--font-lock-settings)
    ;; Indent.
    (setq-local indent-line-function nil)
    (setq-local comment-indent-function nil)
    (setq-local treesit-simple-indent-rules vhdl-ts--treesit-indent-rules)
    ;; Navigation
    (setq-local treesit-defun-type-regexp vhdl-ts-defun-re)
    ;; Imenu.
    (vhdl-ts-imenu-setup)
    ;; Which-func
    (vhdl-ts-which-func-setup)
    ;; Completion
    (add-hook 'completion-at-point-functions #'vhdl-ts-completion-at-point nil 'local)
    ;; Beautify
    (setq-local vhdl-align-alist vhdl-ts-align-alist)
    ;; Setup
    (treesit-major-mode-setup)))


;;; Syntactic support overrides for compatibility with `vhdl-mode'
(defun vhdl-ts-in-comment-p (&optional pos)
  "Check if point is in a comment (include multi-line comments)."
  (let* ((node (if pos
                   (treesit-node-at pos 'vhdl)
                 (vhdl-ts--node-at-point)))
         (pos (or pos (point)))
         (type (treesit-node-type node))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    (and (string= type "comment_content")
         (>= pos (save-excursion ; `vhdl-in-comment-p' returns non-nil when point is after the --
                   (goto-char start)
                   (skip-chars-backward " \t")
                   (point)))
         (<= pos end))))

(defun vhdl-ts-in-comment-advice (fun &rest args)
  "Advice for `vhdl-in-comment-p' for `vhdl-ts-mode'."
  (if (eq major-mode 'vhdl-ts-mode)
      (apply #'vhdl-ts-in-comment-p args)
    (apply fun args)))

(defun vhdl-ts-in-literal ()
  "Determine if point is in a VHDL literal."
  (let* ((node (vhdl-ts--node-at-point))
         (pos (point))
         (type (treesit-node-type node))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    ;; INFO: `vhdl-in-literal' also supports cpp macros (see `vhdl-beginning-of-macro')
    (cond ((and (>= pos (1+ start)) ; `vhdl-in-literal' returns non-nil when point is after the ' or "
                (<= pos end)
                (or (string= type "decimal_integer")
                    (string= type "decimal_float")
                    (string= type "based_literal")
                    (string= type "bit_string_literal")
                    (string= type "string_literal")
                    (string= type "string_literal_std_logic")
                    (string= type "library_constant_boolean")
                    (string= type "library_constant_character")
                    (string= type "library_constant_env")
                    (string= type "library_constant_standard")
                    (string= type "library_constant_std_logic")
                    (string= type "null")
                    (string= type "character_literal")))
           'string)
          ((and (>= pos (save-excursion ; `vhdl-in-comment-p' returns non-nil when point is after the --
                          (goto-char start)
                          (skip-chars-backward " \t")
                          (point)))
                (<= pos end)
                (string= type "comment_content"))
           'comment)
          ((and (>= pos start)
                (<= pos end)
                (string-match "_\\(directive\\|analysis\\)" type))
           'directive)
          (t
           nil))))

(defun vhdl-ts-in-literal-advice (fun &rest args)
  "Advice for `vhdl-in-literal' for `vhdl-ts-mode'."
  (if (eq major-mode 'vhdl-ts-mode)
      (apply #'vhdl-ts-in-literal args)
    (apply fun args)))

;;;; Advice overrides
(advice-add 'vhdl-in-comment-p :around #'vhdl-ts-in-comment-advice)
(advice-add 'vhdl-in-literal   :around #'vhdl-ts-in-literal-advice)


;;; Provide
(provide 'vhdl-ts-mode)


;;; vhdl-ts-mode.el ends here
