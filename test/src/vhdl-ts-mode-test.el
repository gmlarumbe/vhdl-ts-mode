;;; vhdl-ts-mode-test.el --- vhdl-ts-mode ERT tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/test-hdl

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
;;
;; vhdl-ts-mode ERT tests
;;
;;; Code:


;; Allow loading of packages in Emacs interactive session
(defconst vhdl-ts-mode-test-dir (file-name-parent-directory (file-name-directory (or load-file-name (buffer-file-name)))))
(defconst vhdl-ts-mode-test-hdl-dir (file-name-concat vhdl-ts-mode-test-dir "test-hdl"))

(unless noninteractive
  (dolist (dir `(,(file-name-concat vhdl-ts-mode-test-dir "src")
                 ,vhdl-ts-mode-test-hdl-dir))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))))

(require 'test-hdl)
(require 'vhdl-ts-mode)


;;;; Directories
(defconst vhdl-ts-mode-test-ref-dir (file-name-concat vhdl-ts-mode-test-dir "ref"))
(defconst vhdl-ts-mode-test-dump-dir (file-name-concat vhdl-ts-mode-test-dir "dump"))
(defconst vhdl-ts-mode-test-files-dir (file-name-concat vhdl-ts-mode-test-dir "files"))
(defconst vhdl-ts-mode-test-files-common-dir (file-name-concat vhdl-ts-mode-test-files-dir "common"))
(defconst vhdl-ts-mode-test-axi-converter-dir (file-name-concat vhdl-ts-mode-test-files-dir "axi_if_converter"))
(defconst vhdl-ts-mode-test-axi-converter-rtl-dir (file-name-concat vhdl-ts-mode-test-axi-converter-dir "rtl"))
(defconst vhdl-ts-mode-test-axi-converter-tb-dir (file-name-concat vhdl-ts-mode-test-axi-converter-dir "tb"))

(defconst vhdl-ts-mode-test-common-file-list (test-hdl-directory-files vhdl-ts-mode-test-files-common-dir
                                                                       vhdl-ts-file-extension-re))

;;;; Tests
(require 'vhdl-ts-mode-test-faceup)
(require 'vhdl-ts-mode-test-indent)
(require 'vhdl-ts-mode-test-imenu)
(require 'vhdl-ts-mode-test-navigation)
(require 'vhdl-ts-mode-test-utils)
(require 'vhdl-ts-mode-test-beautify)


;;;; Aux funcs
(defun vhdl-ts-mode-test-gen-expected-files ()
  (vhdl-ts-mode-test-faceup-gen-expected-files)
  (vhdl-ts-mode-test-indent-gen-expected-files)
  (vhdl-ts-mode-test-imenu-gen-expected-files)
  (vhdl-ts-mode-test-navigation-gen-expected-files)
  (vhdl-ts-mode-test-utils-gen-expected-files)
  (vhdl-ts-mode-test-beautify-gen-expected-files))


(provide 'vhdl-ts-mode-test)

;;; vhdl-ts-mode-test.el ends here
