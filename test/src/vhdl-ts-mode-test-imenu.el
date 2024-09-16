;;; vhdl-ts-mode-test-imenu.el --- vhdl-ts-mode ERT imenu tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

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
;; vhdl-ts-mode ERT imenu tests
;;
;;; Code:

(defconst vhdl-ts-mode-test-imenu-file-list vhdl-ts-mode-test-common-file-list)

(defconst vhdl-ts-mode-test-ref-dir-imenu (file-name-concat vhdl-ts-mode-test-ref-dir "imenu"))
(defconst vhdl-ts-mode-test-dump-dir-imenu (file-name-concat vhdl-ts-mode-test-dump-dir "imenu"))


(defun vhdl-ts-mode-test-imenu-gen-expected-files ()
  ;; Simple
  (let ((vhdl-ts-imenu-style 'simple))
    (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-imenu-file-list
                                 :dest-dir vhdl-ts-mode-test-ref-dir-imenu
                                 :out-file-ext "simple.el"
                                 :process-fn 'eval-ff
                                 :fn #'test-hdl-imenu-test-file
                                 :args '(vhdl-ts-mode)))
  ;; Tree
  (let ((vhdl-ts-imenu-style 'tree))
    (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-imenu-file-list
                                 :dest-dir vhdl-ts-mode-test-ref-dir-imenu
                                 :out-file-ext "tree.el"
                                 :process-fn 'eval-ff
                                 :fn #'test-hdl-imenu-test-file
                                 :args '(vhdl-ts-mode)))

  ;; Tree-group
  (let ((vhdl-ts-imenu-style 'tree-group))
    (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-imenu-file-list
                                 :dest-dir vhdl-ts-mode-test-ref-dir-imenu
                                 :out-file-ext "tree.group.el"
                                 :process-fn 'eval-ff
                                 :fn #'test-hdl-imenu-test-file
                                 :args '(vhdl-ts-mode))))

(ert-deftest imenu::simple ()
  (let ((vhdl-ts-imenu-style 'simple))
    (dolist (file vhdl-ts-mode-test-imenu-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-imenu (test-hdl-basename file "simple.el"))
                                                           :process-fn 'eval-ff
                                                           :fn #'test-hdl-imenu-test-file
                                                           :args '(vhdl-ts-mode))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-imenu (test-hdl-basename file "simple.el")))))))

(ert-deftest imenu::tree ()
  (let ((vhdl-ts-imenu-style 'tree))
    (dolist (file vhdl-ts-mode-test-imenu-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-imenu (test-hdl-basename file "tree.el"))
                                                           :process-fn 'eval-ff
                                                           :fn #'test-hdl-imenu-test-file
                                                           :args '(vhdl-ts-mode))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-imenu (test-hdl-basename file "tree.el")))))))

(ert-deftest imenu::tree-group ()
  (let ((vhdl-ts-imenu-style 'tree-group))
    (dolist (file vhdl-ts-mode-test-imenu-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-imenu (test-hdl-basename file "tree.group.el"))
                                                           :process-fn 'eval-ff
                                                           :fn #'test-hdl-imenu-test-file
                                                           :args '(vhdl-ts-mode))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-imenu (test-hdl-basename file "tree.group.el")))))))


(provide 'vhdl-ts-mode-test-imenu)

;;; vhdl-ts-mode-test-imenu.el ends here
