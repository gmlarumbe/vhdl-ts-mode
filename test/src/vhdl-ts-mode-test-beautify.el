;;; vhdl-ts-mode-test-beautify.el --- vhdl-ts-mode ERT beautify tests  -*- lexical-binding: t -*-

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
;; vhdl-ts-mode ERT beautify tests
;;
;;; Code:


(defconst vhdl-ts-mode-test-beautify-file-list vhdl-ts-mode-test-common-file-list)

(defconst vhdl-ts-mode-test-ref-dir-beautify (file-name-concat vhdl-ts-mode-test-ref-dir "beautify"))
(defconst vhdl-ts-mode-test-dump-dir-beautify (file-name-concat vhdl-ts-mode-test-dump-dir "beautify"))


(defun vhdl-ts-mode-test-beautify-file ()
  (test-hdl-no-messages
    (vhdl-ts-mode))
  ;; Deindent
  (let ((blank-re "^\\s-+"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward blank-re nil t)
        (replace-match ""))))
  ;; Remove spaces in port connections
  (let ((port-conn-re (concat "\\(?1:^\\s-*" vhdl-ts-identifier-re "\\)\\(?2:\\s-*\\)=>")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward port-conn-re nil t)
        (replace-match "\\1=>"))))
  (test-hdl-no-messages
    (vhdl-ts-beautify-buffer)))

(defun vhdl-ts-mode-test-beautify-block-at-point-fn ()
  (test-hdl-no-messages
    (vhdl-ts-mode)
    (goto-char (point-min))
    (while (vhdl-ts-find-block-fwd)
      (vhdl-ts-beautify-block-at-point))))

(defun vhdl-ts-mode-test-beautify-gen-expected-files ()
  ;; Beautify buffer
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-beautify-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-beautify
                               :out-file-ext "beauty.vhd"
                               :fn #'vhdl-ts-mode-test-beautify-file)
  ;; Beautify buffer (`vhdl-ts-beautify-align-ports-and-params' t)
  (let ((vhdl-ts-beautify-align-ports-and-params t))
    (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-beautify-file-list
                                 :dest-dir vhdl-ts-mode-test-ref-dir-beautify
                                 :out-file-ext "beauty.app.vhd"
                                 :fn #'vhdl-ts-mode-test-beautify-file))
  ;; Beautify block at point
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-beautify-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-beautify
                               :out-file-ext "beauty.block.vhd"
                               :fn #'vhdl-ts-mode-test-beautify-block-at-point-fn)
  ;; Beautify block at point (`vhdl-ts-beautify-align-ports-and-params' t)
  (let ((vhdl-ts-beautify-align-ports-and-params t))
    (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-beautify-file-list
                                 :dest-dir vhdl-ts-mode-test-ref-dir-beautify
                                 :out-file-ext "beauty.block.app.vhd"
                                 :fn #'vhdl-ts-mode-test-beautify-block-at-point-fn)))


(ert-deftest beautify-buffer ()
  (dolist (file vhdl-ts-mode-test-beautify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.vhd"))
                                                         :fn #'vhdl-ts-mode-test-beautify-file)
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.vhd"))))))

(ert-deftest beautify-buffer-align-ports-params ()
  (let ((vhdl-ts-beautify-align-ports-and-params t))
    (dolist (file vhdl-ts-mode-test-beautify-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.app.vhd"))
                                                           :fn #'vhdl-ts-mode-test-beautify-file)
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.app.vhd")))))))

(ert-deftest beautify-block-at-point ()
  (dolist (file vhdl-ts-mode-test-beautify-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.block.vhd"))
                                                         :fn #'vhdl-ts-mode-test-beautify-block-at-point-fn)
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.block.vhd"))))))

(ert-deftest beautify-block-at-point-align-ports-params ()
  (let ((vhdl-ts-beautify-align-ports-and-params t))
    (dolist (file vhdl-ts-mode-test-beautify-file-list)
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-beautify (test-hdl-basename file "beauty.block.app.vhd"))
                                                           :fn #'vhdl-ts-mode-test-beautify-block-at-point-fn)
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-beautify (test-hdl-basename file "beauty.block.app.vhd")))))))




(provide 'vhdl-ts-mode-test-beautify)

;;; vhdl-ts-mode-test-beautify.el ends here
