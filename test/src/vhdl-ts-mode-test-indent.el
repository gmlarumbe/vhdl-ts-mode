;;; vhdl-ts-mode-test-indent.el --- vhdl-ts-mode ERT indent tests  -*- lexical-binding: t -*-

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
;; vhdl-ts-mode ERT indent tests
;;
;;; Code:


(defconst vhdl-ts-mode-test-indent-file-list vhdl-ts-mode-test-common-file-list)

(defconst vhdl-ts-mode-test-ref-dir-indent (file-name-concat vhdl-ts-mode-test-ref-dir "indent"))
(defconst vhdl-ts-mode-test-dump-dir-indent (file-name-concat vhdl-ts-mode-test-dump-dir "indent"))


(defun vhdl-ts-mode-test-indent-fn ()
  (indent-region (point-min) (point-max)))


(defun vhdl-ts-mode-test-indent-gen-expected-files ()
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-indent-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-indent
                               :fn #'test-hdl-indent-buffer
                               :args '(vhdl-ts-mode vhdl-ts-mode-test-indent-fn))
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-indent-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-indent
                               :out-file-ext "no_deindent.vhd"
                               :fn #'test-hdl-indent-buffer
                               :args '(vhdl-ts-mode vhdl-ts-mode-test-indent-fn :no-deindent)))

(ert-deftest indent ()
  (dolist (file vhdl-ts-mode-test-indent-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-indent (test-hdl-basename file))
                                                         :fn #'test-hdl-indent-buffer
                                                         :args '(vhdl-ts-mode vhdl-ts-mode-test-indent-fn))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-indent (test-hdl-basename file))))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-indent (test-hdl-basename file "no_deindent.vhd"))
                                                         :fn #'test-hdl-indent-buffer
                                                         :args '(vhdl-ts-mode vhdl-ts-mode-test-indent-fn))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-indent (test-hdl-basename file "no_deindent.vhd"))))))


(provide 'vhdl-ts-mode-test-indent)

;;; vhdl-ts-mode-test-indent.el ends here
