;;; vhdl-ts-mode-test-faceup.el --- vhdl-ts-mode ERT faceup tests  -*- lexical-binding: t -*-

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

;; vhdl-ts-mode ERT faceup tests

;;; Code:


(defconst vhdl-ts-mode-test-faceup-file-list vhdl-ts-mode-test-common-file-list)

(defconst vhdl-ts-mode-test-ref-dir-faceup (file-name-concat vhdl-ts-mode-test-ref-dir "faceup"))
(defconst vhdl-ts-mode-test-dump-dir-faceup (file-name-concat vhdl-ts-mode-test-dump-dir "faceup"))


(defun vhdl-ts-mode-test-faceup-gen-expected-files ()
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-faceup-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-faceup
                               :out-file-ext "faceup"
                               :fn #'test-hdl-faceup-test-file
                               :args '(vhdl-ts-mode)))

(ert-deftest faceup ()
  (dolist (file vhdl-ts-mode-test-faceup-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-faceup (test-hdl-basename file "faceup"))
                                                         :fn #'test-hdl-faceup-test-file
                                                         :args '(vhdl-ts-mode))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-faceup (test-hdl-basename file "faceup"))))))


(provide 'vhdl-ts-mode-test-faceup)


;;; vhdl-ts-mode-test-faceup.el ends here
