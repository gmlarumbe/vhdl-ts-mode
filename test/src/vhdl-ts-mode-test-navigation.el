;;; vhdl-ts-mode-test-navigation.el --- vhdl-ts-mode ERT navigation tests  -*- lexical-binding: t -*-

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
;; vhdl-ts-mode ERT navigation tests
;;
;;; Code:

(defconst vhdl-ts-mode-test-ref-dir-navigation (file-name-concat vhdl-ts-mode-test-ref-dir "navigation"))
(defconst vhdl-ts-mode-test-dump-dir-navigation (file-name-concat vhdl-ts-mode-test-dump-dir "navigation"))


(defconst vhdl-ts-mode-test-navigation-rtl-file-list (mapcar (lambda (file)
                                                               (file-name-concat vhdl-ts-mode-test-files-common-dir file))
                                                             '("axi_if_converter.vhd"
                                                               "hierarchy.vhd"
                                                               "instances.vhd"
                                                               "tb_axi_if_converter.vhd")))
(defconst vhdl-ts-mode-test-navigation-tb-file-list (mapcar (lambda (file)
                                                              (file-name-concat vhdl-ts-mode-test-files-common-dir file))
                                                            '("global_pkg.vhd"
                                                              "global_sim.vhd"
                                                              "misc.vhd"
                                                              "sexp.vhd")))

(defconst vhdl-ts-mode-test-navigation-block-nav-file-list vhdl-ts-mode-test-common-file-list)


(defun vhdl-ts-mode-test-navigation-gen-expected-files ()
  ;; Instances fwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-rtl-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "inst.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-entity-instance-fwd))
  ;; Instances bwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-rtl-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "inst.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-entity-instance-bwd
                                       :start-pos-max t))
  ;; Procedure-functions fwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-tb-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "pf.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-function-procedure-fwd))
  ;; Procedure-functions bwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-tb-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "pf.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-function-procedure-bwd
                                       :start-pos-max t))
  ;; Block fwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-block-nav-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "block.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-block-fwd))
  ;; Block bwd
  (test-hdl-gen-expected-files :file-list vhdl-ts-mode-test-navigation-block-nav-file-list
                               :dest-dir vhdl-ts-mode-test-ref-dir-navigation
                               :out-file-ext "block.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ts-find-block-bwd
                                       :start-pos-max t)))


(ert-deftest vhdl-ts-mode::navigation::instances ()
  (dolist (file vhdl-ts-mode-test-navigation-rtl-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "inst.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-entity-instance-fwd))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "inst.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "inst.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-entity-instance-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "inst.bwd.el"))))))


(ert-deftest vhdl-ts-mode::navigation::procedure-functions ()
  (dolist (file vhdl-ts-mode-test-navigation-tb-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "pf.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-function-procedure-fwd))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "pf.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "pf.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-function-procedure-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "pf.bwd.el"))))))


(ert-deftest vhdl-ts-mode::navigation::blocks ()
  (dolist (file vhdl-ts-mode-test-navigation-block-nav-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "block.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-block-fwd))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "block.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-navigation (test-hdl-basename file "block.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ts-find-block-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat vhdl-ts-mode-test-ref-dir-navigation (test-hdl-basename file "block.bwd.el"))))))



(provide 'vhdl-ts-mode-test-navigation)

;;; vhdl-ts-mode-test-navigation.el ends here
