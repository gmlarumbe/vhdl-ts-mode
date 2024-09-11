;;; vhdl-ts-mode-test-utils.el --- vhdl-ts-mode ERT utils tests  -*- lexical-binding: t -*-

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
;; vhdl-ts-mode ERT utils tests
;;
;;; Code:


(defconst vhdl-ts-mode-test-utils-file-list vhdl-ts-mode-test-common-file-list)
(defconst vhdl-ts-mode-test-ref-dir-utils (file-name-concat vhdl-ts-mode-test-ref-dir "utils"))
(defconst vhdl-ts-mode-test-dump-dir-utils (file-name-concat vhdl-ts-mode-test-dump-dir "utils"))

(defconst vhdl-ts-mode-test-utils-entity-at-point-file-and-pos
  `((,(file-name-concat vhdl-ts-mode-test-files-common-dir "axi_if_converter.vhd") 125 196 4140 9448 9814 10009 12471 31729 33623)
    (,(file-name-concat vhdl-ts-mode-test-files-common-dir "hierarchy.vhd") 982 1084 7344 13224 15718 34721 36257 36878 37309 37661 38873 40751 60098 60294 61027 61137 62271 65216 79521 98573 101108 104204 123511 123629 125243 125535 126135 131406 131681)
    (,(file-name-concat vhdl-ts-mode-test-files-common-dir "sexp.vhd") 892 963 1046 1063 1064 1079 1151 1163 1172 1173 1271 2223 4101 4155 4190 4224)))

(defconst vhdl-ts-mode-test-utils-instance-at-point-file-and-pos
  `((,(file-name-concat vhdl-ts-mode-test-files-common-dir "axi_if_converter.vhd") 168 196 9493 14266 14268 14269 15657 16769 16775 16776 19059 19989 19990 20243 25844 28958 30411 30601 30941 31912 32596 32596 33595 33596 33597 33600 33623)
    (,(file-name-concat vhdl-ts-mode-test-files-common-dir "instances.vhd") 1025 1437 1497 1500 1501 1510 1568 1635 1636 1637 1638 1682 1738 1767 1829 1951 1980 2120 2162 2120 2274 2316 2449 2539 2540 2541 2596 2681 2756 2757 2759 2780 2822 2830 2842 2873 2907 2935 2936 2937 2939 2961)))

(defconst vhdl-ts-mode-test-utils-block-at-point-file-and-pos
  `((,(file-name-concat vhdl-ts-mode-test-files-common-dir "sexp.vhd") 520 892 936 962 1035 1047 1048 1057 1057 1058 1079 1162 1174 1271 1336 1440 1474 1535 1582 1631 1889 2015 2023 2031 2040 2047 2067 2115 2146 2212 2245 2296 2306 2334 2364 2365 2387 2389 2417 2453 2514 2561 2610 2717 2771 2853 3028 3079 3086 3106 3140 3154 3216 3241 3252 3275 3335 3363 3381 3391 3393 3421 3442 3504 3515 3516 3533 3549 3570 3643 3649 3651 3671 3692 3777 3841 3864 3865 3886 3907 3992 4047 4053 4055 4101 4125 4155 4161 4163 4175 4190 4208 4224 4230)))

(defconst vhdl-ts-mode-test-utils-forward-sexp-file-and-pos
  `((,(file-name-concat vhdl-ts-mode-test-files-common-dir "sexp.vhd") 949 951 955 1064 1067 1070 1244 1256 1312 1321 1479 1486 1668 1670 1672 1705 1707 1709 1750 1752 1754 2059 2066 2076 2085 2287 2295 2390 2458 2647 2808 3115 3124 3406 3426 3534 3651 3658 3676 3685 3866 3891 4072 4126 4175 4209)))

(defconst vhdl-ts-mode-test-utils-backward-sexp-file-and-pos
  `((,(file-name-concat vhdl-ts-mode-test-files-common-dir "sexp.vhd") 1057 1051 1047 1166 1163 2381 2368 1472 1462 2031 2022 1989 1984 1981 1971 1951 1965 1945 1754 1750 2238 2230 2198 2188 2350 2341 3385 3077 3044 3039 2881 2876 3227 3224 3527 3514 3647 3858 3853 4040 4037 4051 4040 4119 4159 4202 4228)))


(defun vhdl-ts-mode-test-utils-block-at-point-fn ()
  (treesit-node-type (vhdl-ts-block-at-point)))

(defun vhdl-ts-mode-test-utils-instance-at-point-fn ()
  (let ((node (vhdl-ts-instance-at-point)))
    (when node
      `(,(vhdl-ts--node-identifier-name node)
        ,(vhdl-ts--node-instance-name node)))))

(defun vhdl-ts-mode-test-utils-entity-at-point-fn ()
  (let ((node (vhdl-ts-entity-at-point)))
    (when node
      (vhdl-ts--node-identifier-name node))))


(defun vhdl-ts-mode-test-utils-gen-expected-files ()
  ;; Forward sexp
  (dolist (file-and-pos vhdl-ts-mode-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ts-mode-test-ref-dir-utils
                                   :out-file-ext "fwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ts-forward-sexp
                                           :pos-list ,pos-list))))
  ;; Backward sexp
  (dolist (file-and-pos vhdl-ts-mode-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ts-mode-test-ref-dir-utils
                                   :out-file-ext "bwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ts-backward-sexp
                                           :pos-list ,pos-list))))
  ;; Block at point
  (dolist (file-and-pos vhdl-ts-mode-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ts-mode-test-ref-dir-utils
                                   :out-file-ext "block.at.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ts-mode-test-utils-block-at-point-fn
                                           :pos-list ,pos-list))))
  ;; Instance at point
  (dolist (file-and-pos vhdl-ts-mode-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ts-mode-test-ref-dir-utils
                                   :out-file-ext "inst.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ts-mode-test-utils-instance-at-point-fn
                                           :pos-list ,pos-list))))
  ;; Entity at point
  (dolist (file-and-pos vhdl-ts-mode-test-utils-entity-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ts-mode-test-ref-dir-utils
                                   :out-file-ext "ent.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ts-mode-test-utils-entity-at-point-fn
                                           :pos-list ,pos-list)))))


(ert-deftest vhdl-ts-mode::utils::block-at-point ()
  (dolist (file-and-pos vhdl-ts-mode-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-utils (test-hdl-basename file "block.at.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ts-mode-test-utils-block-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-utils (test-hdl-basename file "block.at.point.el")))))))

(ert-deftest vhdl-ts-mode::utils::instance-at-point ()
  (dolist (file-and-pos vhdl-ts-mode-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-utils (test-hdl-basename file "inst.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ts-mode-test-utils-instance-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-utils (test-hdl-basename file "inst.point.el")))))))


(ert-deftest vhdl-ts-mode::utils::entity-at-point ()
  (dolist (file-and-pos vhdl-ts-mode-test-utils-entity-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-utils (test-hdl-basename file "ent.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ts-mode-test-utils-entity-at-point-fn
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-utils (test-hdl-basename file "ent.point.el")))))))


(ert-deftest vhdl-ts-mode::utils::forward-sexp ()
  (dolist (file-and-pos vhdl-ts-mode-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-utils (test-hdl-basename file "fwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ts-forward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-utils (test-hdl-basename file "fwd.sexp.el")))))))


(ert-deftest vhdl-ts-mode::utils::backward-sexp ()
  (dolist (file-and-pos vhdl-ts-mode-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ts-mode-test-dump-dir-utils (test-hdl-basename file "bwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ts-backward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ts-mode-test-ref-dir-utils (test-hdl-basename file "bwd.sexp.el")))))))



(provide 'vhdl-ts-mode-test-utils)

;;; vhdl-ts-mode-test-utils.el ends here
