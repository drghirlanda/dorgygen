;; Load and run dorgygen tests.
;; From within Emacs: M-x load-file RET run-tests.el RET
;; From the shell:
;;   emacs --batch -l run-tests.el -f ert-run-tests-batch-and-exit

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir))

(require 'dorgygen)
(require 'dorgygen-python)
(require 'dorgygen-cpp)
(require 'dorgygen-tests)

(ert t)
