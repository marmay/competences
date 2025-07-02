;;; haskell_dev_env.el --- Some useful functions for running the project.
;;; Commentary:
;;; This file contains functions to setup and manage ghcid for different Haskell projects.

(defvar bottom-window nil
  "Variable to hold the bottom window object.")
(defvar pname-exe-frontend "competences-frontend:exe:competences-frontend")
(defvar pname-exe-backend "competences-backend:exe:competences-backend")
(defvar pname-test-frontend "competences-frontend:test:competences-frontend-test")
(defvar pname-test-backend "competences-backend:test:competences-backend-test")
(defvar pname-test-common "competences-common:test:competences-common-test")

;;; Code:
(defun setup-bottom-window ()
  "Setup the bottom window for displaying special buffers."
  (delete-other-windows)
  (split-window-below)
  (setq bottom-window (next-window))
  (switch-to-buffer "*scratch*")
  (other-window 1)
  (tab-line-mode t)
  )

(defun display-in-bottom-window (buffer-name)
  "Display the buffer named BUFFER-NAME in the bottom window."
  (save-selected-window
    (select-window bottom-window)
    (switch-to-buffer buffer-name)
    (tab-line-mode t)
  ))

;; Function to setup ghcid for a given project

(defun setup-ghcid (project-name project-src)
  "Setup ghcid for PROJECT-NAME and run MAIN-FUNCTION."
  (interactive)
  (require 'term)
  (let ((buffer-name (concat "ghcid-" project-name "")))
    (if (get-buffer buffer-name)
        (display-in-bottom-window buffer-name)
      ;; (let ((buffer (get-buffer-create buffer-name)))
      ;;   (with-current-buffer buffer
      ;;     (let ((process (start-process "ghcid" buffer "ghcid" "--command" (concat "cabal repl " project-name) "--test" "main")))
      ;;       (set-process-filter process (lambda (proc output) (with-current-buffer (process-buffer proc) (insert output))))
      ;;       (comint-mode)
      ;;       (setq comint-process-echoes t)))
      ;;   (display-in-bottom-window buffer-name)
      ;; 	))))
      (let* ((cmd "ghciwatch")
             (h (- (window-height) scroll-margin 3))
             (term-buffer-maximum-size h)
             (args (format "--command \"cabal repl %s\" --watch %s --test-ghci main" project-name project-src))
             (switches (split-string-and-unquote args))
             (termbuf (apply 'make-term buffer-name cmd nil switches)))
        (set-buffer termbuf)
        (term-mode)
        (term-char-mode)
        (compilation-minor-mode)
	(display-in-bottom-window termbuf)))))

;; Function to kill ghcid for a given project
(defun kill-ghcid (project-name)
  "Kill ghcid for PROJECT-NAME."
  (let ((buffer-name (concat "*ghcid-" project-name "*")))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))))

;; Function to setup frontend and backend buffers
(defun setup-projects ()
  "Setup buffers for running frontend and backend projects."
  (interactive)
  (stop-projects)
  (stop-tests)
  (setup-bottom-window)
  (setup-ghcid pname-exe-frontend "frontend")
  (setup-ghcid pname-exe-backend "backend"))

;; Function to stop both projects
(defun stop-projects ()
  "Stop both frontend and backend projects."
  (interactive)
  (kill-ghcid pname-exe-frontend)
  (kill-ghcid pname-exe-backend))

;; Function to setup test buffers
(defun setup-tests ()
  "Setup buffers for running tests for all three projects."
  (interactive)
  (stop-projects)
  (setup-bottom-window)
  (setup-ghcid pname-test-frontend "frontend")
  (setup-ghcid pname-test-backend "backend")
  (setup-ghcid pname-test-common "common"))

;; Function to stop tests
(defun stop-tests ()
  "Stop the test buffers."
  (interactive)
  (kill-ghcid pname-test-frontend)
  (kill-ghcid pname-test-backend)
  (kill-ghcid pname-test-common))

;; Keybindings for easy access
(global-set-key (kbd "C-c p") 'setup-projects)
(global-set-key (kbd "C-c s a") 'stop-projects)
(global-set-key (kbd "C-c t") 'setup-tests)
(global-set-key (kbd "C-c s t") 'stop-tests)

(provide 'haskell_dev_env)
;;; haskell_dev_env.el ends here
