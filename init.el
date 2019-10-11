(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions.el")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper.el")
      nil (not init-file-debug))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disable file-name-handlers for a speed boost during init
  (let ((file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))

(defun new-post ()
  ;; prompt for post name
  ;; replace spaces with dashes
  ;; prepend today's date
  ;; add .md
  ;; open file buffer
  (interactive
   (let*  ((name (read-string "Post name: " nil 'my-history))
           (posts-dir "~/dot-asterisk/resources/templates/md/posts")
           (date (format-time-string "%Y-%m-%d")))
     ;; down case name for filename
     (find-file (concat posts-dir "/" (downcase (format "%s-%s.md" date (replace-regexp-in-string " +" "-" name)))))

     ;; enter meta map
     ;; {:title "name"
     ;;  :layout :post
     ;;  :tags [""]
     ;;  :toc true}
     (insert (format "{:title \"%s\"\n :layout :post\n :tags [\"\"]\n :toc false}\n\n" (capitalize name)))
     (insert "### Headline\n\nenter stuff here\n"))))

(defalias 'ecs 'find-file-other-window)
