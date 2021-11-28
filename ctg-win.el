;; ctg-wn.el --- Window configuration manipulation -*- lexical-binding: t; -*-
;;
;; Author: Pedro Abelleira Seco <coutego@gmail.com>
;; URL: https://github.com/coutego/ctg-win
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (ivy "0.13.4") (ht "2.4"))
;; Keywords: frames
;;; Commentary:
;; Utilities to manipulate windows

;;; Code:

(require 'ivy)
(require 'ht)

(defvar ctg-win--window-configs (ht-create)
  "Saved window configs.")

(defun ctg-win--current-context ()
  "Return the current context to take into account for the window configuration."
  (if (require 'persp-mode nil t)
      (->> (get-current-persp)
           (persp-name))
    "root"))

(defun ctg-win--current-conf-map ()
  "Return tha map associated with the current context."
  (let* ((ctx (ctg-win--current-context))
         (m (ht-get ctg-win--window-configs ctx)))
    (unless m
      (setq m (ht-create))
      (ht-set ctg-win--window-configs ctx m))
    m))

(defun ctg-win--wc-add (name)
  "Add the current window configuration to the saved configurations with the given NAME."
  (let ((conf (current-window-configuration))
        (pt (point-marker)))
    (ht-set! (ctg-win--current-conf-map) name (ctg-win--create-winconf name conf pt))))

(defun ctg-win--wc-kill (name)
  "Delete the configuration with NAME, if it exists, silently returning if it doesn't exist."
  (ht-remove (ctg-win--current-conf-map) name))

(defun ctg-win--wc-restore (name)
  "Restore the window configuration with NAME, no-op if if doesn't exist."
  (when-let ((m (ht-get (ctg-win--current-conf-map) name)))
    (let ((conf (ht-get m :conf))
          (pt (ht-get m :pt)))
      (set-window-configuration conf)
      (goto-char pt))))

(defun ctg-win--wc-all ()
  "Return all the names for window configurations in the current context."
  (ht-keys (ctg-win--current-conf-map)))

(defun ctg-win--create-winconf (name conf pt)
  "Create a window configuration structure with the given NAME, CONF and PT."
  (ht<-plist (list :name name :conf conf :point pt)))

(defun ctg-win--create-config-name ()
  "Create a default name for the current window configuration."
  (->> (window-list)
       (-map #'window-buffer)
       (-map #'buffer-name)
       (--reduce (concat acc " | " it))))

(defun ctg-win--read-config-name ()
  "Prompt the user for a config name, proposing a default one."
  (ivy-read "Window configuration identifier: "
            (list (ctg-win--create-config-name))))

;;;###autoload
(defun ctg-win-save-window-state ()
  "Save the window state with a name."
  (interactive)
  (let ((name (ctg-win--read-config-name)))
    (ctg-win--wc-add name)))

(defun ctg-win-restore-window-state ()
  "Restore the window state to a previously saved one.

The window state will be restored to a previous state saved
with a call to [ctg-win/save-window-state]"
  (interactive)
  (ivy-read "Select the window configuration: "
            (ht-map (lambda (k _v) (format "%s" k)) (ctg-win--current-conf-map))
            :action #'ctg-win--wc-restore))

(defun ctg-win-delete-window-state ()
  "Delete a window state."
  (interactive)
  (ivy-read "Select the window configuration to delete: "
            (ht-map (lambda (k _v) (format "%s" k)) (ctg-win--current-conf-map))
            :action #'ctg-win--wc-kill))

(defun ctg-win-toggle-window-split ()
  "Change the orientation of a two window split.

If the windows are stacked horizontally, stack them vertically and viceversa.
Based on code borrowed from a StakOverflow answer."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges) (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'ctg-win)

;;; ctg-win.el ends here
