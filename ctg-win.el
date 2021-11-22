;;; ctg-win.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities to manipulate windows

;;; Code:

(require 'ivy)
(require 'ht)

(defvar ctg-win--window-configs (ht-create)
  "Saved window configs")

(defsubst ctg-win--create-winconf (name conf pt)
  (ht<-plist (list :name name :conf conf :point pt)))

;;;###autoload
(defun ctg-win/save-window-state ()
  "Save the window state with a name"
  (interactive)
  (let ((name (read-string "Enter config name: "))
        (conf (current-window-configuration))
        (pt (point-marker)))
    (ht-set! ctg-win--window-configs name (ctg-win--create-winconf name conf pt))))

(defun ctg-win--do-restore (name)
  (when-let ((m (ht-get ctg-win--window-configs name)))
    (let ((conf (ht-get m :conf))
          (pt (ht-get m :pt)))
      (set-window-configuration conf)
      (goto-char pt))))

(defun ctg-win/restore-window-state ()
  "Restore the window state to a previously saved one.

The window state will be restored to a previous state saved
with a call to [ctg-win/save-window-state]"
  (interactive)
  (ivy-read "Select the window configuration: "
            (ht-map (lambda (k v) (format "%s" k)) ctg-win--window-configs)
            :action #'ctg-win--do-restore))

(defun ctg-toggle-window-split ()
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

