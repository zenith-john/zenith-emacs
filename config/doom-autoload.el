;;; doom-autoload.el --- Temporary doom file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;;; From doom-emacs completion company mode
;;;###autoload
(defvar +company-backend-alist
  '((text-mode :derived (company-dabbrev company-yasnippet company-ispell))
    (prog-mode :derived (:separate company-capf company-yasnippet))
    (conf-mode :derived company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode

To have BACKENDS apply to any mode that is a parent of MODES, set MODES to
:derived, e.g.

  (set-company-backend! :derived 'text-mode 'company-dabbrev 'company-yasnippet)"
  (declare (indent defun))
  (let ((type :exact))
    (when (eq modes :derived)
      (setq type :derived
            modes (pop backends)))
    (dolist (mode (doom-enlist modes))
      (if (null (car backends))
          (setq +company-backend-alist
                (delq (assq mode +company-backend-alist)
                      +company-backend-alist))
        (setf (alist-get mode +company-backend-alist)
              (cons type backends))))))


;;
;;; Library

(defun +company--backends ()
  (append (cl-loop for (mode . rest) in +company-backend-alist
                   for type = (car rest)
                   for backends = (cdr rest)
                   if (or (and (eq type :derived) (derived-mode-p mode)) ; parent modes
                          (and (eq type :exact)
                               (or (eq major-mode mode)  ; major modes
                                   (and (boundp mode)
                                        (symbol-value mode))))) ; minor modes
                   append backends)
          (default-value 'company-backends)))


;;
;;; Hooks

;;;###autoload
(defun +company|init-backends ()
  "Set `company-backends' for the current buffer."
  (unless (eq major-mode 'fundamental-mode)
    (set (make-local-variable 'company-backends) (+company--backends)))
  (add-hook 'after-change-major-mode-hook #'+company|init-backends nil 'local))

(put '+company|init-backends 'permanent-local-hook t)


;;
;;; Commands

;;;###autoload
(defun +company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (and (company-manual-begin)
       (= company-candidates-length 1)))

;;;###autoload
(defun +company/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))

;;;###autoload
(defun +company/whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend '+company/whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (split-string
       (replace-regexp-in-string
        "^[\t\s]+" ""
        (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                (buffer-substring-no-properties (line-end-position) (point-max))))
       "\\(\r\n\\|[\n\r]\\)" t)))))

;;;###autoload
(defun +company/dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively #'company-complete)))

;;;###autoload
(defun +company/dabbrev-code-previous ()
  "TODO"
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'+company/dabbrev)
    (company-select-previous-or-abort)))
;; completion/ivy/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper search))

;;;###autoload (autoload '+ivy:todo "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:todo (&optional bang)
  "An ex wrapper around `+ivy/tasks'."
  (interactive "<!>")
  (+ivy/tasks bang))


;;
;; Project searching

;;;###autoload (autoload '+ivy:pt "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:pt (all-files-p query)
  "Ex interface for `+ivy/pt'"
  (interactive "<!><a>")
  (+ivy/pt all-files-p query))

;;;###autoload (autoload '+ivy:grep "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:grep (all-files-p query)
  "Ex interface for `+ivy/grep'"
  (interactive "<!><a>")
  (+ivy/grep all-files-p query))

;;;###autoload (autoload '+ivy:ag "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:ag (all-files-p query)
  "Ex interface for `+ivy/ag'"
  (interactive "<!><a>")
  (+ivy/ag all-files-p query))

;;;###autoload (autoload '+ivy:rg "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:rg (all-files-p query)
  "Ex interface for `+ivy/rg'"
  (interactive "<!><a>")
  (+ivy/rg all-files-p query))


;;;###autoload (autoload '+ivy:pt-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:pt-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/pt-from-cwd'."
  (interactive "<a><!>")
  (+ivy/pt-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:grep-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:grep-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/grep-from-cwd'."
  (interactive "<a><!>")
  (+ivy/grep-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:ag-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:ag-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/ag-from-cwd'."
  (interactive "<a><!>")
  (+ivy/ag-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:rg-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:rg-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/rg-from-cwd'."
  (interactive "<a><!>")
  (+ivy/rg-from-cwd (not recurse-p) query))

;;; completion/ivy/autoload/hydras.el -*- lexical-binding: t; -*-

;;;###autoload
(after! ivy-hydra
  (defhydra+ hydra-ivy (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("TAB" ivy-alt-done :exit nil)
    ("RET" ivy-done :exit t)
    ("C-SPC" ivy-call-and-recenter :exit nil)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("o" ivy-occur :exit t)))

;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

(defun +ivy--is-workspace-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (+workspace-contains-buffer-p buffer)))

(defun +ivy--is-workspace-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))

;;;###autoload
(defun +ivy-rich-buffer-name (candidate)
  "Display the buffer name.

Buffers that are considered unreal (see `doom-real-buffer-p') are dimmed with
`+ivy-buffer-unreal-face'."
  (let ((b (get-buffer candidate)))
    (cond ((ignore-errors
             (file-remote-p
              (buffer-local-value 'default-directory b)))
           (ivy-append-face candidate 'ivy-remote))
          ((doom-unreal-buffer-p b)
           (ivy-append-face candidate +ivy-buffer-unreal-face))
          ((not (buffer-file-name b))
           (ivy-append-face candidate 'ivy-subdir))
          ((buffer-modified-p b)
           (ivy-append-face candidate 'ivy-modified-buffer))
          (candidate))))

;;;###autoload
(defun +ivy-rich-buffer-icon (candidate)
  "Display the icon for CANDIDATE buffer."
  ;; NOTE This is inspired by `all-the-icons-ivy-buffer-transformer', minus the
  ;; buffer name and extra padding as those are handled by `ivy-rich'.
  (propertize "\t" 'display
              (if-let* ((buffer (get-buffer candidate))
                        (mode (buffer-local-value 'major-mode buffer)))
                  (or
                   (all-the-icons-ivy--icon-for-mode mode)
                   (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))
                   (funcall
                    all-the-icons-ivy-family-fallback-for-buffer
                    all-the-icons-ivy-name-fallback-for-buffer))
                (all-the-icons-icon-for-file candidate))))


;;
;; Library

(defun +ivy--switch-buffer-preview ()
  (let (ivy-use-virtual-buffers ivy--virtual-buffers)
    (counsel--switch-buffer-update-fn)))

(defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
(defalias '+ivy--switch-buffer-unwind      #'counsel--switch-buffer-unwind)

(defun +ivy--switch-buffer (workspace other)
  (let ((current (not other))
        prompt action filter update unwind)
    (cond ((and workspace current)
           (setq prompt "Switch to workspace buffer: "
                 action #'ivy--switch-buffer-action
                 filter #'+ivy--is-workspace-other-buffer-p))
          (workspace
           (setq prompt "Switch to workspace buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action
                 filter #'+ivy--is-workspace-buffer-p))
          (current
           (setq prompt "Switch to buffer: "
                 action #'ivy--switch-buffer-action))
          ((setq prompt "Switch to buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action)))
    (when +ivy-buffer-preview
      (cond ((not (and ivy-use-virtual-buffers
                       (eq +ivy-buffer-preview 'everything)))
             (setq update #'+ivy--switch-buffer-preview
                   unwind #'+ivy--switch-buffer-unwind))
            ((setq update #'+ivy--switch-buffer-preview-all
                   unwind #'+ivy--switch-buffer-unwind))))
    (ivy-read prompt 'internal-complete-buffer
              :action action
              :predicate filter
              :update-fn update
              :unwind unwind
              :preselect (buffer-name (other-buffer (current-buffer)))
              :matcher #'ivy--switch-buffer-matcher
              :keymap ivy-switch-buffer-map
              :caller #'+ivy--switch-buffer)))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.

If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;;;###autoload
(defun +ivy/switch-workspace-buffer-other-window ()
  "Switch another window to a buffer within the current workspace."
  (interactive)
  (+ivy--switch-buffer t t))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (+ivy--switch-buffer nil nil))

;;;###autoload
(defun +ivy/switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (+ivy--switch-buffer nil t))

(defun +ivy--tasks-candidates (tasks)
  "Generate a list of task tags (specified by `+ivy-task-tags') for
`+ivy/tasks'."
  (let* ((max-type-width
          (cl-loop for task in +ivy-task-tags maximize (length (car task))))
         (max-desc-width
          (cl-loop for task in tasks maximize (length (cl-cdadr task))))
         (max-width (max (- (frame-width) (1+ max-type-width) max-desc-width)
                         25)))
    (cl-loop
     with fmt = (format "%%-%ds %%-%ds%%s%%s:%%s" max-type-width max-width)
     for alist in tasks
     collect
     (let-alist alist
       (format fmt
               (propertize .type 'face (cdr (assoc .type +ivy-task-tags)))
               (substring .desc 0 (min max-desc-width (length .desc)))
               (propertize " | " 'face 'font-lock-comment-face)
               (propertize (abbreviate-file-name .file) 'face 'font-lock-keyword-face)
               (propertize .line 'face 'font-lock-constant-face))))))

(defun +ivy--tasks (target)
  (let* (case-fold-search
         (task-tags (mapcar #'car +ivy-task-tags))
         (cmd
          (format "%s -H -S --no-heading -- %s %s"
                  (or (when-let* ((bin (executable-find "rg")))
                        (concat bin " --line-number"))
                      (when-let* ((bin (executable-find "ag")))
                        (concat bin " --numbers"))
                      (error "ripgrep & the_silver_searcher are unavailable"))
                  (shell-quote-argument
                   (concat "\\s("
                           (string-join task-tags "|")
                           ")([\\s:]|\\([^)]+\\):?)"))
                  target)))
    (save-match-data
      (cl-loop with out = (shell-command-to-string cmd)
               for x in (and out (split-string out "\n" t))
               when (condition-case-unless-debug ex
                      (string-match
                       (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
                               (string-join task-tags "\\|")
                               "\\):?\\s-*\\(.+\\)")
                       x)
                      (error
                       (print! (red "Error matching task in file: (%s) %s")
                               (error-message-string ex)
                               (car (split-string x ":")))
                       nil))
               collect `((type . ,(match-string 3 x))
                         (desc . ,(match-string 4 x))
                         (file . ,(match-string 1 x))
                         (line . ,(match-string 2 x)))))))

(defun +ivy--tasks-open-action (x)
  "Jump to the file and line of the current task."
  (let ((location (cadr (split-string x " | ")))
        (type (car (split-string x " "))))
    (cl-destructuring-bind (file line) (split-string location ":")
      (with-ivy-window
        (find-file (expand-file-name file (doom-project-root)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (search-forward type (line-end-position) t)
        (backward-char (length type))
        (recenter)))))

;;;###autoload
(defun +ivy/tasks (&optional arg)
  "Search through all TODO/FIXME tags in the current project. If ARG, only
search current file. See `+ivy-task-tags' to customize what this searches for."
  (interactive "P")
  (ivy-read (format "Tasks (%s): "
                    (if arg
                        (concat "in: " (file-relative-name buffer-file-name))
                      "project"))
            (let ((tasks (+ivy--tasks (if arg buffer-file-name (doom-project-root)))))
              (unless tasks
                (user-error "No tasks in your project! Good job!"))
              (+ivy--tasks-candidates tasks))
            :action #'+ivy--tasks-open-action
            :caller '+ivy/tasks))

;;;###autoload
(defun +ivy/wgrep-occur ()
  "Invoke the search+replace wgrep buffer on the current ag/rg search results."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let* ((caller (ivy-state-caller ivy-last))
         (occur-fn (plist-get ivy--occurs-list caller))
         (buffer
          (generate-new-buffer
           (format "*ivy-occur%s \"%s\"*"
                   (if caller (concat " " (prin1-to-string caller)) "")
                   ivy-text))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall occur-fn))
      (setf (ivy-state-text ivy-last) ivy-text)
      (setq ivy-occur-last ivy-last)
      (setq-local ivy--directory ivy--directory))
    (ivy-exit-with-action
     `(lambda (_)
        (pop-to-buffer ,buffer)
        (ivy-wgrep-change-to-wgrep-mode)))))

;;;###autoload
(defun +ivy-yas-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

;;;###autoload
(defun +ivy-git-grep-other-window-action (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (select-window
     (with-ivy-window
       (let ((file-name   (match-string-no-properties 1 x))
             (line-number (match-string-no-properties 2 x)))
         (find-file-other-window (expand-file-name file-name (ivy-state-directory ivy-last)))
         (goto-char (point-min))
         (forward-line (1- (string-to-number line-number)))
         (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
         (run-hooks 'counsel-grep-post-action-hook)
         (selected-window))))))

;;;###autoload
(defun +ivy-confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))


;;
;; File searching

;;;###autoload
(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME, `counsel-file-jump' if invoked from a
non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  (call-interactively
   (cond ((or (file-equal-p default-directory "~")
              (when-let* ((proot (doom-project-root)))
                (file-equal-p proot "~")))
          #'counsel-find-file)

         ((doom-project-p)
          (let ((files (projectile-current-project-files)))
            (if (<= (length files) ivy-sort-max-size)
                #'counsel-projectile-find-file
              #'projectile-find-file)))

         (#'counsel-file-jump))))

;;;###autoload
(cl-defun +ivy-file-search (engine &key query in all-files (recursive t))
  "Conduct a file search using ENGINE, which can be any of: rg, ag, pt, and
grep. If omitted, ENGINE will default to the first one it detects, in that
order.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (let* ((project-root (or (doom-project-root) default-directory))
         (directory (or in project-root))
         (default-directory directory)
         (engine (or engine
                     (cl-loop for tool in +ivy-project-search-engines
                              if (executable-find (symbol-name tool))
                              return tool)
                     (and (or (executable-find "grep")
                              (executable-find "git"))
                          'grep)
                     (error "No search engine specified (is ag, rg, pt or git installed?)")))
         (query
          (or (if query (rxt-quote-pcre query))
              (when (use-region-p)
                (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                      (end (or (bound-and-true-p evil-visual-end) (region-end))))
                  (when (> (abs (- end beg)) 1)
                    (rxt-quote-pcre (buffer-substring-no-properties beg end)))))))
         (prompt
          (format "%s%%s %s"
                  (symbol-name engine)
                  (cond ((equal directory default-directory)
                         "./")
                        ((equal directory project-root)
                         (projectile-project-name))
                        ((file-relative-name directory project-root))))))
    (require 'counsel)
    (let ((ivy-more-chars-alist
           (if query '((t . 1)) ivy-more-chars-alist)))
      (pcase engine
        ('grep
         (let ((args (if recursive " -R"))
               (counsel-projectile-grep-initial-input query))
           (if all-files
               (cl-letf (((symbol-function #'projectile-ignored-directories-rel)
                          (symbol-function #'ignore))
                         ((symbol-function #'projectile-ignored-files-rel)
                          (symbol-function #'ignore)))
                 (counsel-projectile-grep args))
             (counsel-projectile-grep args))))
        ('ag
         (let ((args (concat (if all-files " -a")
                             (unless recursive " --depth 1"))))
           (counsel-ag query directory args (format prompt args))))
        ('rg
         (let ((args (concat (if all-files " -uu")
                             (unless recursive " --maxdepth 1"))))
           (counsel-rg query directory args (format prompt args))))
        ('pt
         (let ((counsel-pt-base-command
                (concat counsel-pt-base-command
                        (if all-files " -U")
                        (unless recursive " --depth=1")))
               (default-directory directory))
           (counsel-pt query)))
        (_ (error "No search engine specified"))))))

(defun +ivy--get-command (format)
  (cl-loop for tool in (cl-remove-duplicates +ivy-project-search-engines :from-end t)
           if (executable-find (symbol-name tool))
           return (intern (format format tool))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a project search from the project root.

Uses the first available search backend from `+ivy-project-search-engines'. If
ARG (universal argument), include all files, even hidden or compressed ones, in
the search."
  (interactive "P")
  (funcall (or (+ivy--get-command "+ivy/%s")
               #'+ivy/grep)
           arg
           initial-query
           directory))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

Uses the first available search backend from `+ivy-project-search-engines'. If
ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (funcall (or (+ivy--get-command "+ivy/%s-from-cwd")
               #'+ivy/grep-from-cwd)
           arg
           initial-query))


;;;###autoload (autoload '+ivy/rg "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/rg-from-cwd "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/ag "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/ag-from-cwd "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/pt "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/pt-from-cwd "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/grep "completion/ivy/autoload/ivy")
;;;###autoload (autoload '+ivy/grep-from-cwd "completion/ivy/autoload/ivy")

(dolist (engine `(,@(cl-remove-duplicates +ivy-project-search-engines :from-end t) grep))
  (defalias (intern (format "+ivy/%s" engine))
    (lambda (all-files-p &optional query directory)
      (interactive "P")
      (+ivy-file-search engine :query query :in directory :all-files all-files-p))
    (format "Perform a project file search using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine))

  (defalias (intern (format "+ivy/%s-from-cwd" engine))
    (lambda (all-files-p &optional query)
      (interactive "P")
      (+ivy-file-search engine :query query :in default-directory :all-files all-files-p))
    (format "Perform a project file search from the current directory using %s.

QUERY is a regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, search compressed and hidden files as well."
            engine)))

;;; completion/ivy/autoload/posframe.el -*- lexical-binding: t; -*-
;;;###if (featurep! +childframe)

;;;###autoload
(defun +ivy-display-at-frame-center-near-bottom (str)
  "TODO"
  (ivy-posframe--display str #'+ivy-poshandler-frame-center-near-bottom))

;;;###autoload
(defun +ivy-poshandler-frame-center-near-bottom (info)
  "TODO"
  (let ((parent-frame (plist-get info :parent-frame))
        (pos (posframe-poshandler-frame-center info)))
    (cons (car pos)
          (truncate (/ (frame-pixel-height parent-frame) 2)))))

(provide 'doom-autoload)
;;; doom-autoload.el ends here
