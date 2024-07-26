;;; -*- lexical-binding: t; -*-

;;---------------------------------------------------------------------
;; Path Functions
;;---------------------------------------------------------------------

(defun filepath:expand-maybe (dirpath &optional subdir)
  (if subdir (expand-file-name subdir dirpath) dirpath))

(defun filepath:assert-dir-exists (dirpath &optional subdir)
  "Throws error if dirpath does not exist. Optionally pass subdir to check if expanded path exists"
  (unless (file-directory-p dirpath)
    (user-error "`%s' is not a directory." dirpath))

  (let ((checkpath (filepath:expand-maybe dirpath subdir)))
    (unless (file-directory-p checkpath)
      (user-error "`%s' is not a directory." checkpath))
    ))

(defun filepath:ensure-dir-exists (dirpath &optional subdir)
  "Creates dirpath if it does not exist. Optionally pass subdir to create expanded path.
If subdir passed, dirpath must exist"
  (when
      (and subdir (not (file-directory-p dirpath)))
    (unless (file-directory-p dirpath)
      (user-error "`%s' is not a directory." dirpath)))
  
  (let ((newdir (filepath:expand-maybe dirpath subdir)))
    (unless (file-directory-p newdir)
      (make-directory newdir t)
      (message (format "Created directory: %s" newdir)))
      newdir
    ))

(defun filepath:expand-subdir (parentdir dirname &optional ensure)
  "Return expanded subdir with the given parent. Optionally ensure that the subdir exists"
  (filepath:assert-dir-exists parentdir)

  (let ((newdir (filepath:expand-maybe parentdir dirname)))
    (when ensure (filepath:ensure-dir-exists newdir))
    newdir))

(defun filepath:create-empty-file-maybe(filePath)
  "Create a file with FILEPATH parameter."
  ;; TODO need to test that dirpath exists
  
  (when (not (file-exists-p filePath)) 
    (with-temp-buffer (write-file filePath))))

;; adopted from https://github.com/mohkale/emacs (join-path+)
(defun filepath:join-paths (root path &rest rest)
  "Join a series of paths together.
ROOT is the initial path to join PATH and REST onto."
  (setq path (if (file-name-absolute-p path)
                 path
               (concat root
                       (unless (string-suffix-p "/" root)
                         "/")
                       path)))
  (if rest (apply 'filepath:join-paths path rest) path))

;; adopted from https://github.com/mohkale/emacs (dotemacs-initialise-path!)
(customize-set-variable 'filepath$filepath-function-allow-override t)
(defmacro filepath%functionalize-path (path-type path &optional ensure)
  "Initialise a constant for PATH-TYPE and functions to join a subdir or a file onto PATH.
Creates:
  Constant: filepath$$<PATH-TYPE>
  Function: <PATH-TYPE>*filepath(path rest args)"
  (setq path (eval path))
  (let* ((path-type-name (symbol-name path-type))
	 (ensure (if (not ensure) nil t))
         (constant-sym (intern (concat "filepath$$" path-type-name)))
         (func-sym (intern (concat path-type-name "*filepath")))
         (dired-func-sym (intern (concat path-type-name "*open-dired"))))
    `(progn
       ;; TODO Only define once unless filepath$filepath-function-allow-override
       ;;(if ()
	;;   )
       ;;(fboundp ,func-sym)
       
       (defconst ,constant-sym
         (eval-when-compile (expand-file-name ,path)))

       (or ,ensure (eval-when-compile
		    (filepath:ensure-dir-exists ,path)
		    ))
       
       (defun ,func-sym (&optional path &rest args)
         ,(concat "join args onto directory: " path)
         (if path (apply 'filepath:join-paths ,constant-sym path args)
	   ,constant-sym))

       (defun ,dired-func-sym ()
         ,(concat "open dired with directory: " path)
	 (interactive)
         (dired ,path))       
       )))

(defun filepaths:joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'filepaths:joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(provide 'lib/filepaths)
;;; filepaths.el ends here
