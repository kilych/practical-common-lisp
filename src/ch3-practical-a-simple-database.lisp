;;; 3.1 CDs and Records

(defun make-cd (title artist since-the-year localp)
  ;; property list
  (list :title title
        :artist artist
        :since-the-year since-the-year
        :localp localp))

;;; 3.2 Filing CDs

(defvar *db* nil)

(defun add-record (record) (push record *db*))

;;; 3.3 Looking at the Database Contents

(defun dumb-db ()
  (dolist (record *db*)
    (format t "~{~a:~16t~a~%~}~%" record)))

;;; 3.4 Improving the User Interaction

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Since the year") :junk-allowed t)
       0)
   (yes-or-no-p "Local?")))

;; add is bad name for user interaction
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (unless (yes-or-no-p "Another?") (return))))

;;; 3.5 Saving and Loading the Database

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;; 3.6 Querying the Database

(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd)
                     (equal (getf cd :artist) artist))
                 *db*))

;; generalization of select
(defun select (selector)
  (remove-if-not selector *db*))

;; selector maker-example
(defun artist-selector (artist)
  #'(lambda (cd)
      (equal (getf cd :artist) artist)))

;; generalization of selector-maker
(defun my-where (&key title artist since-the-year (localp nil localp-p))
  #'(lambda (cd)
      (and (or (null title)
               (equal (getf cd :title) title))
           (or (null artist)
               (equal (getf cd :artist) artist))
           (or (null since-the-year)
               (equal (getf cd :since-the-year) since-the-year))
           (or (null localp-p)
               (equal (getf cd :localp) localp)))))

;; Seibel's version
(defun where (&key title artist since-the-year (localp nil localp-p))
  #'(lambda (cd)
      (and (if title (equal (getf cd :title) title) t)
           (if artist (equal (getf cd :artist) artist) t)
           (if since-the-year (equal (getf cd :since-the-year) since-the-year) t)
           (if localp-p (equal (getf cd :localp) localp) t))))

;;; 3.7 Updating Existing Records — Another Use for WHERE

;; the new list created by remove-if-not seems to share data with the old one
(defun my-update (selector &key title artist since-the-year (localp nil localp-p))
  (let ((filtered (remove-if-not selector *db*)))
    (dolist (cd filtered)
      (when title (setf (getf cd :title) title))
      (when artist (setf (getf cd :artist) artist))
      (when since-the-year (setf (getf cd :since-the-year) since-the-year))
      (when localp-p (setf (getf cd :localp) localp)))
    filtered))

;; Seibel's version
(defun update (selector &key title artist since-the-year (localp nil localp-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if since-the-year (setf (getf row :since-the-year) since-the-year))
               (if localp-p (setf (getf row :localp) localp)))
             row)
         *db*)))

(defun delete-rows (selector)
  (setf *db* (remove-if selector *db*)))

;;; 3.8 Removing Duplication and Winning Big

;; Step 1 to where macro
;; (equal (getf row field) value)

;; Step 2
(defun make-comprasion-expr (field value)
  (list equal (list getf row field) value))

;; Step 3
(defun make-comprasion-expr (field value)
  (list 'equal (list 'getf 'row field) value))

;; Step 4: backtick
(defun make-comprasion-expr (field value)
  `(equal (getf row ,field) ,value))

(defun make-comprasions-list (fields)
  (loop while fields
     collecting (make-comprasion-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (row) (and ,@(make-comprasions-list clauses))))

;; OPTIONAL:

(defun make-update-expr (field value)
  `(setf (getf row ,field) ,value))

(defun make-updates-list (fields)
  (loop while fields
     collecting (make-update-expr (pop fields) (pop fields))))

(defmacro update (selector &rest clauses)
  `(let ((selected (remove-if-not ,selector *db*)))
     (dolist (row selected)
       ,@(make-updates-list clauses))
     selected))
