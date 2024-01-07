;;;; Perego Luca 894448
;;;; Magliani Andrea 894395 (capo supremo)
;;;; Picco Nicolas 894588

;; hash-table declaration & manipulation
(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
    (gethash name *classes-specs*))

;;class utils
(defun remove-class (name)
  (if (symbolp name)
      (remhash name *classes-specs*)
    (error "classname must be a symbol")))

(defun remove-all-classes ()
    (clrhash *classes-specs*))

(defun find-all (classname)
        (if (symbolp classname)
            (list (gethash classname *classes-specs*))
            (error "classname must be a symbol")))


;; def-class primitive
(defun def-class (classname &optional (parents '()) parts)
    ;; class-name type-check & existance
    (if (symbolp classname) 
        (if (not (gethash classname *classes-specs*))    
            ;; parents type-check
            (if (or (listp parents) (null parents))
                (if (is-class-list parents)
                    ;; parts type-check

                    (let ((newParts (concat-parts (get-all-parents-parts parents) parts)))
                      (print (get-all-parents-parts parents))
                       (if (parts-check newParts)
                        ;; create class and adds it to the hash-table
                        (progn
                            (add-class-spec classname (list 
                                :classname classname 
                                :parents parents 
                                :parts newParts
                            ))
                            (class-spec classname)) 
                        (error "parts must be a list of methods and fields")
                    
                    )
                    )

                    
                    (error "parents must be a list of existing classes")) 
                (error "parents must be a list of classes")) 
            (error "classname already exists")) 
        (error "classname must be a symbol")))

(defun is-class-list (parents)
    (if (null parents)
        t
        (and (is-class (car parents)) (is-class-list (cdr parents)))))

;; get-all-parents-parts
(defun get-all-parents-parts (classname) 
  (let ((class (class-spec classname)))
    (print(if class t nil))
        (if class
            (let ((parents (getf class :parents)))
                (if parents
                    (append (getf class :parts) 
                            (mapcan #'get-all-parents-parts parents))
                    (getf class :parts)))
           '()
        )
    )
)

;; concat parts
(defun concat-parts (parents-parts parts)
  (cond
    ((null parents-parts) parts)
    ((null parts) parents-parts)
    ((cons (car parents-parts) (car parts))
     (cons (concat-parts (cdr parents-parts) (cdr parts)) (car parts)))
    (t (cons (concat-parts (cdr parents-parts) (car parts)) (car parents-parts)))))




;; parts-check
(defun parts-check (parts)
 (if (and (listp parts) (eql (car parts) 'fields))
        (let ((fields (cdr parts)))
          (if (is-valid-fields fields)
            T
            (error "parts must be a list of methods and fields")
          )
        )
        (error "parts must be a list of methods and fields")
    )
)

(defun is-valid-fields (fields)
    (if (null fields)
        T
        (and (is-valid-field-structure (car fields)) (is-valid-fields (cdr fields)))
    )
)



;; field structure
(defun is-valid-field-structure (field)
  (if (= 3 (length field))
      (is-field (first field) (second field) (third field))
    (if (= 2 (length field))
        (is-field (first field) (second field) T) NIL)))

(defun is-field (field-name field-value &optional (field-type T))
  (if (symbolp field-name)
      (if (or (symbolp field-type) (eql field-type T))
          (if (or (typep field-value field-type) 
                  (eql field-type T))
              T
            (error "field-type is not valid or type mismatch"))
        (error "field-type must be a symbol"))
    (error "field-name must be a symbol")))



(defun fields (&rest field-specs)
  (if (null (car field-specs))
      '()
    (dolist (spec field-spec out)
      (let* (name (first spec))
        (value (second spec))
        (f-type (T)))

      (cond (not(null (cddr spec)))
            (setf f-type (car (cddr spec)))) 
      
      (push (field name value type) result))))


;; method structure
(defun is-method (method-name argslist form) T)



;; class control
(defun is-class (classname)
  (if (symbolp classname)
      (if (gethash classname *classes-specs*) T NIL)
    (error "class-name must be a symbol")))



