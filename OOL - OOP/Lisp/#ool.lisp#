;;;; Perego Luca 894448
;;;; Magliani Andrea
;;;; Picco Nicolas

(setq *handle-warn-on-redefinition* :IGNORE)

;; hash-table declaration & manipulation
(defvar *classes-specs* (make-hash-table :test 'equal))

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
                (if (every #'is-class parents)

                    ;; parts type-check
                    (if (parts-check parts)

                        ;; create class and adds it to the hash-table
                        (progn

                            (add-class-spec classname (list 
                                :classname classname 
                                :parents parents 
                                :parts parts
                            ))

                            (class-spec classname)) 
                        (error "parts must be a list of methods and fields")
                    )
                    (error "parents must be a list of existing classes")) 
                (error "parents must be a list of classes")) 
            (error "classname already exists")) 
        (error "classname must be a symbol")))

;; parts-check
(defun parts-check (parts)
    (if (listp parts)
        (if (every #'is-field parts)
            T
            (error "parts must be a list of methods and fields")
        )
        (error "parts must be a list of methods and fields")
    )
)



;; field structure
(defun is-field (field-name field-value &optional (field-type T))
    (if (symbolp field-name)
        (if (or (symbolp field-type) (eql field-type T))
            (if (or (typep field-value field-type) (eql field-type T))
                T
                (error "field-type is not valid or type mismatch")
            )
            (error "field-type must be a symbol")
        )
        (error "field-name must be a symbol")
    )
)

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