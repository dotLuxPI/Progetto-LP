;;;; Perego Luca 894448

;; hash-table declaration & manipulation
(defvar *classes-specs* (make-hash-table :test 'equal))

(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec)
)

(defun class-spec (name)
    (gethash name *classes-specs*)
)

;; make-class primitive
(defun make-class (class-name &optional (parents '()) part)
    
    ;; class-name type-check & existance
    (if (stringp class-name) 
        (if (not (gethash class-name *classes-specs*))    

            ;; parents type-check
            (if (or (listp parents) (null parents))
                (if (every (or #'is-class #'null) parents)
                    
                    ;; part type-check
                    (if (listp part)
                        (if (every (or #'is-method #'is-field) part)

                            ;; create class and adds it to the hash-table
                            (progn
                                (add-class-spec class-name (list 
                                    :class-name class-name 
                                    :parents parents 
                                    :part part
                                ))
                                (class-spec class-name)
                            ) 
                            (error (format NIL "~a ~a"
                                "(make-class)"
                                "part must be a list of methods and fields"
                            ))
                        ) 
                        (error (format NIL "~a ~a"
                            "(make-class)"
                            "part must be a list of methods and fields"
                        ))
                    ) 
                    (error (format NIL "~a ~a"
                        "(make-class)"
                        "part must be a list of methods and fields"
                    ))
                ) 
                (error "(make-class) parents must be a list of classes")
            ) 
            (error "(make-class) parents must be a list of classes")
        ) 
        (error "(make-class) class-name already exists")
    ) 
    (error "(make-class) class-name must be a string")
)

;; make primitive
(defun make (class-name (cons field-name value))
    (if (stringp class-name)
        (if (is-class class-name)
            ;; code
            (error "(make) class-name must identify an existing class")
        )
        (error "(make) class-name must be a string")
    )
)

;; primitiva method
(defun method (method-name))

;; primitiva field
(defun field (instance field-name))

;; primitiva field* 
(defun field* (instance field-name))

;; primitiva fields
(defun fields (instance fie))

;;;; CONTROLLI

;; controllo classe
(defun is-class (class-name)
    (if (stringp class-name)
        (if (gethash class-name *classes-specs*)
            T 
            NIL
        )
        (error "(is-class) class-name must be a string")
    )
)

;; controllo istanza
(defun is-instance (value class-name))

;; controllo metodo
(defun is-method (method-name))

;; controllo field
(defun is-field (field-name))