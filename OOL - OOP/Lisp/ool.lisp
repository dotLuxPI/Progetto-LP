;;;; Perego Luca 894448

;; hash-table declaration & manipulation
(defvar *classes-specs* (make-hash-table :test 'equal))

(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec)
)

(defun class-spec (name)
    (gethash name *classes-specs*)
)

;; def-class primitive
(defun def-class (classname parts) def-class(classname '() parts))
(defun def-class (classname parents parts)
    
    ;; class-name type-check & existance
    (if (stringp classname) 
        (if (not (gethash classname *classes-specs*))    

            ;; parents type-check
            (if (or (listp parents) (null parents))
                (if (every (or #'is-class #'null) parents)
                    
                    ;; parts type-check
                    (if (listp parts)

                            ;; create class and adds it to the hash-table
                            (progn
                                (let newParts (parts-check parts))

                                (add-class-spec classname (list 
                                    :classname classname 
                                    :parents parents 
                                    :parts newParts
                                ))

                                (class-spec classname)) 
                        (error "parts must be a list of methods and fields"))
                    (error "parents must be a list of existing classes")) 
                (error "parents must be a list of classes")) 
            (error "classname already exists")) 
        (error "classname must be a string")))

;; parts-check
(defun parts-check (parts)
    (if (listp parts)
        (if (every (or #'method #'field) parts)
            (concat-parts parts)
            (error "parts must be a list of methods and fields")
        )
        (error "parts must be a list of methods and fields")
    )
)

;; field structure
(defun field (field-name field-value) field(field-name field-value T))
(defun field (field-name field-value field-type)
    (if (stringp field-name)
        (if (or (stringp field-type) (eql field-type T))
            (if (or (typep field-value (intern field-type)) (eql field-type T))
                (list
                    :field-name field-name 
                    :field-value field-value	
                    :field-type field-type
                )
                (error "field-type is not valid or type mismatch")
            )
            (error "field-type must be a string")
        )
        (error "field-name must be a string")
    )
)

;; method structure
(defun method (method-name argslist form) T)



;; controllo classe
(defun is-class (classname)
    (if (stringp classname)
        (if (gethash classname *classes-specs*)
            T 
            NIL
        )
        (error "class-name must be a string")
    )
)