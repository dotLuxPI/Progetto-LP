;;;; Perego Luca 894448
;;;; Magliani Andrea 894395
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



;;;; MEMO!!!!
;; def-class deve restituire il class-name in caso di successo
;; attualmente restituisce l'intera classe

;; DEF_CLASS
(defun def-class (classname &optional (parents '()) &rest parts)
  (if (symbolp classname) 
      (if (not (gethash classname *classes-specs*))    
          (if (or (listp parents) (null parents))
              (if (is-class-list parents)
                  (let* ((class-fields (concat-all 'fields parts))
                         (class-methods (concat-all 'methods parts)))
                    (if (parts-check class-fields class-methods)
                        (let* ((final-fields 
                               (fields-validation 
                                (get-all-fields parents class-fields)
                                NIL))
                               (final-methods class-methods))
                          (progn
                            (add-class-spec 
                             classname (list 
                                        :classname classname 
                                        :parents parents 
                                        :parts (append 
                                                (list 'FIELDS 
                                                      final-fields) 
                                                (list 'METHODS 
                                                      final-methods))))
                            (class-spec classname)))
                      (error "parts must be a list of methods and fields")))
                (error "parents must be a list of existing classes")) 
            (error "parents must be a list of classes")) 
        (error "classname already exists")) 
    (error "classname must be a symbol")))

;; def-class utils 
(defun is-class-list (parents)
  (if (null parents)
      t
    (and (is-class (car parents)) (is-class-list (cdr parents)))))

(defun to-list (x)
  (if (listp x) x (list x)))

(defun concat-all (delimiter parts)
  (if (null parts)
      NIL
    (if (eql delimiter (first (first parts)))
        (append 
         (rest (first parts)) 
         (concat-all delimiter (rest parts))) ;;concat
      (concat-all delimiter (rest parts))))) ;; continue

;; parts-check
(defun parts-check (fields methods)
  (if (and (listp fields) (listp methods)) ;; list or null
      (if (is-valid-fields fields)
          (if (is-valid-methods methods)
              T NIL))))

(defun get-fields (parts)
  (let* ((start (position 'FIELDS parts :test #'eq))
         (end (position 'METHODS parts :test #'eq)))
    (if (null start)
        nil
      (if (null end)
          (rest parts)
        (subseq parts (1+ start) end)))))

(defun get-methods (parts)
  (let* ((start (position 'METHODS parts :test #'eq))
         (end (length parts)))
    (if (null start)
        nil
      (if (null end)
          (rest parts)
        (subseq parts (1+ start) end)))))



;; FIELDS
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



;; fields utils
(defun concat-fields (parts parents-parts)
  (if (null parts)
      parents-parts
    (let ((part (car parts))
          (rest-parts (cdr parts)))
      (if (assoc (car part) parents-parts :test #'equal)
          (cons part (concat-fields rest-parts 
                                    (remove 
                                     (assoc (car part) 
                                            parents-parts 
                                            :test #'equal) 
                                     parents-parts)))
        (cons part
              (concat-fields rest-parts parents-parts))))))

(defun fields-validation (parts parents-parts)
  (let* ((fields parts)
         (parents-fields parents-parts))
    (if (null fields)
        '()
      (cons (fill-in-field (car fields) parents-fields) 
            (fields-validation (rest fields) parents-fields)))))

;; adds missing field parts and normalize field to 3-parameter-form
(defun fill-in-field (part parents-fields) 

  (cond
   ((or (= 1 (length part))
        (and (= 2 (length part)) 
             (eql nil (second part)))
        (and (= 3 (length part)) 
             (eql nil (second part)) (eql t (third part))))
    (compatibility-check 
     (fill-type-field 
      (fill-value-field part parents-fields) 
      parents-fields) 
     parents-fields))
   
   ((or (= 2 (length part)) 
        (eql (third part) t)) 
    (compatibility-check 
     (fill-type-field part parents-fields) 
     parents-fields))
   
   ((= 3 (length part)) 
    (compatibility-check 
     part 
     parents-fields))))

;; add missing value to field if available to inherit
(defun fill-value-field (part parents-fields)

  (if (null parents-fields)
      (list (first part) 
            nil) ;; normalize field length by adding value
    (let ((parent-part (first parents-fields)))
      (if (eql (first part) (first parent-part)) ;; if same field-name
          (list (first part) 
                (second parent-part)) ;; normalize field length by adding value
        (fill-value-field part (rest parents-fields)))))) ;; continue

;; add missing type to field if available to inherit
(defun fill-type-field (part parents-fields)
  (if (null parents-fields) ;; base
      (list (first part) 
            (second part) 
            t) ;; normalize field length by adding type
    (let ((parent-part (first parents-fields)))
      (if (eql (first part) (first parent-part)) ;; if same field-name
          (if (third parent-part) ;; if parent has field-type
              (if (or (typep (second part) 
                             (third parent-part)) ;; if type or subtype
                      (subtypep (type-of (second part)) (third parent-part)))
                  (if (= 2 (length part)) ;; add type to field
                      (list (first part) (second part) (third parent-part))
                    part)
                (if (= 2 (length part)) ;; normalize field length to 3
                    (list (first part) (second part) t)  
                  part))
            (if (= 2 (length part)) ;; normalize field length to 3
                (list (first part) (second part) t)
              part))
        (fill-type-field part (rest parents-fields))))))

;; check compatibility between part type and inherited parts type
(defun compatibility-check (part parts-list) ;; part is !nil and !t
  (if (null parts-list) ;; base
      part ;; return true
    (let ((class-part (first parts-list))) ;; else
      
      (if (eql (first part) (first class-part)) ;; same name -> type check
          (if (null (second class-part))
              part
             
              (if (or (null (third class-part)) ;; if class-part true/nil
                      (eql (third class-part) t))
                  (if (typep (second class-part) 
                             (third part)) ;; check value type
                      part 
                    (error "Type mismatch for field ~A. ~%~A vs ~A~%"
                           (first part) 
                           (third part) 
                           (type-of (second class-part))))
                (if (eql (third part) (third class-part))
                    part
                  (error "Type mismatch for field ~A: ~A vs ~A"
                         (first part) (third part) (third class-part)))))
              (compatibility-check part (rest parts-list)))))) ;; continue

(defun get-all-fields (parents parts)
  (if parents
      (if (is-class (car parents))
          (let ((parent-fields 
                 (first (get-fields 
                        (getf (class-spec (car parents)) 
                              :parts)))))
            (let ((temp-parts (remove-duplicates 
                               (append (get-all-fields 
                                        (rest parents) parts)
                                       (fields-validation parts parent-fields))
                               :test #'equal :key #'car)))
                               (let ((final-parts (concat-fields 
                                                   temp-parts parent-fields)))
             final-parts)))
          (error "~A is not an existing class" (car parents)))
      parts)) ;; aggiungere il compatibility check se da errori strani

;; field structure check
(defun is-valid-fields (fields)
  (if (null fields)
      T 
    (and (is-valid-field-structure (car fields)) 
         (is-valid-fields (cdr fields)))))

;; field structure
(defun is-valid-field-structure (field)
  (if (= 3 (length field)) ;; (name value type)
      (is-field (first field) (second field) (third field))
    (if (= 2 (length field)) ;; (name value)
        (is-field (first field) (second field) T) 
      (if (= 1 (length field)) ;; (name)
          (is-field (first field) nil T) nil))))

(defun is-field (field-name field-value &optional (field-type T))
  (if (symbolp field-name)
      (if (or (symbolp field-type) (eql field-type T))
          (if (or (typep field-value field-type) 
                  (eql field-type T))
              T
            (error "field-type is not valid or type mismatch"))
        (error "field-type must be a symbol"))
    (error "field-name must be a symbol")))


;; METHODS
(defun methods (name argslist form) 
  t)


;; methods utils
(defun is-valid-methods (methods) 
  t)



;; MAKE
(defun make (class-name &rest fields)
  (if (is-class class-name)
      (if (null fields)
          (make-default-instance class-name)
        (make-custom-instance class-name fields))
    (error "~A is not a valid class-name" class-name)))

;; make utils
(defun make-default-instance (class-name)
  (let* ((class-parts (getf (class-spec class-name) :PARTS))
         (fields (get-fields class-parts)))
      (list :class class-name :fields (inherit-fields '() (first fields)))))

(defun make-custom-instance (class-name fields &optional (result nil))
    (let* ((class-parts (getf (class-spec class-name) :PARTS))
         (class-fields (get-fields class-parts)))

    (if (null fields)
        (list :class class-name 
              :fields (inherit-fields result (first class-fields)))
      (let* ((name (first fields))
             (value (second fields)))
        (if (is-valid-instance-value (first class-fields) name value)
            (make-custom-instance class-name 
                                  (cddr fields) 
                                  (append result (list name value)))
          (error "generic invalid-instance error"))))))

(defun is-valid-instance-value (class-fields name value)  
  (if (null class-fields)
      (error "Unknown field: (~A ~A)" name value)
    (let* ((name-class (first (first class-fields)))
           (value-class (second (first class-fields)))
           (type-class (third (first class-fields))))
      (if (eql name name-class) ;; field found
          (if (null value)
              (if (eql type-class T)
                  t (error "Cannot assign NIL to type ~A" 
                           type-class))
            (if (eql type-class T) ;;no type -> check value
                (if (or (typep value (type-of value-class))
                        (subtypep (type-of value) (type-of value-class)))
                    t (error "Cannot assign ~A to type ~A" 
                             (type-of value) (type-of value-class)))
              (if (typep value type-class)
                  t (error "Cannot assign ~A to type ~A" 
                           (type-of value) type-class)))) ;; value not null
        (is-valid-instance-value (rest class-fields) 
                                 name 
                                 value))))) ;; continue

(defun inherit-fields (fields default-fields)
  
  (if (null default-fields)
      fields
    (let* ((field-name (first (first default-fields)))
           (field-value (second (first default-fields))))
      (if (is-name-present fields field-name)
          (inherit-fields fields (rest default-fields))
        (inherit-fields (append fields (list field-name field-value)) 
                        (rest default-fields))))))

(defun is-name-present (fields field-name)
  (if (null fields)
      NIL
    (if (eql (first fields) field-name)
        T 
      (is-name-present (cddr fields) field-name))))



;; FIELD
(defun field (instance field-name)
  (if(is-instance instance)
      (getf (getf instance :fields) field-name)
    (error "~A is not a valid instance" instance)))

;; FIELD*
(defun field* (instance &rest field-names)
  (if (null field-names)
      (error "no attributes to traverse or field not found")
    (let ((value (field instance (first field-names))))
      (if (= 1 (length field-names))
          value
        (if (is-instance value)
            (apply #'field* value (rest field-names))
          (error "~A is not a valid instance" value))))))



;; IS_CLASS
(defun is-class (classname)
  (if (symbolp classname)
      (if (gethash classname *classes-specs*) T NIL)
    (error "class-name must be a symbol")))



;; IS_INSTANCE
(defun is-instance (value &optional(class-name T))
  (if (equal class-name T)
      (if (and (listp value) (not (null (getf value :class))))
          T
        NIL)  ;;checking if value is a valid instance
    (if(is-class class-name)
        (if (and (listp value) 
                 (or (eql class-name (getf value :class)) 
                     (is-subclass-of (getf value :class) class-name)))
            T
          NIL) ;;checking if value is a valid instance of class            
      (error "~A is not a valid class" class-name))))

(defun is-subclass-of (class-name passed-class-name)
  (let ((parents (get-all-parents class-name)))
    (if (member passed-class-name parents) T NIL)))

(defun get-all-parents (class-name)
  (let ((class (class-spec class-name)))
    (if class
        (let ((parents (getf class :parents)))
          (if parents
              (append parents (mapcan #'get-all-parents parents))
            nil))
      (error "Class ~A not found." class-name))))

(defun check-parents (parents superclass)
  (if (null parents)
      NIL
    (or (is-subclass-of (car parents) superclass) 
        (check-parents (cdr parents) superclass))))