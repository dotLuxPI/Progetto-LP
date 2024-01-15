;;;; Perego Luca 894448
;;;; Magliani Andrea 894395
;;;; Picco Nicolas 894588

;; hash-table declaration & manipulation
(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

;; remove-class/1: utility method that removes class identified 
;; by name from the hash-table
(defun remove-class (name)
  (if (symbolp name)
      (remhash name *classes-specs*)
      (error "classname must be a symbol")))

;; remove-all-classes/0: utility method that drops every class from 
;; the hash-table
(defun remove-all-classes ()
  (clrhash *classes-specs*))



;; DEF_CLASS/3
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
                               (final-methods (get-all-methods
                                               parents
                                               class-methods)))
                          (progn

                            (process-all-methods final-methods)

                            (add-class-spec 
                             classname (list 
                                        :classname classname 
                                        :parents parents 
                                        :parts (append 
                                                (list 'FIELDS 
                                                      final-fields) 
                                                (list 'METHODS 
                                                      final-methods))))
                            classname))
			(error "parts must be a list of methods and fields")))
                  (error "parents must be a list of existing classes")) 
              (error "parents must be a list of classes")) 
          (error "classname already exists")) 
      (error "classname must be a symbol")))

;; is-class-list 
(defun is-class-list (parents)
  (if (null parents)
      t
      (and (is-class (car parents)) (is-class-list (cdr parents)))))

;; to-list/1: casts the parameter passed to type list
(defun to-list (x)
  (if (listp x) x (list x)))

;; concat-all/2: gets every field or method 
;; (depends on delimiter value)
(defun concat-all (delimiter parts)
  (if (null parts)
      NIL
      (if (eql delimiter (first (first parts)))
          (append 
           (rest (first parts)) 
           (concat-all delimiter (rest parts))) ;;concat
	  (concat-all delimiter (rest parts))))) ;; continue

;; parts-check/2: calls checking function on fields and methods
(defun parts-check (fields methods)
  (if (and (listp fields) (listp methods))
      (if (is-valid-fields fields)
          (if (is-valid-methods methods)
              T NIL))))
;; get-fields/1: takes parts as input, outputs only the fields
(defun get-fields (parts)
  (let* ((start (position 'FIELDS parts :test #'eq))
         (end (position 'METHODS parts :test #'eq)))
    (if (null start)
        nil
	(if (null end)
            (rest parts)
            (subseq parts (1+ start) end)))))

;; get-methods/2: takes parts as input, outputs only the methods
(defun get-methods (parts)
  (let* ((start (position 'METHODS parts :test #'eq))
         (end (length parts)))
    (if (null start)
        nil
	(if (null end)
            (rest parts)
            (subseq parts (1+ start) end)))))



;; FIELDS/N
(defun fields (&rest field-specs)
  (if (null field-specs)
      '()
      (let* ((spec (first field-specs))
             (name (first spec))
             (value (second spec))
             (f-type (if (null (cddr spec)) 'T (car (cddr spec))))
             (field (list name value f-type)))
	(cons field (apply #'fields (rest field-specs))))))


;; concat-parts/2: concatenates parts with parents-parts, giving priority
;; to the class parts and then parents-parts following the list order
(defun concat-parts (parts parents-parts)
  (if (null parts)
      parents-parts
      (let ((part (car parts))
            (rest-parts (cdr parts)))
	(if (assoc (car part) parents-parts :test #'equal)
            (cons part (concat-parts rest-parts 
                                     (remove 
                                      (assoc (car part) 
                                             parents-parts 
                                             :test #'equal) 
                                      parents-parts)))
            (cons part
		  (concat-parts rest-parts parents-parts))))))

;; field-validation/2: checks if class parts has every field in it as a 
;; 3 parameter field (field name value type) and fills the fields if
;; parameters are missing using fill-in-field
(defun fields-validation (parts parents-parts)
  (let* ((fields parts)
         (parents-fields parents-parts))
    (if (null fields)
        '()
	(cons (fill-in-field (car fields) parents-fields) 
              (fields-validation (rest fields) parents-fields)))))

;; fill-in-field/2: adds missing field components with inheritance 
;; (checking if fields are compatible with their parent-classes) and normalize
;; field to the 3-parameter-form: (field name value type)
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

;; fill-value-field/2: adds missing value to field if available to inherit from
;; parents, else set value as NIL to normalize length
(defun fill-value-field (part parents-fields)

  (if (null parents-fields)
      (list (first part) 
            nil)
      (let ((parent-part (first parents-fields)))
	(if (eql (first part) (first parent-part))
            (list (first part) 
                  (second parent-part))
            (fill-value-field part (rest parents-fields))))))

;; fill-type-field/2: adds missing type to field if available to inherit from
;; parents, else set type as T to normalize length
(defun fill-type-field (part parents-fields)
  (if (null parents-fields)
      (list (first part) 
            (second part) 
            t)
      (let ((parent-part (first parents-fields)))
	(if (eql (first part) (first parent-part))
            (if (third parent-part)
		(if (or (typep (second part) 
                               (third parent-part)) 
			(subtypep (type-of (second part)) (third parent-part)))
                    (if (= 2 (length part))
			(list (first part) (second part) (third parent-part))
			part)
                    (if (= 2 (length part))
			(list (first part) (second part) t)  
			part))
		(if (= 2 (length part))
                    (list (first part) (second part) t)
		    part))
            (fill-type-field part (rest parents-fields))))))

;; compatibility-check/2: checks if part fully is compatible with 
;; parents-fields
(defun compatibility-check (part parts-list)
  (if (null parts-list)
      part
      (let ((class-part (first parts-list)))
	
	(if (eql (first part) (first class-part))
            (if (null (second class-part))
		part
		
		(if (or (null (third class-part))
			(eql (third class-part) t))
                    (if (typep (second class-part) 
                               (third part))
			part 
			(error "Type mismatch for field ~A. ~%~A vs ~A~%"
                               (first part) 
                               (third part) 
                               (type-of (second class-part))))
                    (if (eql (third part) (third class-part))
			part
			(error "Type mismatch for field ~A: ~A vs ~A"
                               (first part) (third part) (third class-part)))))
            (compatibility-check part (rest parts-list))))))

;; get-all-fields/2: gets every field of a class and appends them to the 
;; fields inherited from parents, removing duplicates
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
              (let ((final-parts (concat-parts 
                                  temp-parts parent-fields)))
		final-parts)))
          (error "~A is not an existing class" (car parents)))
      parts))

;; is-valid-fields/1: recursevly calls is-valid-field-structure on fields list
(defun is-valid-fields (fields)
  (if (null fields)
      T 
      (and (is-valid-field-structure (car fields)) 
           (is-valid-fields (cdr fields)))))

;; field structure/1: filters field by length, normalizing it to 3, and passing
;; it the result to is-field
(defun is-valid-field-structure (field)
  (if (= 3 (length field)) ;; (name value type)
      (is-field (first field) (second field) (third field))
      (if (= 2 (length field)) ;; (name value)
          (is-field (first field) (second field) T) 
	  (if (= 1 (length field)) ;; (name)
              (is-field (first field) nil T) nil))))

;; is-field/3: checks every component of the a field
(defun is-field (field-name field-value &optional (field-type T))
  (if (symbolp field-name)
      (if (or (symbolp field-type) (eql field-type T))
          (if (or (typep field-value field-type) 
                  (eql field-type T))
              T
              (error "field-type is not valid or type mismatch"))
          (error "field-type must be a symbol"))
      (error "field-name must be a symbol")))


;; METHODS/N
(defun methods (&rest methods-specs)
  (if (null methods-specs)
      '()
      (let* ((spec (first methods-specs))
             (name (first spec))
             (args (second spec))
             (form (third spec))
             (method (list name args form)))
	(cons method (apply #'methods (rest methods-specs))))))


;; is-valid-methods/1: recursevly calls is-valid-method-structure on
;; methods list 
(defun is-valid-methods (methods) 
  (if (null methods)
      t
      (and (is-valid-method-structure (car methods))
           (is-valid-methods (cdr methods)))))

;; is-valid-method-structure/1: checks if method passed is a list of 3 
;; elements, and calls is-methods function on its components
(defun is-valid-method-structure (method)
  (if (= 3 (length method))
      (is-method (first method) (second method) (third method))))

;; is-method/3: checks every component of a method
(defun is-method (name args form)
  (if (and name form)
      (if (symbolp name)
          (if (listp args)
              (if (is-sexp form)
                  t 
                (error "~A in ~A is not a valid expression" form name))
            (error "~A args must be a list of standard parameter" name))
        (error "method's name must be a symbol"))
    (error "methods must have 3 non-null arguments")))

;; is-sexp/1: checks if argument passed is a symbolic expresson
(defun is-sexp (f)
  (cond ((null f) t)
        ((atom f) t)
        ((and (consp f)
              (is-sexp (car f))
              (is-sexp (cdr f))))))

;; get-all-methods/2: gets every method of a class and appends them to the 
;; methods inherited from parents, removing duplicates
(defun get-all-methods (parents parts)
  (if parents
      (if (is-class (car parents))
          (let ((parent-methods (first (get-methods 
					(getf (class-spec (car parents)) 
                                              :parts)))))
            (let ((temp-methods (remove-duplicates 
                                 (append 
                                  (get-all-methods (rest parents) parts))
                                 :test #'equal :key #'car)))
              (let ((final-parts (concat-parts
                                  temp-methods parent-methods)))
                final-parts)))
        (error "~A is not an existing class" (car parents))) ;; code
    parts))

;; method processing
(defun process-method (method-name method-spec)
  (let* ((args (car method-spec))
         (form (first (cdr method-spec))))

    (setf (fdefinition method-name) 
          (lambda (instance &rest args) 
            (instance-method-check instance method-name)
            (let* ((parts (getf (class-spec (getf instance :class)) :parts))
                   (new-form (third (get-method-listed method-name (first (get-methods parts))))))
            (eval (rewrite-method-code instance args new-form)))))))

;; substitute this with instance
(defun rewrite-method-code (instance args spec)
  (subst (substitute-this instance) 'this)

(defun substitute-this (instance)
  `(quote ,instance)
)

;; instance-method-check/2: checks if instance passed has the method 
;; (referenced by method-name) available to call
(defun instance-method-check (instance method-name)
  (if (is-instance instance)
      (let* ((class (class-spec (getf instance :class)))
             (class-name (getf class :classname))
             (methods (first (get-methods (getf class :parts)))))
        (if (is-method-listed method-name methods)
            T (error "~A is not defined for ~A" method-name class-name)))
      (error "invalid instance on method call ~A" method-name)))

;; is-method-listed/2: returns true if name appear as car of one of the 
;; elements of methods
(defun is-method-listed (name methods)
  (if (null methods)
      nil
      (if (eql name (caar methods))
          T 
	  (is-method-listed name (rest methods)))))

(defun get-method-listed (name methods)
  (if (null methods)
      nil
    (if (eql name (caar methods))
        (car methods)
      (is-method-listed name (rest methods)))))

(defun process-all-methods (methods)

  (if (null methods)
      NIL
    (let* ((method (car methods))
           (name (car method))
           (spec (cdr method)))
      (progn 
        (process-method name spec)
        (process-all-methods (rest methods))))))


;; MAKE/(1 + N)
(defun make (class-name &rest fields)
  (if (is-class class-name)
      (if (null fields)
          (make-default-instance class-name)
          (make-custom-instance class-name fields))
      (error "~A is not a valid class-name" class-name)))

;; make-default-instance/1: creates an instance with default value of its class
(defun make-default-instance (class-name)
  (let* ((class-parts (getf (class-spec class-name) :parts))
         (fields (get-fields class-parts)))
    (list :class class-name :fields (inherit-fields '() (first fields)))))

;; make-custom-instance/3: creates an instance using custom values passed 
;; during instance definition
(defun make-custom-instance (class-name fields &optional (result nil))
  (let* ((class-parts (getf (class-spec class-name) :parts))
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

;; is-valid-instance-value/3: checks if custom name and value are compatible
;; with the field declared in the class 
(defun is-valid-instance-value (class-fields name value)  
  (if (null class-fields)
      (error "Unknown field: (~A ~A)" name value)
      (let* ((name-class (first (first class-fields)))
             (value-class (second (first class-fields)))
             (type-class (third (first class-fields))))
	(if (eql name name-class)
            (if (null value)
		(if (eql type-class T)
                    t (error "Cannot assign NIL to type ~A" 
                             type-class))
		(if (eql type-class T)
                    (if (or (typep value (type-of value-class))
                            (subtypep (type-of value) (type-of value-class)))
			t (error "Cannot assign ~A to type ~A" 
				 (type-of value) (type-of value-class)))
		    (if (typep value type-class)
			t (error "Cannot assign ~A to type ~A" 
				 (type-of value) type-class))))
            (is-valid-instance-value (rest class-fields) 
                                     name 
                                     value)))))

;; inherit-fields/2: returns every field of a class in the following form:
;; ((name-1 value-1) ... (name-n value-n))
(defun inherit-fields (fields default-fields)
  (if (null default-fields)
      fields
      (let* ((field-name (first (first default-fields)))
             (field-value (second (first default-fields))))
	(if (is-name-present fields field-name)
            (inherit-fields fields (rest default-fields))
            (inherit-fields (append fields (list field-name field-value)) 
                            (rest default-fields))))))

;; is-name-present/2: checks if field-name is presents as car of one of the
;; elements in fields
(defun is-name-present (fields field-name)
  (if (null fields)
      NIL
      (if (eql (first fields) field-name)
          T 
	  (is-name-present (cddr fields) field-name))))



;; FIELD/2
(defun field (instance field-name)
  (if(is-instance instance)
     (getf (getf instance :fields) field-name)
     (error "~A is not a valid instance" instance)))

;; FIELD*/(1 + N)
(defun field* (instance &rest field-names)
  (if (null field-names)
      (error "no attributes to traverse or field not found")
      (let ((value (field instance (first field-names))))
	(if (= 1 (length field-names))
            value
            (if (is-instance value)
		(apply #'field* value (rest field-names))
		(error "~A is not a valid instance" value))))))



;; IS_CLASS/1
(defun is-class (classname)
  (if (symbolp classname)
      (if (gethash classname *classes-specs*) T NIL)
      (error "class-name must be a symbol")))



;; IS_INSTANCE/2
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

;; is-subclass-of/2: checks if the first parameter identifies a subclass of
;; the class identified by the second parameter
(defun is-subclass-of (class-name passed-class-name)
  (let ((parents (get-all-parents class-name)))
    (if (member passed-class-name parents) T NIL)))

;; get-all-parents/1: returns every parent of the class identified 
;; by class-name
(defun get-all-parents (class-name)
  (let ((class (class-spec class-name)))
    (if class
        (let ((parents (getf class :parents)))
          (if parents
              (append parents (mapcan #'get-all-parents parents))
              nil))
	(error "Class ~A not found." class-name))))

