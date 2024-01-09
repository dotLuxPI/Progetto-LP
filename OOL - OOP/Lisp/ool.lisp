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

(defun is-class (classname)
  (if (symbolp classname)
      (if (gethash classname *classes-specs*) T NIL)
    (error "class-name must be a symbol")))



;;;; MEMO!!!!
;; def-class deve restituire il class-name in caso di successo
;; attualmente restituisce l'intera classe

;; CLASS CREATION
(defun def-class (classname &optional (parents '()) parts)
  (if (symbolp classname) 
      (if (not (gethash classname *classes-specs*))    
          (if (or (listp parents) (null parents))
              (if (is-class-list parents)
                  (if (parts-check parts)
                      (let ((finalParts (concat-fields (parts-validation (cdr parts) (get-all-parents-parts parents (cdr parts))) (get-all-parents-parts parents (cdr parts)))))
                        (format t "final-parts: ~A~%class: ~A~%parents: ~A~%~%"
                                finalParts parts (get-all-parents-parts parents (cdr parts)))
                        (progn
                          (add-class-spec classname (list 
                                                     :classname classname 
                                                     :parents parents 
                                                     :parts (cons 'FIELDS finalParts)))
                          (print classname)
                          (class-spec classname)))
                    (error "parts must be a list of methods and fields"))
                (error "parents must be a list of existing classes")) 
            (error "parents must be a list of classes")) 
        (error "classname already exists")) 
    (error "classname must be a symbol")))

(defun is-class-list (parents)
  (if (null parents)
      t
    (and (is-class (car parents)) (is-class-list (cdr parents)))))

(defun to-list (x)
  (if (listp x) x (list x)))

;; parts-check
(defun parts-check (parts)
  (if (and (listp parts) (eql (car parts) 'fields))
      (let ((fields (cdr parts)))
        (if (is-valid-fields fields)
            T
          (error "parts must be a list of methods and fields")))
    (error "parts must be a list of methods and fields")))



;; FIELD MANAGEMENT
(defun concat-fields (parts parents-parts)
  (if (null parts)
      parents-parts
    (let ((part (car parts))
          (rest-parts (cdr parts)))
      (if (assoc (car part) parents-parts :test #'equal)
          (cons part (concat-fields rest-parts 
                                    (remove (assoc (car part) parents-parts :test #'equal) parents-parts)))
        (cons part (concat-fields rest-parts parents-parts))))))

(defun parts-validation (parts parents-parts)
  (if (null parts)
      '()
      (cons (fill-in-field (car parts) parents-parts) (parts-validation (rest parts) parents-parts))))

;; adds missing field parts and normalize field to 3-parameter-form
(defun fill-in-field (part parents-fields) 
  (cond
   ((or (= 1 (length part))
        (and (= 2 (length part)) (eql nil (second part)))
        (and (= 3 (length part)) (eql nil (second part)) (eql t (third part))))
    (compatibility-check (fill-type-field (fill-value-field part parents-fields) parents-fields) parents-fields))
   
   ((or (= 2 (length part)) 
        (eql (third part) t)) 
    (compatibility-check (fill-type-field part parents-fields) parents-fields))
   
   ((= 3 (length part)) (compatibility-check part parents-fields))))

;; add missing value to field if available to inherit
(defun fill-value-field (part parents-fields)
  (if (null parents-fields)
      (list (first part) nil) ;; normalize field length by adding value
    (let ((parent-part (first parents-fields)))
      (if (eql (first part) (first parent-part)) ;; if same field-name
          (list (first part) (second parent-part)) ;; normalize field length by adding value
        (fill-value-field part (rest parents-fields)))))) ;; continue

;; add missing type to field if available to inherit
(defun fill-type-field (part parents-fields)
  (if (null parents-fields) ;; base
      (list (first part) (second part) t) ;; normalize field length by adding type
    (let ((parent-part (first parents-fields)))
      (if (eql (first part) (first parent-part)) ;; if same field-name
          (if (third parent-part) ;; if parent has field-type
              (if (or (typep (second part) (third parent-part)) ;; if type or subtype
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
           (if (or (null (third class-part)) ;; if class-part true/nil
                   (eql (third class-part) t))
               (if (typep (second class-part) (third part)) ;; check value type
                   part 
                 (error "Type mismatch for field ~A. ~%Expected: ~A [or valid subtype]~%Found: ~A~%"
                        (first part) (third part) (type-of (second class-part))))
             (if (eql (third part) (third class-part))
                  part
                (error "Type mismatch for field ~A: ~A vs ~A"
                       (first part) (third part) (third class-part))))
         (compatibility-check part (rest parts-list)))))) ;; continue

(defun get-all-parents-parts (parents parts)
  (if parents
      (let* ((parent (car parents))
             (parent-spec (class-spec parent))
             (parent-fields (to-list (getf parent-spec :parts)))
             (parts (to-list parts)))
        (if (parts-validation (cdr parts) (cdr parent-fields))
            (let ((new-parts (append (remove-duplicates (append (cdr parent-fields) (cdr parts))
                                                        :test #'equal :key #'car))))
              (get-all-parents-parts (cdr parents) new-parts))
          (error "generic error")))
    parts))



;; FIELD CREATION
(defun is-valid-fields (fields)
  (if (null fields)
      T (and (is-valid-field-structure (car fields)) (is-valid-fields (cdr fields)))))

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

;; MAKE
(defun make (class-name &optional (fields nil))
  (if (is-class class-name)
      (if (null fields)
          (make-default-instance class-name) ;; return instance with default values
        () ;; return instance with custom values
       )
    (error "~A is not a valid class-name" class-name)))

(defun field (instance field-name)
  (if(is-instance instance)
   ()
    (error "~A is not a valid instance" instance)))
     

(defun is-instance (value &optional(class-name T))
  (if (equal class-name t)
    ()  ;;checking if value is a valid instance
    (if(is-class class-name)
      () ;;checking
      (error "~A is not a valid class" class-name)
        )  ;;checking if value is a valid instance of class 
      )
)