;;SUDOKU SOLVER V1.0
;; CREATED BY FRANCISCO DURAO (https://github.com/FoxyDingo)

(defvar *dims* 9)
(defvar *square* 3)

;;Returns all possibilities for a certain line and column
(defun poss (l c estado)
  (nth c (nth l estado)))

(defun def-poss (l c estado p)
  (setf (nth c (nth l estado)) p)
  (if (equalp (list-length p) 1)
    (progn 
      (limpa-est estado)
      (gera-poss estado))))

(defun sem-poss? (l c estado)
  (equalp (poss l c estado) NIL))

(defun poss-imp? (l c estado)
  (equalp (poss l c estado) (list 'X)))

(defun poss-uni? (l c estado)
  (equalp (list-length (poss l c estado)) 1))

(defun list-pos ()
  (let ((lista nil))
    (setf lista (make-list *dims* :initial-element 0))
    (dotimes (i *dims*)
      (setf (nth i lista) (+ i 1)))
    (return-from list-pos lista)))

;;Generate the list of possibilities for a certain line and column
(defun gera-lista-poss (l c estado)
  (let ((pos (list-pos))
        (ltmp 0)
        (ctmp 0))
    (dotimes (lt *dims*)
      (if (poss-uni? lt c estado) (setf pos (remove (car (poss lt c estado)) pos))))
    (dotimes (ct *dims*)
      (if (poss-uni? l ct estado) (setf pos (remove (car (poss l ct estado)) pos))))
    (dotimes (lt *square*)   
      (dotimes (ct *square*)
        (progn
          (setf ltmp (+ (* (floor (/ l *square*)) *square* ) (mod (+ l lt) *square*)))
          (setf ctmp (+ (* (floor (/ c *square*)) *square* ) (mod (+ c ct) *square*)))
          (if (poss-uni? ltmp ctmp estado) (setf pos (remove (car (poss ltmp ctmp estado)) pos))))))
    (if (> (list-length pos) 0) (def-poss l c estado pos) (def-poss l c estado (list 'X)))
    ))

(defun encontra-e-acerta-linha (l n estado)
  (dotimes (c *dims*)
    (if (not (equalp (member n (poss l c estado)) NIL))
      (progn
        (def-poss l c estado (cons n NIL))
        (return-from encontra-e-acerta-linha t)))))

(defun encontra-e-acerta-coluna (c n estado)
  (dotimes (l *dims*)
    (if (not (equalp (member n (poss l c estado)) NIL))
      (progn
        (def-poss l c estado (cons n NIL))
        (return-from encontra-e-acerta-coluna t)))))

(defun encontra-e-acerta-quadrado (l c n estado)
  (let ((ctmp 0)
    (ltmp 0))
  (dotimes (lt *square*)   
    (dotimes (ct *square*)
      (setf ltmp (+ (* (floor (/ l *square*)) *square* ) (mod (+ l lt) *square*)))
      (setf ctmp (+ (* (floor (/ c *square*)) *square* ) (mod (+ c ct) *square*)))
      (if (not (equalp (member n (poss ltmp ctmp estado)) NIL))
        (progn
        (def-poss ltmp ctmp estado (cons n NIL))
        (return-from encontra-e-acerta-quadrado t)))))))
  
;;ENG after having all possibilities calculated acerta-poss goes to every position and
;;    if it finds a position where there is only one possibility it goes to all positions 
;;    in the same line, column and square and deletes that number from the list of possibilities
;;ENG acerta-poss can only be called after all possibilities have been calculated for every l c
;;PT  acerta-poss apenas pode ser chamada depois de todas as possibilidades terem sido calculadas
(defun acerta-poss (estado)
  (let ((poss-counter (make-list *dims* :initial-element 0))
        (ql 0)
        (qc 0)
        (tmp-poss '())
        (ctmp 0)
        (ltmp 0))
    ;;linhas
    (dotimes (l *dims*)
      (setf poss-counter (make-list *dims* :initial-element 0))
      (dotimes (c *dims*)
        (setf tmp-poss (poss l c estado))
        (if (> (list-length tmp-poss) 1)
          (progn
           (dolist (nr tmp-poss)
            (setf (nth (- nr 1) poss-counter) (+ (nth (- nr 1) poss-counter) 1))))))
        (dotimes (n *dims*)
          (if (equalp (nth n poss-counter) 1) 
            (progn
            (encontra-e-acerta-linha l (+ n 1) estado)))))
    ;;colunas
    (dotimes (c *dims*)
      (setf poss-counter (make-list *dims* :initial-element 0))
      (dotimes (l *dims*)
        (setf tmp-poss (poss l c estado))
        (if (> (list-length tmp-poss) 1)
          (progn
            (dolist (nr tmp-poss)
              (setf (nth (- nr 1) poss-counter) (+ (nth (- nr 1) poss-counter) 1))))))
          (dotimes (n *dims*)
            (if (equalp (nth n poss-counter) 1) (encontra-e-acerta-coluna c (+ n 1) estado))))
    ;;quadrados 
    (dotimes (l *square*)
      (setf ql (* l *square*))
      (dotimes (c *square*)
        (setf qc (* c *square*))
        (setf poss-counter (make-list *dims* :initial-element 0))
        (dotimes (lt *square*)   
          (dotimes (ct *square*)
            (progn
              (setf ltmp (+ (* (floor (/ ql *square*)) *square* ) (mod (+ ql lt) *square*)))
              (setf ctmp (+ (* (floor (/ qc *square*)) *square* ) (mod (+ qc ct) *square*)))
              (setf tmp-poss (poss ltmp ctmp estado))
              (if (> (list-length tmp-poss) 1)
              (dolist (nr tmp-poss)
                (progn
                  (setf (nth (- nr 1) poss-counter) (+ (nth (- nr 1) poss-counter) 1))))))))
          (dotimes (n *square*)
            (if (equalp (nth n poss-counter) 1) 
              (progn
                (encontra-e-acerta-quadrado ql qc (+ n 1) estado))))))
        ))

(defun gera-poss (estado)
    (dotimes (l *dims*)
      (dotimes (c *dims*)
        (if (sem-poss? l c estado)
          (gera-lista-poss l c estado)))))

(defun vitoria? (estado)
  (let ((nrCamposMax (* *dims* *dims*))
        (nrCamposUnico 0))
    (dotimes (l *dims*)
      (dotimes (c *dims*)
      (if (poss-uni? l c estado) (setf nrCamposUnico (+ nrCamposUnico 1)))))
    (if (equalp nrCamposUnico nrCamposMax) t)))

(defun estado-imp? (estado)
  (dotimes (l *dims*)
    (dotimes (c *dims*)
      (if (poss-imp? l c estado) (return-from estado-imp? t))))
  nil)

(defun limpa-est (estado)
  (dotimes (l *dims*)
    (dotimes (c *dims*)
      (if (not (poss-uni? l c estado)) (def-poss l c estado NIL)))))

(defun solver (estado)
    (gera-poss estado)
    (acerta-poss estado)
    (limpa-est estado)
    estado) 

;;ENG copia-estado only copies states with no multiple possibilities
;;PT  copia-estado so copia estados sem multiplas possibilidades
(defun copia-estado (estado)
  (let ((tmp-estado (make-list *dims* )))
    (dotimes (a *dims*)
      (setf (nth a tmp-estado) (make-list *dims*)))
    (dotimes (l *dims*)
      (dotimes (c *dims*)
        (if (equalp (list-length (poss l c estado)) 1)
          (setf (nth c (nth l tmp-estado)) (list (car (poss l c estado)))))))
    tmp-estado))


(defun gera-alternativas (estado lst-estados)
  (let ((possibs nil)
    (tmp-estado nil)
    (lst-tmp lst-estados)
    (est (copia-estado estado)))
    (gera-poss est)
    (loop for i from 2 to *dims* do
      (dotimes (l *dims*)
        (dotimes (c *dims*)
          (setf possibs (poss l c est))
          (if (equalp (list-length possibs) i)
            (progn
              (dotimes (n i)
                (setf tmp-estado (copia-estado est))
                (def-poss l c tmp-estado (list (nth n possibs)))
                (setf lst-tmp (append (list (copia-estado tmp-estado)) lst-tmp)))
              (return-from gera-alternativas lst-tmp))))))))
    
(defun su-solver (st)
  (let ((lst-estados (list st))
    (estado nil)
    (tmp-estado nil)
    (estado-ant nil))
    (setf *dims* (list-length st))
    (setf *square* (sqrt *dims*))
    (loop
      (block inner
        (setf estado (first lst-estados))
        (if (vitoria? estado)(return-from su-solver estado))
        (if (equalp estado estado-ant) 
          (progn
            (setf lst-estados (gera-alternativas estado lst-estados))
            (setf lst-estados (remove estado lst-estados))
            (return-from inner)))
        ;(su-solver estado)
        (setf tmp-estado (solver (copia-estado estado)))
        (if (not (estado-imp? tmp-estado))
          (progn
            (setf lst-estados (append (list tmp-estado) lst-estados))
            (setf estado-ant estado)))
        (setf lst-estados (remove estado lst-estados :count 1)))
      )))


