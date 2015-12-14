;;;Projet 

;;;Jeux combat d'espace


; IMPORTATION DES LIBRABRIES GRA{P}HIQUE et PHYSIQUE


(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")
; (ql:quickload "clinch")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main))

(in-package :asteroids)

(defparameter *map-width* 920)
(defparameter *map-height* 680)

(defparameter *window* nil)
(defparameter *window-width* 920)

(defparameter *deceleration* 0.8)

(defparameter *ticks* 0)

(defparameter *songoku-max-age* 8)

(defparameter *explosion-max-radius* 0.5)

(defun vector-sum (a b)
  (mapcar #'+ a b))

(defun vector-scale (v factor)
  (mapcar #'* v (list factor factor)))

(defun vector-subtract (a b)
  (mapcar #'- a b))

;;; distance between point a and point b
;;; parameters must be lists of 2 numbers (x y)
(defun my-distance (a b)
  (sqrt (apply #'+
               (mapcar (lambda (x)
                         (expt x 2))
                       (vector-subtract a b)))))

(defun square-from-midpoint (point radius)
  (rectangle-from-midpoint-* (x point)
                             (y point)
                             (* radius 2)
                             (* radius 2)))

(defun deg->rad (degs)
  (* degs (/ pi 180)))

(defun rad->deg (rads)
  (* rads (/ 180 pi)))

(defun radial-point-from (p radius angle)
  (point :x (+ (* radius (sin (deg->rad angle))) (x p))
         :y (+ (* radius (cos (deg->rad angle))) (y p))))

(defun get-ticks ()
  (let ((ticks (shiftf *ticks* (sdl-get-ticks))))
    (* (- *ticks* ticks) 0.001)))

;;; representant un object sur la carte du jeu
(defclass objet-general ()
  ((pos :initarg :pos :initform '(0.5 0.5) :accessor pos)
   (radius :initarg :radius :accessor radius)
   (velocity :initarg :velocity :initform '(0 0) :accessor velocity)))

(defclass asteroid (objet-general)
  ((size :initarg :size :initform 'big :reader size)
   (radii :initform nil :accessor radii)
   (rotation :initform (* (- (random 1.0) 0.5) 5) :reader rotation)
   (facing :initform 0 :accessor facing)
   (pos :initform `(,(random 1.0) ,(random 1.0)))))

(defclass bullet (objet-general)
  ((radius :initform 0.005)
   (fusee :initarg :fusee :accessor fusee)))

(defclass explosion (objet-general)
  ((radius :initform 0)))

(defclass songoku (objet-general)
  ((radius :initform 0.03)
   (age :initform 0 :accessor age)))

(defclass bullet-songoku (songoku) ())

(defclass freeze-songoku (songoku) ())

(defclass protection-songoku (songoku) ())

(defclass fusee (objet-general)
  ((horloges :initform (make-hash-table) :accessor horloges)
   (acceleration :initform '(0 0) :accessor acceleration)
   (facing :initform 0 :accessor facing)
   (radius :initform 0.04)))

(defclass horloge ()
  ((remaining :initarg :seconds :initform 0 :accessor remaining)))

(defclass universe ()
  ((objet-generals :initform nil :accessor objet-generals)
   (fusee :initform nil :accessor fusee)
   (bullet :initform nil :accessor bullet)
   (horloges :initform (make-hash-table) :accessor horloges)
   (level :initform 0 :accessor level)
   (num-asteroids :initform 0 :accessor num-asteroids)
   (point-gagne :initform 0 :accessor point-gagne)
   (max-level :initform 0 :accessor max-level)
   (high-point-gagne :initform 0 :accessor high-point-gagne)
   (coeurs :initform 0 :accessor coeurs)))

(defmethod collide ((objet-general objet-general) (other objet-general) (universe universe)) t)

(defmethod map-coords ((objet-general objet-general))
  (destructuring-bind (x y) (pos objet-general)
    (point :x (round (* x *map-width*))
           :y (round (* y *map-height*)))))

(defun relative-coords (x y)
  (list (/ x *map-width*) (/ y *map-height*)))

(defmethod map-radius ((objet-general objet-general))
  (round (* (radius objet-general) *map-width*)))

(defmethod update ((objet-general objet-general) time-delta (universe universe))
  (setf (pos objet-general)
        (mapcar (lambda (x) (mod x 1))
                (vector-sum (pos objet-general)
                            (vector-scale (velocity objet-general)
                                          time-delta)))))

(defmethod intersects-p ((objet-general objet-general) (other objet-general))
  (< (my-distance (pos objet-general) (pos other))
     (+ (radius objet-general) (radius other))))

(defmethod render ((objet-general objet-general))
  (values))

(defmethod initialize-instance :after ((asteroid asteroid) &key)
  (let ((radius (cdr (assoc (size asteroid)
                            '((big . 0.1) (medium . 0.075) (small . 0.05)))))
        (spd (cdr (assoc (size asteroid)
                         '((big . 0.1) (medium . 0.15) (small . 0.25))))))
    (setf (radius asteroid) radius)
    (setf (radii asteroid)
          (loop for i from 0 below 20
            collect (round (* (+ 0.9 (random 0.2))
                              (map-radius asteroid)))))
    (setf (velocity asteroid)
          `(,(- (random (* 2 spd)) spd) ,(- (random (* 2 spd)) spd)))))

(defun random-songoku (&key pos)
  (make-instance (case (random 3)
                   (0 'bullet-songoku)
                   (1 'freeze-songoku)
                   (2 'protection-songoku))
                 :pos pos))

(defmethod break-down ((asteroid asteroid) (universe universe))
  (with-slots ((pos pos) (size size)) asteroid
    (if (eq size 'small)
      ;; gradually reduce the probability of songokus appearing
      (if (< (random 100) (/ 100 (+ 4 (* (level universe) 0.3))))
          `(,(random-songoku :pos pos))
          nil)
      (let ((smaller (cond
                     ((eq size 'big) 'medium)
                     ((eq size 'medium) 'small))))
        `(,(make-instance 'asteroid :pos pos :size smaller)
          ,(make-instance 'asteroid :pos pos :size smaller))))))

(defmethod done ((horloge horloge))
  (<= (ceiling (remaining horloge)) 0))

(defmethod frozen-p ((universe universe))
  (let ((horloge (gethash 'freeze (horloges universe) nil)))
    (and horloge
         (not (done horloge)))))

(defmethod update ((asteroid asteroid) time-delta (universe universe))
  (declare (ignore time-delta))
  (when (not (frozen-p universe))
    (incf (facing asteroid) (rotation asteroid))
    (call-next-method)))

(defmethod render ((asteroid asteroid))
  (draw-polygon (loop for i from 0
                      for r in (radii asteroid)
                  collect (radial-point-from (map-coords asteroid) r
                                             (+ (facing asteroid)
                                                (* i 18))))
                :color *white*))

(defmethod remove-from-universe ((universe universe) (objet-general objet-general))
  (setf (objet-generals universe) (remove objet-general (objet-generals universe))))

(defmethod remove-from-universe :after ((universe universe) (asteroid asteroid))
  (decf (num-asteroids universe)))

(defmethod remove-from-universe :after ((universe universe) (fusee fusee))
  (setf (fusee universe) nil))

(defmethod update ((songoku songoku) time-delta (universe universe))
  (when (> (ceiling (incf (age songoku) time-delta))
           *songoku-max-age*)
    (remove-from-universe universe songoku)))

(defmethod add-point-gagne ((universe universe) (point-gagne number))
  (setf (high-point-gagne universe)
        (max (incf (point-gagne universe) point-gagne)
             (high-point-gagne universe))))

(defmethod add-point-gagne ((universe universe) (songoku songoku))
  (add-point-gagne universe (* (level universe) 10)))

(defmethod add-point-gagne ((universe universe) (asteroid asteroid))
  (add-point-gagne universe (cdr (assoc (size asteroid)
                               '((big . 1) (medium . 2) (small . 5))))))

(defmethod collide :before ((fusee fusee) (songoku songoku) (universe universe))
  (remove-from-universe universe songoku)
  (add-point-gagne universe songoku))

(defmethod songoku-active-p ((fusee fusee) songoku)
  (let ((horloge (gethash songoku (horloges fusee) nil)))
    (and horloge
         (not (done horloge)))))

(defmethod add-seconds ((horloge horloge) seconds)
  (incf (remaining horloge) seconds))

(defmethod add-protection ((fusee fusee) &key (seconds 0))
  (if (songoku-active-p fusee 'protection)
    (add-seconds (gethash 'protection (horloges fusee)) seconds)
    (setf (gethash 'protection (horloges fusee))
          (make-instance 'horloge :seconds seconds))))

;;Ajouter  6^ secondes de protection
(defmethod collide :before ((fusee fusee) (songoku protection-songoku) (universe universe))
  (add-protection fusee :seconds 6))

(defmethod render ((songoku protection-songoku))
  (let ((coords (map-coords songoku))
        (radius (map-radius songoku)))
    (draw-circle coords radius
                 :color *blue*)
    (draw-polygon `(,(radial-point-from coords (round (* radius 0.8)) 40)
                    ,(radial-point-from coords (round (* radius 0.8)) 0)
                    ,(radial-point-from coords (round (* radius 0.8)) -40)
                    ,(radial-point-from coords (round (* radius 0.8)) -135)
                    ,(radial-point-from coords (round (* radius 0.8)) 135))
                  :color *white*)))

(defmethod add-super-bullets ((fusee fusee) &key (seconds 0))
  (if (songoku-active-p fusee 'super-bullets)
    (add-seconds (gethash 'super-bullets (horloges fusee)) seconds)
    (setf (gethash 'super-bullets (horloges fusee))
          (make-instance 'horloge :seconds seconds))))

(defmethod collide :before ((fusee fusee) (songoku bullet-songoku) (universe universe))
  (add-super-bullets fusee :seconds 6))

(defmethod render ((songoku bullet-songoku))
  (let ((coords (map-coords songoku))
        (radius (map-radius songoku)))
    (draw-circle coords radius
                 :color *magenta*)
    (draw-circle coords (round (* radius 0.3))
                 :color *white*)))

(defmethod add-freeze ((universe universe) &key (seconds 0))
  (if (frozen-p universe)
    (add-seconds (gethash 'freeze (horloges universe)) seconds)
    (setf (gethash 'freeze (horloges universe))
          (make-instance 'horloge :seconds seconds))))

(defmethod collide :before ((fusee fusee) (songoku freeze-songoku) (universe universe))
  (add-freeze universe :seconds 6))

(defmethod render ((songoku freeze-songoku))
  (let ((coords (map-coords songoku))
        (radius (map-radius songoku)))
    (draw-circle coords radius
                 :color *cyan*)
    (draw-polygon (loop for i from 0 to 11
                    collect (radial-point-from coords
                                               (round (* radius (if (= (mod i 2) 0)
                                                                       0.7
                                                                       0.2)))
                                               (* i 30)))
                  :color *white*)))

(defmethod add-to-universe ((universe universe) (objet-general objet-general))
  (setf (objet-generals universe) (cons objet-general (objet-generals universe)))
  (values objet-general))

(defmethod collide :before ((fusee fusee) (asteroid asteroid) (universe universe))
  (unless (songoku-active-p fusee 'protection)
    (remove-from-universe universe fusee)
    (add-to-universe universe (make-instance 'explosion :pos (pos fusee)))
    (decf (coeurs universe))))

(defmethod in-universe-p ((universe universe) (objet-general objet-general))
  (find objet-general (objet-generals universe)))

(defmethod fusee-moved ((universe universe) (fusee fusee))
  (dolist (objet-general (objet-generals universe))
    (when (and (not (eq fusee objet-general))
               (intersects-p fusee objet-general))
      (collide fusee objet-general universe))
    ;; si une collision detruit notre fusee, annuler de checking les collisions
    (when (not (in-universe-p universe fusee))
      (return fusee))))

(defmethod update-horloge ((horloge horloge) time-delta)
  (unless (done horloge)
    (decf (remaining horloge) time-delta)))

(defmethod update :around ((fusee fusee) time-delta (universe universe))
  (setf (velocity fusee)
        (vector-scale (vector-sum (velocity fusee)
                                  (acceleration fusee))
                      *deceleration*))
  (maphash (lambda (name horloge)
             (declare (ignore name))
             (update-horloge horloge time-delta))
           (horloges fusee))
  (call-next-method)
  (fusee-moved universe fusee))


; MOUVEMENT DU fusee par PROPULSION de bullet

(defmethod cannon ((fusee fusee) coords)
  (setf (acceleration fusee)
        (vector-sum (acceleration fusee)
                    (vector-scale (vector-subtract coords (pos fusee))
                                  -0.08))))

(defmethod monter-fusee ((fusee fusee) coords)
  (setf (acceleration fusee)
        (vector-sum (acceleration fusee)
                    (vector-scale (vector-subtract coords (pos fusee))
                                  0.12))))

(defmethod stop-thrust ((fusee fusee))
  (setf (acceleration fusee) '(0 0)))

(defmethod shoot-at ((fusee fusee) coords (universe universe))
  (let ((bullet (make-instance 'bullet :pos (pos fusee)
                                       :fusee fusee)))
    (setf (velocity bullet)
          (vector-scale (vector-subtract coords (pos bullet))
                        3))
    (add-to-universe universe bullet)))

(defmethod render ((fusee fusee))
  (let* ((coords (map-coords fusee))
         (radius (map-radius fusee))
         (facing (facing fusee))
         (nose (radial-point-from coords radius facing))
         (left (radial-point-from coords radius (- facing 130)))
         (right (radial-point-from coords radius (+ facing 130)))
         (tail (radial-point-from coords (round (* radius 0.5)) (+ facing 180))))
    (draw-polygon (list nose left tail right)
                  :color *white*)
    (when (songoku-active-p fusee 'protection)
          (draw-circle coords
                      (round (+ radius (random 3)))
                      :color *blue*))))

(defmethod super-p ((bullet bullet))
  (songoku-active-p (fusee bullet) 'super-bullets))

(defmethod collide :before ((bullet bullet) (asteroid asteroid) (universe universe))
  (remove-from-universe universe asteroid)
  (when (not (super-p bullet))
    (remove-from-universe universe bullet))
  (mapcar (lambda (objet-general)
            (add-to-universe universe objet-general))
          (break-down asteroid universe))
  (add-to-universe universe (make-instance 'explosion :pos (pos asteroid)))
  (add-point-gagne universe asteroid))

(defmethod render ((bullet bullet))
  (let ((coords (map-coords bullet))
        (radius (map-radius bullet)))
    (draw-circle coords radius
                 :color *red*)
    (when (super-p bullet)
          (draw-circle coords (+ (random 3))
                       :color *magenta*))))

(defmethod bullet-moved ((universe universe) (bullet bullet))
  (dolist (objet-general (objet-generals universe))
    (when (and (not (eq bullet objet-general))
               (intersects-p bullet objet-general))
      (collide bullet objet-general universe))
    (when (not (in-universe-p universe bullet))
      (return bullet))))

(defmethod update ((bullet bullet) time-delta (universe universe))
  (setf (pos bullet)
        (vector-sum (pos bullet)
                    (vector-scale (velocity bullet)
                                  time-delta)))
  (destructuring-bind (x y) (pos bullet)
    (when (or (not (< 0 x *map-width*))
              (not (< 0 y *map-height*)))
      (remove-from-universe universe bullet)))
  (bullet-moved universe bullet))

(defmethod render ((explosion explosion))
  (let ((coords (map-coords explosion))
        (radius (map-radius explosion)))
    (draw-circle coords radius :color *red*)
    (draw-circle coords
                 (+ radius (random 3))
                 :color *red*)))

(defmethod update ((explosion explosion) time-delta (universe universe))
  (when (> (incf (radius explosion) time-delta)
           *explosion-max-radius*)
    (remove-from-universe universe explosion)))

(defmethod start-next-level ((universe universe))
  (with-accessors ((level level)
                   (max-level max-level)
                   (objet-generals objet-generals)
                   (horloges horloges)
                   (fusee fusee))
                   universe
    (incf level)
    (setf max-level (max max-level level))
    (setf objet-generals nil)
    (setf horloges (make-hash-table))
    (dotimes (i level)
      (add-to-universe universe (make-instance 'asteroid)))
    (add-to-universe universe (or fusee (make-instance 'fusee)))
    (add-protection (fusee universe) :seconds 6)))

(defmethod level-cleared-p ((universe universe))
  (< (num-asteroids universe) 1))

(defmethod after ((universe universe) horloge-name &key (seconds 0) do)
  (multiple-value-bind (horloge exists) (gethash horloge-name (horloges universe))
    (if exists
      (when (done horloge)
        (remhash horloge-name (horloges universe))
        (when (functionp do)
          (funcall do)))
      (setf (gethash horloge-name (horloges universe))
            (make-instance 'horloge :seconds seconds)))))

(defmethod update-universe ((universe universe) time-delta)
  (maphash (lambda (name horloge)
             (declare (ignore name))
             (update-horloge horloge time-delta))
           (horloges universe))
  (dolist (objet-general (objet-generals universe))
    (update objet-general time-delta universe))
  ;; start next level 3 seconds after clearing
  (when (level-cleared-p universe)
    (after universe
           'cleared
           :seconds 3
           :do (lambda ()
                 (incf (coeurs universe))
                 (start-next-level universe))))
  ;; restart level 3 seconds after death - game over if no more coeurs
  (unless (fusee universe)
    (after universe
           'death
           :seconds 3
           :do (lambda ()
                 (if (< (coeurs universe) 1)
                   (setf (level universe) 0) ; game over
                   (let ((fusee (make-instance 'fusee)))
                     (add-to-universe universe fusee)
                     (add-protection fusee :seconds 6)))))))

(defmethod add-to-universe :after ((universe universe) (asteroid asteroid))
  (incf (num-asteroids universe)))

(defmethod add-to-universe :after ((universe universe) (fusee fusee))
  (setf (fusee universe) fusee))

(defmethod render-universe ((universe universe) paused)
  (clear-display *black*)
  ;; hud
  (sdl-gfx:draw-string-solid-* (format nil "Level-passe ~d" (level universe))
                               10 10
                               :color *blue*)
  (sdl-gfx:draw-string-solid-* (format nil "coeurs-reste ~d" (coeurs universe))
                               10 (- *map-height* 28)
                               :color *blue*)
  (sdl-gfx:draw-string-solid-* (format nil "point-gagne ~d" (point-gagne universe))
                               (- *map-width* 127) (- *map-height* 28)
                               :color *blue*)
  (sdl-gfx:draw-string-solid-* (format nil
                                       "~a [Q]uitter"
                                       (if (= (level universe) 0)
                                           "[J]ouer"
                                           "[P]ause"))
                               (- *map-width* 127) 10
                               :color *blue*)
  (if (= (level universe) 0)
    ;; title screen
    (progn
      (sdl-gfx:draw-string-solid-* "KivanolaiJEDI"
                                   (round (* 1/2 (- *map-width* 12)))
                                   (round (* 1/4 (- *map-height* 18)))
                                   :color *blue*)
      (sdl-gfx:draw-string-solid-* (format nil
                                           "High point-gagne: ~d"
                                           (high-point-gagne universe))
                                   (round (* 1/2 (- *map-width* 171)))
                                   (round (* 1/2 (- *map-height* 18)))
                                   :color *blue*)
      (sdl-gfx:draw-string-solid-* (format nil "Max level: ~d" (max-level universe))
                                   (round (* 1/2 (- *map-width* 135)))
                                   (round (* 3/4 (- *map-height* 18)))
                                   :color *blue*))
    (progn
      ;; jeux universe
      (set-clip-rect (rectangle :x 0 :y 0 :w *map-width* :h *map-height*)
                     :surface *default-display*)
      (dolist (objet-general (objet-generals universe))
        (render objet-general))
      (set-clip-rect nil :surface *default-display*)
      ;; pause text
      (when paused
        (sdl-gfx:draw-string-solid-* "ATTENDANT___continuez?"
                                     (round (* 1/2 (- *map-width* 54)))
                                     (round (* 1/2 (- *map-height* 18)))
                                     :color *blue*)))))

(defun calc-angle (a b)
  (destructuring-bind (x y) (vector-subtract b a)
    (rad->deg (atan x y))))

(defun main ()
  (with-init ()
    (setf *window*
          (window 960 720
                  :title-caption "trungLISPstarTrekProjetL3"
                  :icon-caption "manolaz2015"))
    (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
    (setf (frame-rate) 60)
    (clear-display *black*)
    (let ((universe (make-instance 'universe))
          (paused nil))
      (with-events ()
        (:quit-event () t)
        (:mouse-motion-event (:x x :y y)
          (when (fusee universe)
            (setf (facing (fusee universe))
                  (calc-angle (pos (fusee universe)) (relative-coords x y)))))
        (:mouse-button-down-event (:x x :y y)
          (when (and (> (level universe) 0)
                     (fusee universe)
                     (not paused))
            (shoot-at (fusee universe) (relative-coords x y) universe)
            ; (cannon (fusee universe) (relative-coords x y))
            (monter-fusee (fusee universe) (relative-coords x y))
            ))
        (:mouse-button-up-event ()
          (when (and (> (level universe) 0)
                     (fusee universe))
            (stop-thrust (fusee universe))))
        (:key-up-event (:key key)
          (case key
            (:sdl-key-escape (push-quit-event))
            (:sdl-key-q (setf (level universe) 0))            
            (:sdl-key-j (if (= (level universe) 0)
                          (progn
                            (setf (point-gagne universe) 0)
                            (setf (coeurs universe) 3)
                            (setf *ticks* (sdl-get-ticks))
                            (start-next-level universe))
                          (setf paused (not paused))))))
        (:idle ()
          (when (and (> (level universe) 0)
                     (not paused))
            (update-universe universe (get-ticks)))
          (render-universe universe paused)
          (update-display))))))
