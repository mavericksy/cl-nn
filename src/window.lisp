;;
(in-package #:cl-nn)
;;
(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defmacro with-window-renderer (title (window renderer) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window
                        :title ,title
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))


(defun run ()
  (with-window-renderer "Let's AI like it's 1989" (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle ()
             ;; Clear screen
             (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
             (sdl2:render-clear renderer)

             ;; Render red filled quad
             (sdl2:with-rects ((fill-rect (/ *screen-width* 4)
                                          (/ *screen-height* 4)
                                          (/ *screen-width* 2)
                                          (/ *screen-height* 2)))
               (sdl2:set-render-draw-color renderer #xFF #x00 #x00 #xFF)
               (sdl2:render-fill-rect renderer fill-rect))

             ;; Render green outlined quad
             (sdl2:with-rects ((outline-rect (round (/ *screen-width* 6))
                                             (round (/ *screen-height* 8))
                                             (round (* 2/3 *screen-width*))
                                             (round (* 2/3 *screen-height*))))
               (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
               (sdl2:render-draw-rect renderer outline-rect))

             ;; Draw blue horizontal line
             (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
             (sdl2:render-draw-line renderer
                                    0
                                    (/ *screen-height* 2)
                                    *screen-width*
                                    (/ *screen-height* 2))

             ;; Draw vertical line of yellow dots
             (sdl2:set-render-draw-color renderer #xFF #xFF #x00 #xFF)
             (loop for i from 0 below *screen-height* by 4
                   do (sdl2::render-draw-point renderer (/ *screen-width* 2) i))

             ;; Update screen
             (sdl2:render-present renderer)))))

(defun test-render-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun test-render-hello (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  ;; H
  (sdl2:render-draw-line renderer 20 20 20 100)
  (sdl2:render-draw-line renderer 20 60 60 60)
  (sdl2:render-draw-line renderer 60 20 60 100)
  ;; E
  (sdl2:render-draw-line renderer 80 20 80 100)
  (sdl2:render-draw-line renderer 80 20 120 20)
  (sdl2:render-draw-line renderer 80 60 120 60)
  (sdl2:render-draw-line renderer 80 100 120 100)
  ;; L
  (sdl2:render-draw-line renderer 140 20 140 100)
  (sdl2:render-draw-line renderer 140 100 180 100)
  ;; L
  (sdl2:render-draw-line renderer 200 20 200 100)
  (sdl2:render-draw-line renderer 200 100 240 100)
  ;; O
  (sdl2:render-draw-line renderer 260 20 260 100)
  (sdl2:render-draw-line renderer 260 100 300 100)
  (sdl2:render-draw-line renderer 300 20 300 100)
  (sdl2:render-draw-line renderer 260 20 300 20))

(defun test-render-lines (renderer)
  (sdl2:with-points ((a 200 200)
                     (b 300 400)
                     (c 400 200))
    (sdl2:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-lines renderer points num))))

(defun test-render-points (renderer)
  (sdl2:with-points ((a (random 800) (random 800))
                     (b (random 800) (random 800))
                     (c (random 800) (random 800)))
    (sdl2:set-render-draw-color renderer 0 255 0 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-points renderer points num))))

(defun test-render-rect (renderer)
  (sdl2:render-draw-rect renderer (sdl2:make-rect 400 400 35 35)))

(defun test-render-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :for y :upto 5
                   :collect (sdl2:make-rect (+ 400 (* x 10)) (+ 200 (* y 10)) 8 8)))
    (sdl2:render-draw-rects renderer rects num)))

(defun test-render-fill-rect (renderer)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 445 400 35 35)))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :collect (sdl2:make-rect (+ 500 (* x 10)) 400 8 8)))
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (sdl2:render-fill-rects renderer rects num)))
;;
(defun renderer-test ()
  (with-window-renderer "Let's AI like it's 1989" (window renderer)
    (sdl2:with-event-loop (:method :poll)
      (:keyup
       (:keysym keysym)
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)))
      (:idle
       ()
       (test-render-clear renderer)
       (test-render-hello renderer)
       (test-render-lines renderer)
       (test-render-points renderer)
       (test-render-rect renderer)
       (test-render-rects renderer)
       (test-render-fill-rect renderer)
       (test-render-fill-rects renderer)
       (sdl2:render-present renderer)
       (sdl2:delay 33))
      (:quit () t))))
