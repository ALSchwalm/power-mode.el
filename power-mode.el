
(setq power-shake-animation-timer nil)
(setq power-shake-timeout-timer nil)

(setq power-shake-default-intensity 5)
(setq power-shake-default-duration "0.15 sec")

(setq power-shake-current-state nil)
(setq power-shake-states '(up down left right))
(setq power-shake-start-position nil)

(defun from-frame-position (frame-position)
  "Convert a frame position to a pixel position"
  (if (integerp frame-position)
      frame-position
    (car (cdr frame-position))))

(defun to-frame-position (px-position)
  "Convert a pixel position to a frame position"
  (if (< px-position 0)
      (list '+ px-position)
    px-position))

(defun power-shake-end ()
  (when power-shake-animation-timer
    (cancel-timer power-shake-animation-timer)
    (setq power-shake-animation-timer nil))
  (when power-shake-timeout-timer
    (cancel-timer power-shake-timeout-timer)
    (setq power-shake-timeout-timer nil))
  (when power-shake-start-position
    (set-frame-parameter nil 'top (car power-shake-start-position))
    (set-frame-parameter nil 'left (cdr power-shake-start-position))
    (setq power-shake-start-position nil)))

(defun power-shake-running ()
  (or power-shake-animation-timer power-shake-timeout-timer))

(defun power-shake-once (intensity)
  (let ((current-top (from-frame-position (frame-parameter nil 'top)))
        (current-left (from-frame-position (frame-parameter nil 'left))))
    (if (eq power-shake-current-state nil)
        (setq power-shake-current-state power-shake-states)
      (setq power-shake-current-state (-rotate 1 power-shake-current-state)))
    (cond ((eq (car power-shake-current-state) 'up)
           (set-frame-parameter nil 'top (to-frame-position (+ current-top intensity))))
          ((eq (car power-shake-current-state) 'down)
           (set-frame-parameter nil 'top (to-frame-position (- current-top intensity))))
          ((eq (car power-shake-current-state) 'left)
           (set-frame-parameter nil 'left (to-frame-position (+ current-left intensity))))
          ((eq (car power-shake-current-state) 'right)
           (set-frame-parameter nil 'left (to-frame-position (- current-left intensity))))))
  power-shake-current-state)

(defun power-shake (&optional duration intensity)
  (let ((intensity (or intensity power-shake-default-intensity))
        (duration (or duration power-shake-default-duration)))
    (when (power-shake-running)
      (power-shake-end))
    (setq power-shake-start-position
          (cons (frame-parameter nil 'top)
                (frame-parameter nil 'left)))
    (setq power-shake-animation-timer (run-at-time "0 sec"
                                                   0.04
                                                   'power-shake-once
                                                   intensity))
    (setq power-shake-timeout-timer (run-at-time duration
                                                 nil
                                                 'power-shake-end))))

(defun power-shake-hook (start end len)
  "A hook that can be added to add a 'shaking' effect after a keypress.

NOTE: autocomplete-mode may interfere with this hook."
  (power-shake))

(defun power-smoke (center &optional intensity)
  "TODO: Implement this")

;; To run, evaluate:
;;  (add-hook 'after-change-functions 'power-shake-hook nil t)
;;
;; To Stop:
;;  (remove-hook 'after-change-functions 'power-shake-hook)
;; or
;;  (setq after-change-functions (cdr after-change-functions))
