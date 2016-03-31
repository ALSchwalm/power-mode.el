
(setq power-shake-animation-timer nil)
(setq power-shake-timeout-timer nil)

(setq power-shake-default-intensity 5)
(setq power-shake-default-duration "0.15 sec")

(setq power-shake-current-state nil)
(setq power-shake-states '(up down left right))
(setq power-shake-start-position nil)

(defun power-shake-end ()
  (when power-shake-animation-timer
    (cancel-timer power-shake-animation-timer)
    (setq power-shake-animation-timer nil))
  (when power-shake-timeout-timer
    (cancel-timer power-shake-timeout-timer)
    (setq power-shake-timeout-timer nil))
  (when power-shake-start-position
    (set-frame-parameter (selected-frame) 'top (car power-shake-start-position))
    (set-frame-parameter (selected-frame) 'left (cdr power-shake-start-position))
    (setq power-shake-start-position nil)))

(defun power-shake-running ()
  (or power-shake-animation-timer power-shake-timeout-timer))

(defun power-shake-once (intensity)
  (let* ((frame (selected-frame))
         (current-top (frame-parameter frame 'top))
         (current-left (frame-parameter frame 'left)))
    (if (eq power-shake-current-state nil)
        (setq power-shake-current-state power-shake-states)
      (setq power-shake-current-state (-rotate 1 power-shake-current-state)))
    (cond ((eq (car power-shake-current-state) 'up)
           (let ((new-top (+ current-top intensity)))
             (set-frame-parameter frame 'top new-top)))
          ((eq (car power-shake-current-state) 'down)
           (let ((new-top (- current-top intensity)))
             (set-frame-parameter frame 'top new-top)))
          ((eq (car power-shake-current-state) 'left)
           (let ((new-left (+ current-left intensity)))
             (set-frame-parameter frame 'left new-left)))
          ((eq (car power-shake-current-state) 'right)
           (let ((new-left (- current-left intensity)))
             (set-frame-parameter frame 'left new-left)))))
  power-shake-current-state)

(defun power-shake (&optional duration intensity)
  (let ((intensity (if intensity intensity power-shake-default-intensity))
        (duration (if duration duration power-shake-default-duration)))
    (when (power-shake-running)
      (power-shake-end))
    (setq power-shake-start-position
          (cons (frame-parameter (selected-frame) 'top)
                (frame-parameter (selected-frame) 'left)))
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
