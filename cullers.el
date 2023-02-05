(require 'color)

(defun culler-ascii-to-hexadecimal(x)
  (cond ((and (<= x 90) (>= x 65))
         (- x (- 65 10)))
        ((and (<= x 57) (>= x 48))
         (- x 48))
        (t 0)))

(defun pair-bond(x)
  "Every car needs its cdr. Creates a list of conses from a given list."
  (if (null x)
      x
    (let ((pair (cons (car x) (cadr x))))
      (cons pair (pair-bond (cddr x))))))

(defun culler-hex-to-rgb(hex)
  "Convert a hex to rgb value"
  (let ((pairs (pair-bond (coerce (string-trim-left hex "#") 'list))))
    (mapcar #'(lambda(x)
                (/ (+ (* 16 (culler-ascii-to-hexadecimal (car x)))
                   (culler-ascii-to-hexadecimal (cdr x))) 255.0)) pairs)))

;; These next two functions could probably be replaced by a macro
;; that evaluates to the trio of nth calls.
(defun culler-rgb-to-hsl(rgb)
  "Wrapper for color.elc's function of a similar name.
Lets the function accept a list"
  (color-rgb-to-hsl (nth 0 rgb)
                    (nth 1 rgb)
                    (nth 2 rgb)))

(defun culler-darken-hsl(hsl percent)
  "Wrapper for color.elc's function of a similar name.
Lets the function accept a list"
  (color-darken-hsl (nth 0 hsl)
                    (nth 1 hsl)
                    (nth 2 hsl) percent))

(defun culler-lighten-hsl(hsl percent)
  "Wrapper for color.elc's function of a similar name."
  (color-lighten-hsl (nth 0 hsl)
                     (nth 1 hsl)
                     (nth 2 hsl) percent))

(defun culler-hsl-to-hex(hsl)
  "Wrapper for color.elc's function of a similar name."
  (let ((rgb (color-hsl-to-rgb (nth 0 hsl)
                               (nth 1 hsl)
                               (nth 2 hsl))))
    (color-rgb-to-hex (nth 0 rgb)
                      (nth 1 rgb)
                      (nth 2 rgb) 2)))
(provide 'cullers)
