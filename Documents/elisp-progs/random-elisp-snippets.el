(defun my/heart () "evals to an ASCII art heart"
       (let ((s "\n")
             (y 1.5))
         (while (>= y -1.0)
           (let ((x -1.0))
             (while (<= x 1.0)
               (setq s (concat s
                               (cond
                                ((<= (+ (expt x 2) (expt (- y (expt (expt x 2) (/ 1.0 3.0))) 2)) 1) "*")
                                (" "))))
               (setq x (+ x 0.1))))
           (setq s (concat s "\n"))
           (setq y (- y 0.2)))
         s))
