(def add (lambda (a b)
  (+ a b)))

(def debug_pass (lambda (value)
  (debug value)
  value))

(def main (lambda ()
  (debug_pass (* 10 (debug_pass (add 3 4))))))
