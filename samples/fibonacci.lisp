(def fib (lambda (a b n)
  (if n
    (do
      (debug a)
      (fib b (+ a b) (- n 1)))
    a)))

(def main (lambda ()
  (fib 1 1 15)))