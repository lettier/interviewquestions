;;;; David Lettier
;;;; (C) 2015.
;;;; http://www.lettier.com/

;;; Implement the least common multiple
;;; function for two integers.

;;; To run: $ cat lcm.clj | lein exec

(
  defn lcm [x y]
  (
    if (and (zero? x) (zero? y))
      0
    (
      if (or (zero? x) (zero? y))
        nil
        (
          first
          (
            filter #(zero? (mod % y))
              (
                for [a (range 1 (+ 1 y))]
                (* a x)
              )
          )
        )
    )
  )
)

(println (= (lcm 0 3) nil))
(println (= (lcm 0 0) 0))
(println (= (lcm 5 3) 15))
(println (= (lcm 1 10) 10))
(println (= (lcm 20 10) 20))
(println (= (lcm (lcm 20 10) (lcm 1 10)) 20))
