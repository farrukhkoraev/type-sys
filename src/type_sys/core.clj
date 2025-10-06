(ns type-sys.core
  (:require
   [type-sys.lambda :as lmb]))

(comment
  let [zero '(fn [f] (fn [x] x))
       one '(fn [f] (fn [x] (f x)))
       two '(fn [f] (fn [x] (f (f x))))

       add '(fn [m] (fn [n] (fn [f] (fn [x] ((m f) ((n f) x))))))
       mul '(fn [m] (fn [n] (fn [f] (fn [x] ((m (n f)) x)))))]

  (= (lmb/normalize (list (list add zero) one)) one)
  (= (lmb/normalize (list (list add one) one)) two)
  (= (lmb/normalize (list (list mul one) one)) one))

