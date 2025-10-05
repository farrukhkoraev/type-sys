(ns type-sys.lambda
  (:require [clojure.spec.alpha :as s]))

; α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω
;Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω

(defn empty-ctx [] {})

;type = a, b...
;fn-type = [a a]
;var = name | [name type] 
;lambda = (fn [[x arg-type] fn-type] M) 
;appl = (M N)

(defn lambda? [term]
  (and (list? term) (= (first term) 'fn)))

(defn param [term]
  (cond
    (symbol? term) term
    (= (first term) 'fn) (first (second term))
    :else nil))

(defn body [term]
  (cond
    (symbol? term) term
    (lambda? term) (last term)
    :else nil))

(defn free-vars [term]
  (if (symbol? term) #{term}
      (if (lambda? term)
        (disj (free-vars (body term)) (param term))
        (clojure.set/union
         (free-vars (first term))
         (free-vars (second term))))))

(defn bound-vars [term]
  (if (symbol? term) #{}
      (if (= (first term) 'fn)
        (conj (bound-vars (body term))  (param term))
        (clojure.set/union
         (bound-vars (first term))
         (bound-vars (second term))))))

(free-vars '(fn [x] (fn [y] z)))

; (fn [[x a] [a a]] x)
(defn fresh-var [used name]
  (loop [n 0]
    (let [cand (symbol (str name n))]
      (if (contains? used cand)
        (recur (+ n 1))
        cand))))

(defn alpha-subst [term x y]
  (if (symbol? term)
    (if (= term x) y term)
    (if (= (first term) 'fn)
      (if (= (param term) x)
        (list 'fn [y] (alpha-subst (last term) x y))
        (list 'fn [(param term)] (alpha-subst (last term) x y)))
      (list (alpha-subst (first term) x y)
            (alpha-subst (second term) x y)))))

(defn alpha-eqv
  ([term]
   (let [x (param term)]
     (cond
       (symbol? term) (alpha-subst term x (fresh-var #{x} x))
       (lambda? term) (let [b (alpha-eqv (body term))
                            t (list 'fn (second term) b)]
                        (alpha-subst t x (fresh-var #{x} x)))
       :else term))))

;(alpha-eqv '(fn [x] (fn [y] (x z))))

(defn substitute [term x M]
  (cond
    (symbol? term) (if (= term x) M term)
    (lambda? term) (let [[_ p b] (alpha-eqv term)]
                     (cond
                       (= (param term) x) term
                       (not (contains?
                             (free-vars M)
                             (param term))) (list
                                             'fn
                                             [(param term)]
                                             (substitute (last term) x M))
                       :else (list 'fn p (substitute b x M))))
    :else (list (substitute (first term) x M)
                (substitute (second term) x M))))

(free-vars (second '(x y)))

; (let [term '(fn [[x a] a] y)
;       x 'y
;       y 'z]
;   (if (= (first term) 'fn)
;     (let [p (first (second term))
;           t (second (second term))
;           b (last term)]
;       (if (= (first p) x)) term
;
;       )
;     nil))

(defn beta-red [term]
  (cond
    (symbol? term) term
    (lambda? term) (list 'fn (second term) (beta-red (last term)))
    :else (let [[func arg] term]
            (if-not (lambda? func)
              term
              (substitute (body func) (param func) arg)))))

(defn normalize [term]
  (loop [cur term]
    (let [next (beta-red cur)]
      (if (= cur next) cur
          (recur next)))))

(defn check-type [term ctxt])
(defn infere-type [term ctxt])

(def zero '(fn [f] (fn [x] x)))
(def one '(fn [f] (fn [x] (f x))))

(def add '(fn [n] (fn [m] (fn [f] (fn [x] ((n f) ((m f) x)))))))

(beta-red (list add (alpha-eqv one)))
