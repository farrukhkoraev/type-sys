(ns type-sys.lambda
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]))

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

(defn appl? [term]
  (and (list? term) (= (count term) 2)))

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

(defn fresh-var [used name]
  (loop [n 0]
    (let [cand (symbol (str name n))]
      (if (contains? used cand)
        (recur (+ n 1))
        cand))))

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

(defn rename [term x y]
  (cond
    (symbol? term) (if (= term x) y term)
    (lambda? term) (if (= (param term) x)
                     (list 'fn [y] (rename (body term) x y))
                     (list 'fn [(param term)] (rename (body term) x y)))
    :else (list (rename (first term) x y)
                (rename (second term) x y))))

(defn substitute [term x M]
  (cond
    (symbol? term) (if (= term x) M term)
    (lambda? term)
    (let [[_ [p] b] term
          M-fv (free-vars M)]
      (cond
        (= p x) term
        (contains? M-fv p) (let [used (set/union M-fv (free-vars b) #{x})
                                 new-p (fresh-var used p)
                                 new-b (rename b p new-p)]
                             (list 'fn [new-p] (substitute new-b x M)))
        :else (list 'fn [p] (substitute b x M))))
    :else (list (substitute (first term) x M)
                (substitute (second term) x M))))

(defn beta-step [term]
  (cond
    (and (appl? term)
         (lambda? (first term))) (let [[M N] term]
                                   (substitute (body M) (param M) N))
    (appl? term) (let [[M N] term
                       M' (beta-step M)]
                   (if M'
                     (list M' N)
                     (if-let [N' (beta-step N)] (list M N') nil)))
    (lambda? term) (if-let [b' (beta-step (body term))]
                     (list 'fn (second term) b')
                     nil)
    :else nil))

(defn normalize [term]
  (loop [cur term]
    (let [next (beta-step cur)]
      (if (or (nil? next) (= cur next)) cur
          (recur next)))))

