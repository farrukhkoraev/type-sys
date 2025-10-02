(ns type-sys.lambda
  (:require [clojure.spec.alpha :as s]))

; α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω
;Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω

(defn empty-ctx [] {})

(defn t-var [name]
  {:tag :var
   :name name})

(defn t-abstr
  ([[p t] body tt]
   {:tag :abstr
    :param [p t]
    :body body
    :type tt})
  ([[p t] body] (t-abstr [p t] body nil)))

(defn t-appl [f arg]
  {:tag :appl
   :abstr f
   :arg arg})

(defn free-vars [term]
  (case (:tag term)
    :variable #{(:name term)}
    :lambda (disj (free-vars (:body term)) (:param term))
    :application (clojure.set/union
                  (free-vars (:lambda term))
                  (free-vars (:arg term)))))

(defn bound-vars [term]
  (case (:tag term)
    :variable #{}
    :lambda (conj
             (bound-vars (:body term))
             (:param term))
    :application (clojure.set/union
                  (bound-vars (:lambda term))
                  (bound-vars (:arg term)))))
(defn substitute [term-old x term-new]
  (case (:tag term-old)
    :var (if (= (:name term-old) x) term-new term-old)
    :abstr (let [p (:param term-old)
                 t (:type term-old)
                 b (:body term-old)]
             (if (= p x) term-old
                 (t-abstr p t (substitute b x term-new))))
    :appl (t-appl
           (substitute (:abstr term-old) x term-new)
           (substitute (:arg term-old) x term-new))))

(defn beta-red [term]
  (case (:tag term)
    :var term
    :abstr (t-abstr (:param term) (:type term) (beta-red (:body term)))
    :appl (let [f (:abstr term)
                x (:arg term)]
            (substitute (:body f) (:param f) x))))

(defn normalize [term]
  (loop [cur term]
    (let [next (beta-red cur)]
      (if (= cur next) cur
          (recur next)))))

(defn to-str [term]
  (case (:tag term)
    :var (str (:name term))
    :abstr (let [[p t] (:param term)
                 b (:body term)
                 tt (:type term)]
             (if (nil? tt)
               (str "(" "λ" p ":" t "." (to-str b) ")")
               (str "(" "λ" p ":" t "." (to-str b) ": " tt ")")))
    :appl (str (to-str (:abstr term)) " " (to-str (:arg term)))))

(to-str (t-appl (t-abstr ['x 'α] (t-var 'x) 'α->α) (t-var 'y)))
