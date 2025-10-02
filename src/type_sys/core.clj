(ns type-sys.core)

; type var like 'a 'b etc
(defn tyvar [x] (x :tvar))

; type constant like int bool
(defn tycon [x] (x :tcon))

; arrow type ('a1-> 'a2 ... -> 'ai->'aj) i.e function type args and return type
(defn tyarr [x] (x :tcon))

(defn type-check [p])

