(ns speculoos.patterns
  (:require [clojure.core.match :as m]
            [cljs.core.match :as cm]
            [clojure.core.match.protocols :as mp]
            [clojure.set :as set]
            [speculoos.utils :as u]
            [speculoos.specs :as ss]
            [speculoos.state :as state :refer [*cljs?*]]))

(do :extra-patterns-predicates

    (defn coercion-shorthand-pattern? [xs]
      (and (= 2 (count xs))
           (qualified-keyword? (first xs))))

    (defn validation-shorthand-pattern? [xs]
      (and (= 2 (count xs))
           (qualified-keyword? (second xs))))

    (defn coercion-pattern? [xs]
      (and (= 3 (count xs))
           (= :< (second xs))))

    (defn validation-pattern? [xs]
      (and (= 3 (count xs))
           (= :- (second xs)))))

(do :core.match.extension

    (extend-protocol mp/ISyntaxTag
      clojure.lang.ISeq
      (syntax-tag [xs]
        (cond
          (validation-pattern? xs) ::spec
          (coercion-pattern? xs) ::coerce
          (validation-shorthand-pattern? xs) ::spec-shorthand
          (coercion-shorthand-pattern? xs) ::coerce-shorthand
          (state/registered-type? (first xs)) ::type
          (u/predicate-symbol? (first xs)) ::pred
          :else ::m/seq)))

    (defmethod m/emit-pattern ::type [[c & ms]]
      (m/emit-pattern (list (zipmap (->> (state/registered-type? c) :fields (map keyword)) ms)
                            :guard `(fn [x#] (~(u/mksym c "?") x#)))))

    (defmethod m/emit-pattern ::pred [[c x]]
      (m/emit-pattern (list x :guard c)))

    (defmethod m/emit-pattern ::spec [[x _ s]]
      (m/emit-pattern (list x :guard `(partial ~(ss/spec-sym "valid?") ~s))))

    (defmethod m/emit-pattern ::coerce [[x _ s]]
      (m/emit-pattern (list x :<< (ss/conformer-form s))))

    (defmethod m/emit-pattern ::spec-shorthand [[x s]]
      (m/emit-pattern (list x :guard `(partial ~(ss/spec-sym "valid?") ~s))))

    (defmethod m/emit-pattern ::coerce-shorthand [[s x]]
      (m/emit-pattern (list x :<< (ss/conformer-form s))))

    (defmethod m/emit-pattern-for-syntax [:default :as]
      [[p _ sym]] (vary-meta (m/emit-pattern p) merge {:as sym}))

    (comment ;; the following definition is due to
      (m/match [1] [(x :<< inc)] x)
      ;;=> "No method in multimethod 'to-source' for dispatch value: null"
      )

    (defmethod m/to-source nil [x o]
      (if (m/wildcard-pattern? x)
        x
        (throw (Exception. (str "No method in multimethod 'to-source' for dispatch value: null " x o)))))

    ;; slightly modified version of core.match version,
    ;; in order to not consider constructors symbol as duplicate wildcards
    (defn wildcards-and-duplicates
      "Returns a vector of two elements: the set of all wildcards and the
     set of duplicate wildcards.  The underbar _ is excluded from both."
      [patterns]
      (loop [remaining patterns seen #{} dups #{}]
        (if-let [patterns (seq remaining)]
          (let [pat (first patterns)
                pats (rest patterns)]
            (cond
              (or (= pat '_) (= pat '&))
              (recur pats seen dups)

              (symbol? pat)
              (if (contains? seen pat)
                (recur pats seen (conj dups pat))
                (recur pats (conj seen pat) dups))

              (vector? pat)
              (recur (concat pats pat) seen dups)

              (map? pat)
              (recur (concat pats (vals pat)) seen dups)

              (seq? pat)
              (cond
                (= (first pat) 'quote)
                (recur pats seen dups)

                ;; here ---------------------------------------
                (or
                  (state/registered-type? (first pat))
                  (u/predicate-symbol? (first pat)))
                (recur (concat pats (next pat)) seen dups)
                ;; --------------------------------------------

                (= (first pat) :or)
                (let [wds (map wildcards-and-duplicates
                               (map list (take-nth 2 pat)))
                      mseen (apply set/union (map first wds))]
                  (recur pats (set/union seen mseen)
                         (apply set/union dups
                                (set/intersection seen mseen)
                                (map second wds))))

                (= (second pat) :as)
                (recur (concat pats (take-nth 2 pat)) seen dups)

                :else
                (recur (conj pats (first pat)) seen dups))
              :else
              (recur pats seen dups)))
          [seen dups])))

    ;; purely functional
    (alter-var-root #'m/wildcards-and-duplicates
                    (fn [_] wildcards-and-duplicates)))

(do :match-fns

    (defmacro fm [x & xs]
      (let [[nam [x & xs]] (if (symbol? x) [x xs] [(gensym) (cons x xs)])
            [return-spec clauses] (if (qualified-keyword? x) [x xs] [nil (cons x xs)])
            clauses (partition 2 clauses)
            arity (comp count first)
            variadic-pattern? #(-> % reverse next first (= '&))
            variadic-clause? (comp variadic-pattern? first)
            fixed-clauses (remove variadic-clause? clauses)
            variadic-clauses (filter variadic-clause? clauses)
            by-arity (group-by arity fixed-clauses)
            variadic-arity (and (seq variadic-clauses)
                                (-> variadic-clauses first first count dec))

            wrap-return
            (fn [expr]
              (if return-spec
                `(or ~(ss/validator-form return-spec expr)
                     ~(u/error-form (str *ns*) "/"
                                    (name nam) ":\ninvalid return value: "
                                    `(~'pr-str ~expr) " is not a valid " return-spec))
                expr))

            variadic-case
            (fn self [[pat expr]]
              (let [lpat (last pat)
                    blpat (vec (drop-last 2 pat))
                    lpat'
                    (if-not (seq? lpat)
                      lpat
                      (condp = (u/prob 'syntag pat (mp/syntax-tag lpat))
                        ::spec (list (first lpat) :- (list (ss/spec-sym "*") (nth lpat 2)))
                        ::coerce (list (first lpat) :< (list (ss/spec-sym "*") (nth lpat 2)))
                        ::spec-shorthand (list (first lpat) :- (list (ss/spec-sym "*") (second lpat)))
                        ::coerce-shorthand (list (second lpat) :< (list (ss/spec-sym "*") (first lpat)))
                        lpat))]
                [(conj blpat lpat') (wrap-return expr)]))]

        (binding [*cljs?* (and (:ns &env) true)]

          (when variadic-arity

            (assert (apply = (map arity variadic-clauses))
                    "variadic patterns should be equals in length")

            (assert (every? (partial > variadic-arity) (keys by-arity))
                    (str "fixed arity > variadic arity"
                         (take-nth 2 clauses))))

          (clojure.walk/macroexpand-all

            `(fn ~nam

               ;; fixed clauses
               ~@(map (fn [[argv clauses]]
                        `(~argv ~(wrap-return
                                   `(~(if *cljs?* `cm/match `m/match)
                                      ~argv ~@(apply concat clauses)))))
                      (u/map-keys (fn [n] (vec (repeatedly n gensym)))
                                  by-arity))

               ;; variadic clauses
               ~@(when variadic-arity
                   (let [argv
                         (-> (dec variadic-arity)
                             (repeatedly gensym)
                             (concat ['& (gensym)])
                             vec)
                         variadic-clauses
                         (map variadic-case variadic-clauses)]

                     [`(~argv (~(if *cljs?* `cm/match `m/match)
                                ~(vec (remove '#{&} argv))
                                ~@(apply concat variadic-clauses)))])))))))

    (defmacro defm
      "a simple pattern matched function"
      [name & clauses]
      `(def ~name (fm ~name ~@clauses))))