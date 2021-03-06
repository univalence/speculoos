(ns speculoos.patterns
  (:require [clojure.core.match :as m]
            [cljs.core.match :as cm]
            [clojure.core.match.protocols :as mp]
            [clojure.set :as set]
            [speculoos.utils :as u]
            [speculoos.specs :as ss]
            [speculoos.state :as state :refer [if-cljs]]))

(defn qualified-type-symbol [x]
  (when-let [qs (state/qualify-symbol x)]
    (u/dotjoin (namespace qs) (name qs))))

(defn known-type? [s]
  (when-let [qs (qualified-type-symbol s)]
    (state/registered-type? qs)))

(do :extra-patterns-predicates

    (defn spec-shorthand-pattern? [xs]
      (and (seq? xs) (= 2 (count xs))
           (qualified-keyword? (second xs))))

    (defn spec-pattern? [xs]
      (and (seq? xs) (= 3 (count xs))
           (= :- (second xs)))))

(do :core.match.extension

    (extend-protocol mp/ISyntaxTag
      clojure.lang.ISeq
      (syntax-tag [xs]
        (cond
          (spec-pattern? xs) ::spec
          (spec-shorthand-pattern? xs) ::spec-shorthand
          (known-type? (first xs)) ::type
          (u/predicate-symbol? (first xs)) ::pred
          :else ::m/seq)))

    (defmethod m/emit-pattern ::type [[c & ms]]
      (let [{:keys [fields-names positional arity]} (known-type? c)
            fields-keywords (map keyword fields-names)
            positional-extension (when (and arity (> (count ms) arity)) (apply hash-map (drop arity ms)))
            pred-sym (symbol (namespace c) (str (name c) "?"))]
        (m/emit-pattern (list (if positional
                                (merge (zipmap fields-keywords ms) positional-extension)
                                (merge (zipmap fields-keywords (repeat nil))
                                       (apply hash-map ms)))
                              :guard `(fn [x#] (~pred-sym x#))))))

    (defmethod m/emit-pattern ::pred [[c x]]
      (m/emit-pattern (list x :guard c)))

    (defmethod m/emit-pattern ::spec [[x _ s]]
      (m/emit-pattern (list (list x :guard (if-cljs 'cljs.core/identity clojure.core/identity))
                            :<< (ss/conformer-form s))))

    (defmethod m/emit-pattern ::spec-shorthand [[x s]]
      (m/emit-pattern (list (list x :guard (if-cljs 'cljs.core/identity clojure.core/identity))
                            :<< (ss/conformer-form s))))

    (defmethod m/emit-pattern-for-syntax [:default :as] [[p _ sym]]
      (vary-meta (m/emit-pattern p) merge {:as sym}))

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
                  (known-type? (first pat))
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

    (defn expand-match-form [argv clauses & [wild-clause]]
      (macroexpand `(~(if-cljs `cm/match `m/match)
                      ~argv ~@(doall (apply concat clauses))
                      ~@wild-clause)))


    (defmacro fm [& body]
      (u/expanding
        (let [[x & xs] (u/walk-dotsyms body)
              [nam [x & xs]] (if (symbol? x) [x xs] [(gensym) (cons x xs)])
              [return-spec clauses] (cond (qualified-keyword? x) [x xs]
                                          (= :- x) [(first xs) (next xs)]
                                          :else [nil (cons x xs)])

              wildcard? (partial = :else)

              pattern? #(or (vector? %) (wildcard? %))

              clauses
              (cond

                ;; regular polyarity fn syntax: (pat1 & body1) (pat2 & body2) ...
                (every? seq? clauses)
                (map (fn [[p & bod :as all]]
                       (let [cnt (count bod)]
                         (if (= 1 cnt) all (list p (list* 'do bod))))
                       clauses))

                ;; match syntax: pat1 expr1 pat2 expr2 ...
                (every? pattern? (take-nth 2 clauses))
                (partition 2 clauses)

                ;; single arity: pattern & body
                (vector? (first clauses))
                (if (= 2 (count clauses))
                  (list clauses)
                  (list (list (first clauses) (list* 'do (next clauses))))))


              wild-clause? (comp wildcard? first)

              wild-clause (first (filter wild-clause? clauses))

              clauses (remove wild-clause? clauses)

              wrap-return
              (fn [expr]
                (if return-spec
                  `(or ~(ss/validator-form return-spec expr)
                       ~(u/error-form (str *ns*) "/"
                                      (name nam) ":\ninvalid return value: "
                                      `(~'pr-str ~expr) " is not a valid " return-spec))
                  expr))

              ;; trying to compile it without core.match if possible (resulting in better performances)
              fn-form (when-not wild-clause
                        (try (macroexpand `(fn ~nam ~@(map (fn [[pat expr]] (list pat (wrap-return expr))) clauses)))
                             (catch Exception _ nil)))]


          (or fn-form
              ;; else do the heavy stuff
              (let [arity (comp count first)
                    variadic-pattern? #(and (vector? %) (-> % reverse next first (= '&)))
                    variadic-clause? (comp variadic-pattern? first)
                    fixed-clauses (remove variadic-clause? clauses)
                    variadic-clauses (filter variadic-clause? clauses)
                    by-arity (group-by arity fixed-clauses)
                    variadic-arity (and (seq variadic-clauses)
                                        (-> variadic-clauses first first count dec))

                    variadic-case
                    (fn self [[pat expr]]
                      (let [lpat (last pat)
                            blpat (vec (drop-last 2 pat))
                            lpat'
                            (if-not (seq? lpat)
                              lpat
                              (condp = (mp/syntax-tag lpat)
                                ::spec (list (first lpat) :- (list (ss/spec-sym "*") (nth lpat 2)))
                                ::spec-shorthand (list (first lpat) :- (list (ss/spec-sym "*") (second lpat)))
                                lpat))]
                        [(conj blpat lpat') (wrap-return expr)]))]

                (do ;state/binding-expansion-dynamic-vars

                  (when variadic-arity

                    (assert (apply = (map arity variadic-clauses))
                            "variadic patterns should be equals in length")

                    (assert (every? (partial > variadic-arity) (keys by-arity))
                            (str "fixed arity > variadic arity"
                                 (take-nth 2 clauses))))

                  `(fn ~nam

                     ;; fixed clauses
                     ~@(mapv (fn [[argv clauses]]
                               `(~argv ~(wrap-return
                                          (expand-match-form argv clauses wild-clause))))
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
                               (mapv variadic-case variadic-clauses)]

                           [(list argv
                                  (expand-match-form
                                    (vec (remove '#{&} argv))
                                    variadic-clauses
                                    wild-clause))])))))))))

    (defmacro defm
      "a simple pattern matched function"
      [name & clauses]
      `(u/dof ~name (fm ~name ~@clauses)))

    (comment (macroexpand '(fm ::num [a] (+ a a)))
             (macroexpand '(fn [a] a))
             (macroexpand '(fm ::num [{a :a}] a [a b] b))
             (macroexpand '(fm :- number? [a] :a [a b] :b))))



