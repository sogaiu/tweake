# deval - defensive eval

(def atoms
  (invert [:boolean
           :buffer
           :keyword
           :nil
           :number
           :string
           :symbol]))

(defn atom?
  [x]
  (truthy? (get atoms (type x))))

(comment

  (atom? true)
  # =>
  true

  (atom? @"hello")
  # =>
  true

  (atom? :smile!)
  # =>
  true

  (atom? nil)
  # =>
  true

  (atom? 3.1415926535)
  # =>
  true

  (atom? ``must have goofed up somewhere``)
  # =>
  true

  (atom? 'dolphin)
  # =>
  true

  (atom? (fiber/new (fn [_] 1)))
  # =>
  false

  (atom? '[:head :end])
  # =>
  false

  (atom? (map inc [0 1 2]))
  # =>
  false

  (atom? {:a 1})
  # =>
  false

  (atom? @{:x 0})
  # =>
  false

  )

(def colls
  (invert [:array
           :struct
           :table
           :tuple]))

(defn coll?
  [x]
  (truthy? (get colls (type x))))

(comment

  (coll? '[:head :end])
  # =>
  true

  (coll? (map inc [0 1 2]))
  # =>
  true

  (coll? {:a 1})
  # =>
  true

  (coll? @{:x 0})
  # =>
  true

  (coll? true)
  # =>
  false

  (coll? @"hello")
  # =>
  false

  (coll? :smile!)
  # =>
  false

  (coll? nil)
  # =>
  false

  (coll? 3.1415926535)
  # =>
  false

  (coll? ``must have goofed up somewhere``)
  # =>
  false

  (coll? 'dolphin)
  # =>
  false

  (coll? (fiber/new (fn [_] 1)))
  # =>
  false

  )

(def safe-syms
  (invert 
    ~[% %= * *= + ++ += - -- -= -> ->> -?> -?>> / /= < <= = > >=
      accumulate accumulate2 all and any?
      boolean? buffer? bytes?
      case catseq cfunction? cmp comp complement cond count
      dec deep= deep-not= div do
      empty? even? every?
      false? filter find find-index first flatten flatten-into fn
      from-pairs function?
      get get-in group-by
      has-key? has-value?
      idempotent? identity if if-let if-not in inc indexed? interleave
      interpose invert
      keep keys keyword?
      last length lengthable? let
      map mapcat match math/abs max merge merge-into min mod
      nan? nat? neg? next nil? not not= number?
      odd? one? or
      pairs partial partition partition-by pos? postwalk prewalk product
      put put-in
      quasiquote quote
      range reduce reduce2 reverse reverse!
      scan-number seq set short-fn slice some sort sorted sorted-by
      splice string/find string/find-all string/has-prefix?
      string/has-suffix? string? struct? sum symbol?
      table? tabseq toggle true? truthy? tuple? type
      unless unquote update update-in
      values
      walk when when-let while
      zero? zipcoll]))

(defn safe-sym?
  [sym]
  (truthy? (or (get safe-syms sym)
               (string/has-prefix? "$" sym))))

(comment

  (safe-sym? 'all)
  # =>
  true

  (safe-sym? 'os/cd)
  # =>
  false

  )

(defn safe?
  [form]
  (truthy?
    (cond
      (and (tuple? form) (= :parens (tuple/type form)))
      (let [head (get form 0)]
        (and (safe-sym? head)
             (all safe? (tuple/slice form 1))))
      #
      (indexed? form)
      (all safe? form)
      #
      (dictionary? form)
      (all |(let [[k v] $]
              (and (safe? k) (safe? v)))
           (pairs form))
      #
      (symbol? form)
      (safe-sym? form)
      #
      (atom? form)
      true
      #
      false)))

(comment

  (safe? true)
  # =>
  true

  (safe? @"hello")
  # =>
  true

  (safe? :smile!)
  # =>
  true

  (safe? nil)
  # =>
  true

  (safe? 3.1415926535)
  # =>
  true

  (safe? ``must have goofed up somewhere``)
  # =>
  true

  (safe? 'and)
  # =>
  true

  (safe? 'dolphin)
  # =>
  false

  (safe? '[:head :end])
  # =>
  true

  (safe? (map inc [0 1 2]))
  # =>
  true

  (safe? '(map inc [0 1 2]))
  # =>
  true

  (safe? {:a 1})
  # =>
  true

  (safe? @{:x 0})
  # =>
  true

  (safe? (fiber/new (fn [_] 1)))
  # =>
  false

  (safe? '(fiber/new (fn [_] 1)))
  # =>
  false

  (safe? '|(+ $ 80))
  # =>
  true

  (safe? '|(os/cwd))
  # =>
  false

  (safe? '(map pp [:a :b]))
  # =>
  false

  (safe? (peg/compile 1))
  # =>
  false

  )

