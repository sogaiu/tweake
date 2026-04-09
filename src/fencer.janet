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

(def limited-syms
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

(defn limited-sym?
  [sym]
  (truthy? (or (get limited-syms sym)
               (string/has-prefix? "$" sym))))

(comment

  (limited-sym? 'all)
  # =>
  true

  (limited-sym? 'os/cd)
  # =>
  false

  )

(defn limited?
  [form]
  (truthy?
    (cond
      (and (tuple? form) (= :parens (tuple/type form)))
      (let [head (get form 0)]
        (and (limited-sym? head)
             (all limited? (tuple/slice form 1))))
      #
      (indexed? form)
      (all limited? form)
      #
      (dictionary? form)
      (all |(let [[k v] $]
              (and (limited? k) (limited? v)))
           (pairs form))
      #
      (symbol? form)
      (limited-sym? form)
      #
      (atom? form)
      true
      #
      false)))

(comment

  (limited? true)
  # =>
  true

  (limited? @"hello")
  # =>
  true

  (limited? :smile!)
  # =>
  true

  (limited? nil)
  # =>
  true

  (limited? 3.1415926535)
  # =>
  true

  (limited? ``must have goofed up somewhere``)
  # =>
  true

  (limited? 'and)
  # =>
  true

  (limited? 'dolphin)
  # =>
  false

  (limited? '[:head :end])
  # =>
  true

  (limited? (map inc [0 1 2]))
  # =>
  true

  (limited? '(map inc [0 1 2]))
  # =>
  true

  (limited? {:a 1})
  # =>
  true

  (limited? @{:x 0})
  # =>
  true

  (limited? (fiber/new (fn [_] 1)))
  # =>
  false

  (limited? '(fiber/new (fn [_] 1)))
  # =>
  false

  (limited? '|(+ $ 80))
  # =>
  true

  (limited? '|(os/cwd))
  # =>
  false

  (limited? '(map pp [:a :b]))
  # =>
  false

  (limited? (peg/compile 1))
  # =>
  false

  )

