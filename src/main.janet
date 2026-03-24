(import ./args :as a)
(import ./jipper :as j)

(def version "DEVEL")

(def usage
  `````
  Usage: tweake <file> <path> <value>

         tweake [-h|--help]|[-v|--version]

  Modify and display `.jdn` content [1].

  Parameters:

    <file>                 path to `.jdn` file
    <path>                 describes "path" to target to replace
    <value>                value to replace with

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Show content based on `.niche.jdn` which excludes a file:

    $ tweake .niche.jdn ':excludes' '["src/args.janet"]'

    Show content based on `bundle/info.jdn` with new name:

    $ tweake bundle/info.jdn ':name' '"cooler-name"'

    Show content based on `bundle/info.jdn` with new tag for a
    vendored dependency:

    $ tweake bundle/info.jdn \
             ':vendored |(= (get $ :name) "niche") :tag' \
             '"shiny-new-tag"'

  ---

  [1] It's only the content which is modified and displayed, i.e.
      files are not modified in-place,

  `````)

########################################################################

(comment

  (def path-str ":vendored 2 :tag")

  (def value `"7caf81f636bb97104aada6544219733b3c86badf"`)

  # bundle/info.jdn
  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (def path (parse-all path-str))
  # =>
  @[:vendored 2 :tag]

  (def step-0 (get ds (get path 0)))
  # =>
  [{:name "some-bundle-bits"
    :paths [["sbb.janet" "bundle/"]]
    :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
    :url "https://github.com/sogaiu/some-bundle-bits"}
   {:name "jell"
    :paths [["jell" "bin/"]]
    :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
    :url "https://github.com/sogaiu/jell"}
   {:name "niche"
    :paths [["niche.janet" "bin/"]]
    :tag "7caf81f636bb97104aada6544219733b3c86badf"
    :url "https://github.com/sogaiu/niche"}]

  (def step-1 (get step-0 (get path 1)))
  # =>
  {:name "niche"
   :paths [["niche.janet" "bin/"]]
   :tag "7caf81f636bb97104aada6544219733b3c86badf"
   :url "https://github.com/sogaiu/niche"}

  (def step-2 (get step-1 (get path 2)))
  # =>
  "7caf81f636bb97104aada6544219733b3c86badf"

  )

(defn get-via-path
  [ds path]
  (var context ds)
  (def new-path @[])
  (each step path
    (cond
      (or (keyword? step) (nat? step))
      (do
        (array/push new-path step)
        (set context (get context step)))
      #
      (function? step)
      (do
        (assertf (indexed? context)
                 "expected indexed, found: %n" context)
        (var capture nil)
        (eachp [i elt] context
          (when (step elt)
            (array/push new-path i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target item in: %n" context))
        #
        (set context capture))
      #
      (errorf "unexpected value: %n" step)))
  #
  [context new-path])

(comment

  (def src
    ``
    {:name "ghost"
     :url "https://github.com/sogaiu/ghost"
     :repo "git+https://github.com/sogaiu/ghost"
     :vendored
     [{:name "some-bundle-bits"
       :url "https://github.com/sogaiu/some-bundle-bits"
       :tag "70409ab34b7c762118e0d31efaa4d2f01d46fecf"
       :paths [["sbb.janet" "bundle/"]]}
      {:name "jell"
       :url "https://github.com/sogaiu/jell"
       :tag "718464b8b94971fd8f9e984ca81d35e2c859546e"
       :paths [["jell" "bin/"]]}
      {:name "niche"
       :url "https://github.com/sogaiu/niche"
       :tag "7caf81f636bb97104aada6544219733b3c86badf"
       :paths [["niche.janet" "bin/"]]}]}
    ``)

  (def ds (parse src))

  (get-via-path ds [:vendored 2 :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  (get-via-path ds [:vendored
                    |(= (get $ :name) "niche")
                    :tag])
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  )

########################################################################

# XXX: not sure if should check whether node for zloc is:
#
#      :table, :struct
(defn get-via-key
  ``
  Move to the location of `key`'s associated value in the Janet
  dictionary associated with `zloc`.  Currently, `key` can be a Janet
  keyword (e.g. `:ant`) or a Janet natural number (e.g. `0` or `2`).

  If no such key exists, return nil.  Otherwise return the determined
  location.
  ``

  [zloc key]
  (assertf (or (keyword? key) (nat? key))
           "expected keyword or natural number, found: %n" key)
  #
  (def first-key-zloc (j/down-skip-wsc zloc))
  (when first-key-zloc
    (def key-str (string (if (keyword? key) ":" "") key))
    (var cur-zloc first-key-zloc)
    (while (def cur-node (j/node cur-zloc))
      (when (match cur-node
              [:keyword _ (@ key-str)]
              true
              #
              [:number _ (@ key-str)]
              true
              #
              false)
        (break))
      (set cur-zloc (-> cur-zloc
                        j/right-skip-wsc
                        j/right-skip-wsc)))
    #
    (when cur-zloc
      (def value-zloc (j/right-skip-wsc cur-zloc))
      (assertf value-zloc "failed to find value for key: %n" key)
      #
      value-zloc)))

(comment

  (-> (j/par "{:a 1}")
      j/zip-down
      (get-via-key :a)
      j/node)
  # =>
  [:number @{:bc 5 :bl 1 :bp 4 :ec 6 :el 1 :ep 5} "1"]

  (-> (j/par `{-1 "minus-one" 0 "zero"}`)
      j/zip-down
      (get-via-key 0)
      j/node)
  # =>
  [:string @{:bc 19 :bl 1 :bp 18 :ec 25 :el 1 :ep 24} `"zero"`]

  (-> (j/par "{}")
      j/zip-down
      (get-via-key :a)
      j/node)
  # =>
  nil

  (-> (j/par "{:a 1 :b 2}")
      j/zip-down
      (get-via-key :c)
      j/node)
  # =>
  nil

  (-> (j/par (string "{# hi there\n"
                     " :ant 1\n"
                     " :bee 2}"))
      j/zip-down
      (get-via-key :bee)
      j/node)
  # =>
  [:number @{:bc 7 :bl 3 :bp 26 :ec 8 :el 3 :ep 27} "2"]

  )

# XXX: not sure if should check whether node for zloc is:
#
#      :array, :bracket-array, :tuple, :bracket-tuple
(defn get-via-index
  ``
  Move to the location corresponding to `index` in the Janet indexed
  data structure associated with `zloc`.  `index` should be a
  Janet natural number (e.g. `0` or `11`).

  If no such index exists, return nil.  Otherwise return the
  determined location.
  ``
  [zloc index]
  (assertf (nat? index) "expected natural number, found: %n" index)
  #
  (def first-index-zloc (j/down-skip-wsc zloc))
  (when first-index-zloc
    (var cur-zloc first-index-zloc)
    (repeat index
      (set cur-zloc (j/right-skip-wsc cur-zloc)))
    #
    cur-zloc))

(comment

  (-> (j/par "[:x :y]")
      j/zip-down
      (get-via-index 1)
      j/node)
  # =>
  [:keyword @{:bc 5 :bl 1 :bp 4 :ec 7 :el 1 :ep 6} ":y"]

  (-> (j/par "[]")
      j/zip-down
      (get-via-index 1)
      j/node)
  # =>
  nil

  (-> (j/par "[:ant]")
      j/zip-down
      (get-via-index 1)
      j/node)
  # =>
  nil

  (-> (j/par (string "[# hi there\n"
                     " :ant :bee :cat]"))
      j/zip-down
      (get-via-index 2)
      j/node)
  # =>
  [:keyword @{:bc 12 :bl 2 :bp 23 :ec 16 :el 2 :ep 27} ":cat"]

  )

(defn get-via
  ``
  Dispatch to `get-via-key` if `zloc` represents a dictionary, or to
  `get-via-index` if it represents an indexed data structure.

  Otherwise, error.
  ``
  [zloc id]
  (def the-type (get (j/node zloc) 0))
  (case the-type
    :struct (get-via-key zloc id)
    :table (get-via-key zloc id)
    :array (get-via-index zloc id)
    :bracket-array (get-via-index zloc id)
    :tuple (get-via-index zloc id)
    :bracket-tuple (get-via-index zloc id)
    (errorf "unexpected type: %n" the-type)))

(comment

  (-> (j/par (string "{# hi there\n"
                     " :ant 1\n"
                     " :bee 2}"))
      j/zip-down
      (get-via :bee)
      j/node)
  # =>
  [:number @{:bc 7 :bl 3 :bp 26 :ec 8 :el 3 :ep 27} "2"]

  (-> (j/par `{-1 "minus-one" 0 "zero"}`)
      j/zip-down
      (get-via 0)
      j/node)
  # =>
  [:string @{:bc 19 :bl 1 :bp 18 :ec 25 :el 1 :ep 24} `"zero"`]

  (-> (j/par "{}")
      j/zip-down
      (get-via :a)
      j/node)
  # =>
  nil

  (-> (j/par (string "[# hi there\n"
                     " :ant :bee :cat]"))
      j/zip-down
      (get-via 2)
      j/node)
  # =>
  [:keyword @{:bc 12 :bl 2 :bp 23 :ec 16 :el 2 :ep 27} ":cat"]

  )

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def file-path (get opts :file-path))
  (def [ok? src] (protect (slurp file-path)))
  (when (not ok?)
    (errorf "failed to read in: %s" file-path))
  #
  (def data (parse-all src))
  (when (<= 2 (length data))
    (errorf "only supports one top-level piece of data atm"))
  #
  (def path (get opts :path))
  (def [value new-path] (get-via-path (get data 0) path))
  #
  (def zloc (-> src j/par j/zip-down))
  (var cur-zloc zloc)
  (each step new-path
    (set cur-zloc (get-via cur-zloc step)))
  (when (nil? cur-zloc)
    (errorf "unexpected nil value for current location"))
  #
  (def found-value-str (j/gen (j/node cur-zloc)))
  (assertf (= value (parse found-value-str))
           "expected: %n, but found: %n" value (parse found-value-str))
  #
  (def new-value-str (get opts :value-str))
  (def v-zloc (-> new-value-str j/par j/zip-down))
  (def e-zloc (j/replace cur-zloc (j/node v-zloc)))
  (def new-src (j/gen (j/root e-zloc)))
  #
  (print new-src))

