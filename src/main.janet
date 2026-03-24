(import ./args :as a)
(import ./get :as g)
(import ./jipper :as j)

(def version "DEVEL")

(def usage
  `````
  Usage: tweake <file> <path> <value>
         tweake - <path> <value>

         tweake [-h|--help]|[-v|--version]

  Slightly modify [1] and display JDN content [2].

  Parameters:

    <file>                 path to `.jdn` file
    <path>                 describes "path" to target to replace
    <value>                value to replace with
    -                      read JDN content from standard input

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Show content based on standard input with :name's value
    changed:

    $ echo '{:name "hello"}' | \
      tweake - ':name' '"annyeong"'

    Show content based on `.niche.jdn` which excludes a file:

    $ tweake .niche.jdn ':excludes' '["src/args.janet"]'

    Show content based on `.niche.jand` which changes target:

    $ tweake .niche.jdn ':includes 1' '"tweake"'

    Show content based on `bundle/info.jdn` with new name:

    $ tweake bundle/info.jdn ':name' '"cooler-name"'

    Show content based on `bundle/info.jdn` with new tag for a
    vendored dependency:

    $ tweake bundle/info.jdn \
             ':vendored |(= (get $ :name) "niche") :tag' \
             '"shiny-new-tag"'

  ---

  [1] A fair bit of the formatting as well as comments are
      preserved.

  [2] It's only the content which is modified and displayed,
      i.e. files are not modified in-place.

  `````)

########################################################################

(defn tweak
  [src path new-value-str]
  # check source string
  (def [ok? data] (protect (parse-all src)))
  (when (not ok?)
    (errorf "failed to parse content"))
  #
  (var skip 0)
  # adjust data and path depending on number of top-level items
  (def [mod-data mod-path]
    (cond
      (= 1 (length data))
      [(get data 0) path]
      #
      (let [first-step (get path 0)]
        (assertf (and (nat? first-step)
                      (<= 0 first-step (dec (length data))))
                 (string "number of top-level data detected > 1: %d\n"
                         "first element of path not a natural number")
                 (length data))
        (set skip first-step)
        [(get data first-step) (slice path 1)])))
  # non-zipper traversal to learn various things
  (def [value new-path] (g/get-via-path mod-data mod-path))
  # prepare zipper for zipper traversal
  (def zloc (-> src j/par j/zip-down))
  (var cur-zloc zloc)
  # if needed, skip to appropriate top-level item
  (repeat skip
    (set cur-zloc (j/right-skip-wsc cur-zloc)))
  # traverse the path, one step at a time
  (each step new-path
    (set cur-zloc (g/get-via cur-zloc step)))
  (when (nil? cur-zloc)
    (errorf "unexpected nil value for current location"))
  #
  # prepare replacement
  (def found-value-str (j/gen (j/node cur-zloc)))
  (assertf (deep= value (parse found-value-str))
           "expected: %n, but found: %n" value (parse found-value-str))
  #
  (def v-zloc (-> new-value-str j/par j/zip-down))
  # replace
  (def e-zloc (j/replace cur-zloc (j/node v-zloc)))
  # generate new source string
  (j/gen (j/root e-zloc)))

(comment

  (tweak `{:key "value"}` [:key] `"lock"`)
  # =>
  `{:key "lock"}`

  (def src
    ``
    [{:name "alice"
      :value 1}
     {:name "bob"
      :value 2}]
    ``)

  (tweak src [0 :value] `11`)
  # =>
  ``
  [{:name "alice"
    :value 11}
   {:name "bob"
    :value 2}]
  ``

  (tweak src [|(= (get $ :name) "bob") :value] `11`)
  # =>
  ``
  [{:name "alice"
    :value 1}
   {:name "bob"
    :value 11}]
  ``

  (def project-janet-src
    ``
    (declare-project
       :name "janet-peg"
       :url "https://github.com/sogaiu/janet-peg")

    (declare-source
      :prefix "janet-peg"
      :source @["lib"])
    ``)

  (tweak project-janet-src [1 2] `"janet-pegs"`)
  # =>
  ``
  (declare-project
     :name "janet-peg"
     :url "https://github.com/sogaiu/janet-peg")

  (declare-source
    :prefix "janet-pegs"
    :source @["lib"])
  ``

  (tweak project-janet-src [1] `nil`)
  # =>
  ``
  (declare-project
     :name "janet-peg"
     :url "https://github.com/sogaiu/janet-peg")

  nil
  ``

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
  (def input (get opts :input))
  (def path (get opts :path))
  (def new-value-str (get opts :value-str))
  #
  (def [ok? src] (protect (if (= input stdin)
                            (file/read input :all)
                            (slurp input))))
  (when (not ok?)
    (errorf "failed to read in: %s" input))
  #
  (def new-src (tweak src path new-value-str))
  #
  (print new-src))

