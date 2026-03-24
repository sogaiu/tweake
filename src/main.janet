(import ./args :as a)
(import ./get :as g)
(import ./jipper :as j)

(def version "DEVEL")

(def usage
  `````
  Usage: tweake <file> <path> <value>
         tweake - <path> <value>

         tweake [-h|--help]|[-v|--version]

  Modify and display `.jdn` content [1].

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

(defn tweak
  [src path new-value-str]
  (def data (parse-all src))
  (when (<= 2 (length data))
    (errorf "only supports one top-level piece of data atm"))
  #
  (def [value new-path] (g/get-via-path (get data 0) path))
  #
  (def zloc (-> src j/par j/zip-down))
  (var cur-zloc zloc)
  (each step new-path
    (set cur-zloc (g/get-via cur-zloc step)))
  (when (nil? cur-zloc)
    (errorf "unexpected nil value for current location"))
  #
  (def found-value-str (j/gen (j/node cur-zloc)))
  (assertf (= value (parse found-value-str))
           "expected: %n, but found: %n" value (parse found-value-str))
  #
  (def v-zloc (-> new-value-str j/par j/zip-down))
  (def e-zloc (j/replace cur-zloc (j/node v-zloc)))
  #
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

