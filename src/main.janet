(import ./args :as a)
(import ./jtraverse :as jt)
(import ./traverse :as t)

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

    Show content based on `bundle/info.jdn` with new :name:

    $ tweake bundle/info.jdn ':name' '"cooler-name"'

    Show content based on standard input with :name's value
    changed:

    $ cat bundle/info.jdn | tweake - ':name' '"spot"'

    Show content based on `.niche.jdn` which changes included
    paths:

    $ tweake .niche.jdn ':includes 0' '"tweake"'

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
  [src top-level-index path value-str]
  (def [found-value new-path skip]
    (t/scan-src src top-level-index path))
  #
  (jt/traverse-src src skip new-path found-value value-str))

(comment

  (tweak `{:key "value"}` 0 [:key] `"lock"`)
  # =>
  `{:key "lock"}`

  (def src
    ``
    [{:name "alice"
      :value 1}
     {:name "bob"
      :value 2}]
    ``)

  (tweak src 0 [0 :value] `11`)
  # =>
  ``
  [{:name "alice"
    :value 11}
   {:name "bob"
    :value 2}]
  ``

  (tweak src 0 [|(= (get $ :name) "bob") :value] `11`)
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

  (tweak project-janet-src 1 [2] `"janet-pegs"`)
  # =>
  ``
  (declare-project
    :name "janet-peg"
    :url "https://github.com/sogaiu/janet-peg")

  (declare-source
    :prefix "janet-pegs"
    :source @["lib"])
  ``

  (tweak project-janet-src 1 [] `nil`)
  # =>
  ``
  (declare-project
    :name "janet-peg"
    :url "https://github.com/sogaiu/janet-peg")

  nil
  ``

  (tweak project-janet-src
         nil
         [|(= (get $ 0) 'declare-source)]
         `nil`)
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
  (def {:input input
        :path path
        :top-level-index top-level-index
        :value-str value-str} opts)
  #
  (def [ok? src] (protect (if (= input stdin)
                            (file/read input :all)
                            (slurp input))))
  (when (not ok?)
    (errorf "failed to read in: %s" input))
  #
  (def new-src (tweak src top-level-index path value-str))
  #
  (print new-src))

