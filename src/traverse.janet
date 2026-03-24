(import ./get :as g)
(import ./jipper :as j)

(defn scan-src
  [src top-level-index path]
  # check source string
  (def [ok? data] (protect (parse-all src)))
  (when (not ok?)
    (errorf "failed to parse content"))
  #
  (var tl-idx nil)
  # determine top-level item to operate on
  (def top-level-item
    (if (nat? top-level-index)
      (get data top-level-index)
      (let [first-step (get path 0)]
        (assertf (function? first-step)
                 "first item of path should be a function: %n"
                 first-step)
        (var capture nil)
        (eachp [i elt] data
          (when (first-step elt)
            (set tl-idx i)
            (set capture elt)
            (break)))
        (when (nil? capture)
          (errorf "failed to find target top-level item: %n"
                  data))
        #
        capture)))
  (assertf top-level-item
           "failed to determine top-level item for: %n" data)
  # adjust path if needed
  (def mod-path (if tl-idx (slice path 1) path))
  # finalize top-level index
  (set tl-idx (cond
                (nat? top-level-index) top-level-index
                (nat? tl-idx) tl-idx
                (errorf "failed to determine top-level index")))
  # non-zipper traversal to learn various things
  [;(g/get-via-path top-level-item mod-path) tl-idx])

(defn traverse-src
  [src skip path value value-str]
  # prepare zipper for zipper traversal
  (def zloc (-> src j/par j/zip-down))
  (var cur-zloc zloc)
  # if needed, skip to appropriate top-level item
  (repeat skip
    (set cur-zloc (j/right-skip-wsc cur-zloc)))
  # traverse the path, one step at a time
  (each step path
    (set cur-zloc (g/get-via cur-zloc step)))
  (when (nil? cur-zloc)
    (errorf "unexpected nil value for current location"))
  #
  # prepare replacement
  (def found-value-str (j/gen (j/node cur-zloc)))
  (assertf (deep= value (parse found-value-str))
           "expected: %n, but found: %n" value (parse found-value-str))
  #
  (def v-zloc (-> value-str j/par j/zip-down))
  # replace
  (def e-zloc (j/replace cur-zloc (j/node v-zloc)))
  # generate new source string
  (j/gen (j/root e-zloc)))

