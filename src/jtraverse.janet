(import ./jget :as jg)
(import ./jipper :as j)

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
    (set cur-zloc (jg/get-via cur-zloc step)))
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

