(defn parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break {:show-help true}))
  #
  (when (or (not head) (= head "-v") (= head "--version"))
    (break {:show-version true}))
  #
  (array/remove the-args 0)
  #
  (def [opts file-path]
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      [@{} head]
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (def opts parsed)
        (def new-head (get the-args 0))
        (array/remove the-args 0)
        (assertf new-head "expected a command but found none: %n" args)
        [opts new-head])))
  #
  (def input
    (if (= "-" file-path)
      stdin
      (do
        (assertf (= :file (os/stat file-path :mode))
                 "not a file path: %s" file-path)
        #
        (os/realpath file-path))))
  # XXX: improve feedback message?
  (assertf (<= 2 (length the-args))
           "need at least two more arguments: %n" the-args)
  #
  (def [path-str value-str] the-args)
  (assertf (parse-all path-str) "could not parse: %n" path-str)
  #
  (def path (parse-all path-str))
  # XXX: using `tuple?` below, but the expected input is a tuple
  #      that represents a short-fn.  things of the form |(...)
  #      can be used, e.g.
  #
  #        |(= (get $ :name) "niche")
  #
  #      because the parser "expands" this to:
  #
  #        (short-fn (= (get $ :name) "niche"))
  #
  (let [first-step (first path)]
    (if (symbol? first-step)
      (assertf (string/has-prefix? "@" first-step)
               "leading symbol item must start with @: %n" first-step)
      (assertf (or (keyword? first-step)
                   (nat? first-step)
                   (tuple? first-step))
               "unexpected type for first step in path: %n" first-step)))
  #
  (assertf (all |(or (keyword? $) (nat? $) (tuple? $))
                (slice path 1))
           "only keywords, natural numbers, tuples allowed: %n"
           (slice path 1))
  #
  (when (not= "nil" value-str)
    (assertf (parse value-str) "could not parse: %n" value-str))
  #
  (array/remove the-args 0)
  (array/remove the-args 0)
  #
  (def top-level-index
    (let [first-step (get path 0)]
      (if (and (symbol? first-step)
               (string/has-prefix? "@" (slice first-step 0 1)))
        (let [scanned (scan-number (slice first-step 1))]
          (if (number? scanned)
            scanned
            # XXX: haven't settled on whether a keyword is better here
            nil))
        0)))
  #
  (merge opts
         {:input input
          :top-level-index top-level-index
          # XXX: is this `eval` use likely to be a problem?
          :path (eval (tuple/brackets ;(slice path 1)))
          :value-str value-str
          :rest the-args}))

