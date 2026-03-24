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
  (assertf (all |(or (keyword? $) (nat? $) (tuple? $)) path)
           "detected other than keywords, natural numbers, or tuples: %n" path)
  #
  (assertf (parse value-str) "could not parse: %n" value-str)
  #
  (array/remove the-args 0)
  (array/remove the-args 0)
  #
  (merge opts
         {:input input
          # XXX: is this `eval` use likely to be a problem?
          :path (eval path)
          :value-str value-str
          :rest the-args}))

