#! /usr/bin/env janet

(use ./sh-dsl)

(defn copy-file
  [src dst]
  (spit dst (slurp src)))

########################################################################

(prin "* running jell...") (flush)
(def jell-exit ($ janet ./bin/jell))
(assertf (zero? jell-exit)
         "jell exited: %d" jell-exit)
(print "done")

(prin "* copying tweake.janet to tweake...")
(copy-file "tweake.janet" "tweake")
(print "done")

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

########################################################################

(print "* updating README...")
(def readme-update-ext ($ janet tweake -h > README))
(assertf (zero? readme-update-ext)
         "updating README exited: %d" readme-update-ext)
(print "done")

########################################################################

(print "* trying some invocations...")

# sourced from tweake -h output
(def tests
  [~[[./tweake bundle/info.jdn ":name" `"cooler-name"`]
     "data/0.jdn"]
   #
   ~[[./tweake .niche.jdn ":includes 0" `"tweake"`]
     "data/1.jdn"]
   #
   ~[[./tweake
      bundle/info.jdn
      `:vendored |(= (get $ :name) "niche") :tag`
      `"shiny-new-tag"`]
     "data/2.jdn"]])

(var n-passed 0)
(var n-failed 0)

(each [cmd path] tests
  (def output ($< ;cmd))
  (def results (parse output))
  (def expected (parse (slurp path)))
  (def success (deep= results expected))
  (var msg nil)
  (if success
    (do
      (++ n-passed)
      (set msg "passed"))
    (do
      (++ n-failed)
      (set msg "failed")))
  (printf "%n: %s" cmd msg))

(printf "passed / total: [%d/%d]"
        n-passed (+ n-passed n-failed))

(print "done")

########################################################################

(print "*** remember to update visito! ***")

