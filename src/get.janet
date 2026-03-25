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
        (def new-context (get context step))
        (assertf new-context "failed to take a step: %n in context: %n"
                 step context)
        (array/push new-path step)
        (set context new-context))
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

