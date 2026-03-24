(import ../src/jipper :as j)
(import ../src/main :as m)

(comment

  # depth is insufficient as a way to ensure that a "found" value is
  # the one corresponding to a particular "path"

  # below, there are three 3s all at the same depth
  (def example
    {:a [1 2 {:x 3
              :z 3}]
     :b [7 8 {:y 3}]})

  # path to first 3
  (get-in example [:a 2 :x])
  # =>
  3

  # path to second 3
  (get-in example [:a 2 :z])
  # =>
  3

  # path to third 3
  (get-in example [:b 2 :y])
  # =>
  3

  )

(comment

  (def path-str ":vendored 2 :tag")

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

  (def zloc
    (j/zip-down (j/par src)))

  (def m-zloc
    (j/search-from zloc 
                   |(match (j/node $)
                      [:string _ `"7caf81f636bb97104aada6544219733b3c86badf"`]
                      true)))

  (j/node m-zloc)
  # =>
  [:string @{:bc 9 :bl 15 :bp 484 :ec 51 :el 15 :ep 526} 
   `"7caf81f636bb97104aada6544219733b3c86badf"`]

  # check at appropriate depth
  (= (inc (length path))
     (length (j/path m-zloc)))
  # =>
  true

  (def e-zloc
    (j/replace m-zloc 
               [:string @{} `"9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"`]))

  (def new-src (j/gen (j/root e-zloc)))
  # =>
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
     :tag "9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"
     :paths [["niche.janet" "bin/"]]}]}
  ``

  (def new-ds (parse new-src))

  (m/get-via-path new-ds [:vendored 2 :tag])
  # =>
  ["9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b" @[:vendored 2 :tag]]

  (m/get-via-path new-ds [:vendored
                          |(= (get $ :name) "niche")
                          :tag])
  # =>
  ["9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b" @[:vendored 2 :tag]]

  )

(comment

  (def path-str ":vendored 2 :tag")

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

  # phase 1: ensure path makes sense for ds and obtain new path

  (def [value new-path] (m/get-via-path ds path))
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  # phase 2: zipper traversal with edit

  (def zloc (j/zip-down (j/par src)))

  # first item in path to look for is :vendored
  (var cur-zloc (j/down-skip-wsc zloc))

  (var cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :keyword

  (get cur-node 2)
  # =>
  ":name"

  (set cur-zloc (j/right-until cur-zloc
                               |(match (j/node $)
                                  [:keyword _ ":vendored"]
                                  true
                                  #
                                  false)))

  (set cur-node (j/node cur-zloc))
  # =>
  [:keyword @{:bc 2 :bl 4 :bp 101 :ec 11 :el 4 :ep 110} ":vendored"]

  # found the keyword move to corresponding value
  (set cur-zloc (j/right-skip-wsc cur-zloc))

  # tuple
  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :bracket-tuple

  # move down into value
  (set cur-zloc (j/down-skip-wsc cur-zloc))

  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :struct

  # second path item, 2, so need to do right-skip-wsc twice
  (set cur-zloc (j/right-skip-wsc cur-zloc))
  (set cur-zloc (j/right-skip-wsc cur-zloc))

  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :struct

  # move down into struct
  (set cur-zloc (j/down-skip-wsc cur-zloc))

  (set cur-node (j/node cur-zloc))
  # =>
  [:keyword @{:bc 4 :bl 13 :bp 420 :ec 9 :el 13 :ep 425} ":name"]

  # last path item, :tag
  (set cur-zloc (j/right-until cur-zloc
                               |(match (j/node $)
                                  [:keyword _ ":tag"]
                                  true
                                  #
                                  false)))

  (set cur-node (j/node cur-zloc))
  # =>
  [:keyword @{:bc 4 :bl 15 :bp 479 :ec 8 :el 15 :ep 483} ":tag"]

  # found the keyword move to corresponding value
  (set cur-zloc (j/right-skip-wsc cur-zloc))

  # target node
  (set cur-node (j/node cur-zloc))
  # =>
  [:string @{:bc 9 :bl 15 :bp 484 :ec 51 :el 15 :ep 526} 
   `"7caf81f636bb97104aada6544219733b3c86badf"`]

  # can verify value against returned info from `m/get-via-path`
  value
  # =>
  "7caf81f636bb97104aada6544219733b3c86badf"

  (= value
     (parse (get cur-node 2)))
  # =>
  true

  (def e-zloc
    (j/replace cur-zloc
               [:string @{} `"9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"`]))

  (j/gen (j/root e-zloc))
  # =>
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
     :tag "9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"
     :paths [["niche.janet" "bin/"]]}]}
  ``

  )

(comment

  (def path-str ":vendored 2 :tag")

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

  # phase 1: ensure path makes sense for ds and obtain new path

  (def [value new-path] (m/get-via-path ds path))
  # =>
  ["7caf81f636bb97104aada6544219733b3c86badf" @[:vendored 2 :tag]]

  # phase 2: zipper traversal with edit

  (def zloc (j/zip-down (j/par src)))

  (var cur-zloc zloc)
  (var cur-node (j/node cur-zloc))
  (var step nil)

  # first step in path
  (set step (get path 0))
  # =>
  :vendored

  (set cur-zloc (m/get-via cur-zloc step))
  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :bracket-tuple

  # second step in path
  (set step (get path 1))
  # =>
  2

  (set cur-zloc (m/get-via cur-zloc step))
  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :struct

  # third step in path
  (set step (get path 2))
  # =>
  :tag

  (set cur-zloc (m/get-via cur-zloc step))
  (set cur-node (j/node cur-zloc))

  (get cur-node 0)
  # =>
  :string

  cur-node
  # =>
  [:string @{:bc 9 :bl 15 :bp 484 :ec 51 :el 15 :ep 526}
   `"7caf81f636bb97104aada6544219733b3c86badf"`]

  # can verify value against returned info from `m/get-via-path`
  value
  # =>
  "7caf81f636bb97104aada6544219733b3c86badf"

  (= value
     (parse (get cur-node 2)))
  # =>
  true

  (def e-zloc
    (j/replace cur-zloc
               [:string @{} `"9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"`]))

  (j/gen (j/root e-zloc))
  # =>
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
     :tag "9c2832d803b39f27ffd12d1a5fc82f3b83c64b4b"
     :paths [["niche.janet" "bin/"]]}]}
  ``

  )

