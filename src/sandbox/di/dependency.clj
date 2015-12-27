(ns sandbox.di.dependency)

(defn get-movie [movie-id]
  {:id "42" :title "A movie"})

(defn get-favorite-videos [user-id]
  [{:id 1}])

(defn get-favorite-decorated-videos [user-id get-movie get-favorite-videos]
  (for [video (get-favorite-videos user-id)]
    {:movie (get-movie (:id video))
     :video video}))

;; add test stubs
(defn get-test-movie [movie-id]
  {:id "43" :title "A test movie"})

(defn get-test-favorite-videos [user-id]
  [{:id 2}])

(defn get-favorite-decorated-videos-2 [user-id]
  (for [video (get-favorite-videos user-id)]
    {:movie (get-movie (:id video))
     :video video}))

(with-redefs
  [get-favorite-videos get-test-favorite-videos
   get-movie get-test-movie]
  (doall (get-favorite-decorated-videos-2 2)))
