(ns sandbox.lazyseq.pages)

(defn get-page [page-num]
  (cond
    (= page-num 1) "Page 1"
    (= page-num 2) "Page 2"
    (= page-num 3) "Page 3"
    :default nil))

(defn paged-sequence [page-num]
  (let [page (get-page page-num)]
    (when page
      (cons page (lazy-seq (paged-sequence (inc page-num)))))))
