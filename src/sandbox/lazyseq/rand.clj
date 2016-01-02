(ns sandbox.lazyseq.rand)

(def integers (range Integer/MAX_VALUE))

(def randoms (repeatedly (fn [] (rand-int Integer/MAX_VALUE))))
