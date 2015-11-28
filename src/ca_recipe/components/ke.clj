(ns ca-recepie.components.ke)

;; read
(defn get-rules [ke])
(defn find-rules [ke criterial])

;;update
(defn add-rule [ke rule])
(defn replace-rule [ke old-rule new-role])
(defn delete-rule [ke rule])

;;processing
(defn fire-rules [ke request])
