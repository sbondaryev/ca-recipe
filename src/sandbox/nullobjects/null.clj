(ns sandbox.nullobjects.null)

(defn build-person [first-name last-name] (if (and first-name last-name)
                                            {:first-name first-name :last-name last-name} {:first-name "John" :last-name "Doe"}))
