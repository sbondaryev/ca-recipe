(ns stock-ticker-demo.price-feed)

(def stock-map {"YHOO" {:max-price 31.1 :min-price 11.51
                        :wavelength 90 :starting-point 0.5
                        :eps 2.15}
                "APPL" {:max-price 408.38 :min-price 84.11
                        :wavelength 60 :starting-point 0
                        :eps 31.25}
                "GOOG" {:max-price 809.1 :min-price 292.96
                        :wavelength 50 :starting-point 0.75
                        :eps 29.31}
                "AMZN" {:max-price 274.1 :min-price 42.7
                        :wavelength 45 :starting-point 0.25
                        :eps 1.15}})
