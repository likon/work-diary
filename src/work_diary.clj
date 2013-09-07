
(ns work-diary
  (:gen-class)
  (:use [work-diary gui]
        [seesaw core]))

(defn -main
  [& args]
  (native!)
  (-> *root* add-behaviors pack! show!))

