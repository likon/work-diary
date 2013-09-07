(ns work-diary.core
  (:gen-class)
  (:use [work-diary util])
  (:import (java.util Date)))

;; [{:begin-date date1 :end-date date2 :content "content"} {... ...}]
;; all data is referenced.
(def current-diary (ref []))

(def +format-str+ "%-20s%-20s%-20s")

(defn diary-equal?
  [segment diary-note]
  (= segment (time-segment (:begin-date diary-note) (:end-date diary-note))))

(defn find-diary
  "return the found segment ref sequence, examples:
  (#<Ref@xxx: {:begin-date xxx :end-date xxx :content xxx}>)"
  [segment]
  (filter #(= segment
              (time-segment (:begin-date @%) (:end-date @%)))
          @current-diary))

(defn write-diary
  ([begin-date content] (write-diary begin-date (Date.) content))
  ([begin-date end-date content]
   (let [found (find-diary (time-segment begin-date end-date))]
     (if (not (empty? found))
       (dosync (alter (first found) assoc
                      :begin-date begin-date
                      :end-date end-date
                      :content content))
       (let [diary (ref {:begin-date begin-date
                         :end-date end-date
                         :content content})]
         (dosync (alter current-diary conj diary)))))))

(defn get-sorted-diaries
  []
  (sort-by #(.getTime (:begin-date @%)) @current-diary))

;; drop the ref and convert date to time segment
(defn get-converted-diaries
  []
  (let [diaries (get-sorted-diaries)]
    (reverse
      (reduce (fn [feed diary]
                (conj feed
                      {:time-segment (time-segment (:begin-date @diary)
                                                   (:end-date @diary))
                       :content (:content @diary)}))
              ()
              diaries))))

(defn get-diary
  "return a map such as {:begin-date date1 :end-date date2 :content content}"
  [segment]
  (let [found (find-diary segment)]
    (if (empty? found) nil
      @(first found))))


(defn print-all-diary
  [diaries]
  (let [iter-loop (fn [v]
                    (if (empty? v) nil
                      (let [diary @(first v)]
                        (print-format +format-str+
                                      (pretty-date (:begin-date diary))
                                      (pretty-date (:end-date diary))
                                      (:content diary))
                        (recur (rest v)))))]
    (print-format +format-str+ "begin-date" "end-date" "content")
    (print-format "------------------------")
    (iter-loop diaries)))

(defn get-all-time-segments
  [diaries]
  (map #(time-segment (:begin-date @%) (:end-date @%)) diaries))

(defn get-work-time
  []
  (reduce (fn [acc diary]
            (+ acc
               (- (.getTime (:end-date @diary))
                  (.getTime (:begin-date @diary)))))
          0
          @current-diary))

(defn get-work-time-segments
  []
  (map (fn [diary]
         (- (.getTime (:end-date @diary))
            (.getTime (:begin-date @diary))))
       (get-sorted-diaries)))

;(defn -main
;[& args]
;(let [begin-date (Date.)
;end-date (Date.)]
;(write-diary begin-date end-date "test")
;(write-diary begin-date end-date "I am a student.")
;(print-all-diary @current-diary)
;(Thread/sleep 2000)
;(let [begin-date (Date. (- 2012 1900) 8 12 6 10)
;end-date (Date. (- 2012 1900) 8 12 6 10)]
;(write-diary begin-date end-date "今天完成串口驱动的调试")
;(print-all-diary @current-diary))

;(println (get-all-time-segments (get-sorted-diaries)))
;(println (get-converted-diaries))))

