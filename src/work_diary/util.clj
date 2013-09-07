
(ns work-diary.util
  (:gen-class)
  (:require [clojure.string :as string])
  (:import [java.util Date]))


(defn date->hour-minute [date]
  (format "%02d:%02d" (.getHours date) (.getMinutes date)))

(defn hour-minute->date [string]
  (let [hour-minute (string/split string #":")]
    (if (= (count hour-minute) 2)
      (let [current (Date.)]
        (Date. (.getYear current) (.getMonth current) (.getDate current)
               (Integer/parseInt (first hour-minute))
               (Integer/parseInt (nth hour-minute 1)))))))

(defn time-segment->dates [string]
  (let [hour-minutes (string/split string #"-")]
    (if (= (count hour-minutes) 2)
      [(hour-minute->date (first hour-minutes))
       (hour-minute->date (nth hour-minutes 1))])))

(defn time-segment 
  [begin-date end-date]
  (let [begin (date->hour-minute begin-date)
        end (date->hour-minute end-date)]
    (str begin "-" end)))

(defmacro print-format 
  [fmt & args]
  `(println (format ~fmt ~@args)))

(defn pretty-date
  [date]
  (format "%04d-%02d-%02d %02d:%02d:%02d" 
          (+ (.getYear date) 1900)  
          (inc (.getMonth date))  
          (.getDate date)
          (.getHours date)  
          (.getMinutes date)  
          (.getSeconds date)))

(defn pretty-day
  [date]
  (format "%04d-%02d-%02d" 
          (+ (.getYear date) 1900)
          (inc (.getMonth date))
          (.getDate date)))

