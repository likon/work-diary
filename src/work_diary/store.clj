(ns work-diary.store
  (:gen-class)
  (:use [clojure.java io]
        [work-diary util])
  (:require [clojure.contrib.prxml :as prxml]
            [clojure.string :as string])
  (:import [java.util Date]))

(def week-day-cn ["日" "一" "二" "三" "四" "五" "六"])

(defn diaries->xml 
  [diary work-time work-time-segments]
  (binding [prxml/*prxml-indent* 2
            prxml/*html-compatible* true]
    (with-out-str
      (prxml/prxml 
        [:table {:cellspacing 0 :width 650} [:style {:type "text/css"}
                                             "table,tr,td{border-style: solid; border-width: 1px}"]
         [:th {:colspan 4} (str (pretty-day (Date.)) " 星期" (week-day-cn (.getDay (Date.))) " 的工作日志")]
         [:tr [:td {:width 50} "序号"] [:td {:width 70} "时间"] [:td "工作事项及内容"] [:td {:width 50} "工作耗时(分钟)"]]
         (let [gen-row (fn [acc diary time-segments index]
                         (if (empty? diary) acc
                           (recur                                 
                             (conj acc 
                                   [:tr 
                                    [:td {:width 50} (str index)] 
                                    [:td {:width 100} (:time-segment (first diary))]
                                    [:td [:br] (map #(list % [:br]) (string/split (:content (first diary)) #"\n"))]
                                    [:td (format "%d" (int (/ (first time-segments) 60000.0)))]])
                             (rest diary) (rest time-segments) (inc index))))]
           (reverse (gen-row () diary work-time-segments 1)))
         [:tr [:td {:colspan 2} "实际工作时间合计（分钟）"] [:td {:colspan 2} (str (format "%d" (int work-time)))]]]
        [:br]))))

(defn to-html
  [content]
  (str
    "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
    "<head>"
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
    "</head>"
    "<body>"
    content
    "</body>"
    "</html>"))

(defn diaries-save
  [filename diaries work-time work-time-segments]
  (with-open [wr (clojure.java.io/writer filename :append false)]
    (.write wr (to-html (diaries->xml diaries work-time work-time-segments)))))

(defn diaries-temp-save
  [filename diaries]
  (with-open [wr (clojure.java.io/writer filename :if-not-exists :create)]
    (.write wr (str diaries))))

(defn diaries-temp-load
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (read-string (apply str (line-seq rdr)))))