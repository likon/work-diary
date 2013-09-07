(ns work-diary.gui
  (:gen-class)
  (:use [seesaw core swingx]
        [work-diary core store util])
  (:require [seesaw.bind :as b])
  (:import (java.util Date)))

(defn make-component [title desc content]
  (border-panel
    :id :component
    :hgap 5 :vgap 5 :border 5
    :north (header :title title :description (str "<html>" desc "</html>"))
    :center content))

(defn make-editor []
  (make-component 
    "Diary Editor" "" 
    (border-panel
      :north (border-panel
               :north
               (horizontal-panel 
                 :items [(label :text "Time Segment: ")
                         (combobox :id :time-segment :editable? true :model [])
                         (label :text " ")
                         (button :id :start-work :text "Start Work")
                         (label :text "  ")
                         (button :id :stop-work :text "View")])
               :center
               (horizontal-panel
                 :items [(label :text "Time Segment: ")
                         (label :id :begin-time :text "H:M")
                         (label :text " - ")
                         (label :id :end-time :text "H:M")]))
      :center (border-panel
                :north
                (scrollable 
                  (editor-pane
                    :id :text-area
                    :size [400 :by 300]))
                :center
                (scrollable
                  (editor-pane
                    :id :debug-area
                    :size [400 :by 100]))))))

(defn make-viewer []
  (make-component "Diary Viewer" ""
                  (border-panel
                    :north (button :text "Diary View"))))

(defn make-config []
  (make-component "Config View" ""
                  (border-panel
                    :north 
                    (horizontal-panel
                      :items [(label :text "Diaries Directory: ")
                              (text :id :diary-directory :text "")]))))

(def *begin-time* (ref nil))
(def *editor* (make-editor))
(def *viewer* (make-viewer))
(def *config-view* (make-config))
(def *list-model* {"Editor" *editor*})

(defn print-view-message [head content]
  (let [debug-view (select *editor* [:#debug-area])]
    (text! debug-view (str (text debug-view) head (pretty-date (Date.)) ": " content "\n"))))

(def info (partial print-view-message "[info]"))
(def debug (partial print-view-message "[debug]"))
(def warn (partial print-view-message "[warn]"))
(def error (partial print-view-message "[error]"))

(defn make-ui []
  (frame 
    :title "Diary Management Program!"
    :size [900 :by 800]
    :content
    (border-panel 
      :hgap 5 :vgap 5 :border 5
      :north (label-x :wrap-lines? true
                      :text (str "This software can ease your work on diary "
                                 "because you shouldn't care about the time, just the content."))
      :center (left-right-split
                (listbox-x
                  :id :chooser
                  :model (keys *list-model*))
                (border-panel :id :container
                              :center *editor*)
                :divider-location 1/4))))

(def *root* (make-ui))

(defn get-file-name [ext]
  (str "diaries/" (pretty-day (Date.)) ext))
(def temp-file (get-file-name ".temp"))
(def html-file (get-file-name ".html"))
(def doc-file (get-file-name ".doc"))

(defn on-start-work-cb [e]
  (let [begin-time (select *editor* [:#begin-time])
        start-button (select *editor* [:#start-work])]
    (if @*begin-time*
      (let [editor-area (select *editor* [:#text-area])
            content (text editor-area)
            end-time (select *editor* [:#end-time])
            combobox (select *editor* [:#time-segment])
            time-segment (selection combobox)]
        (if-not (empty? content)
          (do
            (let [diary (find-diary time-segment)]
              (if-not (empty? diary)
                (let [diary (nth diary 0)]
                  (write-diary (:begin-date @diary) (:end-date @diary) content))
                (if-not (empty? time-segment)
                  (let [dates (time-segment->dates time-segment)]
                    (if dates
                      (write-diary (first dates) (nth dates 1) content)))
                  (write-diary @*begin-time* content))))
            (info "Work done!")
            (diaries-temp-save temp-file (str (get-converted-diaries)))
            ;(info (str (get-converted-diaries)))
            (config! start-button :text "Start Work")
            (config! editor-area :text "")
            (config! end-time :text (date->hour-minute (Date.)))

            (doto combobox (.removeAllItems))
            (doseq [item (get-all-time-segments @current-diary)] (.addItem combobox item))
            (selection! combobox "")
            (dosync (ref-set *begin-time* nil)))
          (do
            (error "Please type some character in the editor area!!")
            (dosync (ref-set *begin-time* nil))
            (config! begin-time :text "")
            (config! end-time :text "")
            (config! start-button :text "Start Work"))))
      (dosync (ref-set *begin-time* (Date.))
        (info (str "Work start at " (pretty-date @*begin-time*)))
        (config! begin-time :text (date->hour-minute @*begin-time*))
        (config! start-button :text "Submit")))))

(defn on-stop-work-cb [e]
  (try
    (do
      (diaries-save doc-file (get-converted-diaries) (/ (get-work-time) 60000.0) (get-work-time-segments))
      (info "diaries saved successfully.")
      (.exec (Runtime/getRuntime) (str "cmd /c start " doc-file)))
    (catch Exception e
      (error "diaries save failed!")
      (.printStackTrace e))))

(defn on-time-segment-cb [e]
  (let [combobox (select *editor* [:#time-segment])
        diaries (find-diary (selection combobox))
        editor (select *editor* [:#text-area])]
    (if-not (empty? diaries)
      (text! editor (apply str (map #(:content @%) diaries)))
      (text! editor ""))))

(defn on-chooser-cb [e]
  (let [chooser (select *root* [:#chooser])
        container (select *root* [:#container])
        component (select *root* [:#component])]
    (replace! container component (*list-model* (selection chooser)))))

(defn on-exit-cb [e]
  (dispose! *root*))

(defn load-temp-diaries []
  (try 
    (let [diaries (diaries-temp-load temp-file)
          combobox (select *editor* [:#time-segment])]
      (doseq [diary diaries]
        (do
          (let [[begin-time end-time] (time-segment->dates (:time-segment diary))]
            (write-diary begin-time end-time (:content diary)))))
      (doto combobox (.removeAllItems))
      (doseq [item (get-all-time-segments @current-diary)] (.addItem combobox item))
      (selection! combobox "")
      (info "diaries loaded."))
    (catch Exception e
      (info "no saved diaries.")
      (.printStackTrace e))))

(defn add-behaviors [root]
  (let [start-button (select *editor* [:#start-work])
        stop-button (select *editor* [:#stop-work])
        chooser (select root [:#chooser])
        time-segment (select *editor* [:#time-segment])]
    (listen start-button :action on-start-work-cb)
    (listen stop-button :action on-stop-work-cb)
    (listen chooser :selection on-chooser-cb)
    (listen time-segment :selection on-time-segment-cb)
    (config! root :on-close :exit)
    (load-temp-diaries))
  root)


;(defn -main
;[& args]
;(native!)
;(-> *root* add-behaviors pack! show!))
