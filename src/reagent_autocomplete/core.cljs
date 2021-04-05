;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @ Copyright (c) Michael Leachim                                                      @
;; @ You can find additional information regarding licensing of this work in LICENSE.md @
;; @ You must not remove this notice, or any other, from this software.                 @
;; @ All rights reserved.
;; @version '0.2.0'
;; @@@@@@ At 2018-14-10 14:25 <mklimoff222@gmail.com> @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

(ns reagent-autocomplete.core
  (:require
    [clojure.string :as cljs-string]
    [reagent.core :as reagent]))

(def search-icon
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAABSUlEQVR4AezTM3hYURjG8X9t27btdspW27a2ut1r21iqYImtKVOMJbZtfs+Jc5+LOb+z3e897zWWdeqPDee4zj4WYNlC3Kiitmmlc4+emNSTx1RT225FsQITehGkNlYRwi8e40F205G9GPZTbfJhPChdOE+RHC1hPoYsVzWP6EJrM0iUiSuGOEnYgY4sVY9/ObqGSLSSMXTstRQ9R9dOCQaixUbmoei6IsG3aBkp82x03ZTgQ7T0kXkFuk5K8A9aZss8EV2r1O+g5bzM7dFHikT305FuhMr0CPq4LdEcptDeC/Woe6GP/qRJPI1NbR7zW/XNX8GgjerPr+ETO5lGf9ZzlThVE4MJO8mntqOlrtaFnhg0Dtt2JUU8oTu1ZqtgHQ8IJJViQvnKFvWIXdpUWdYLb9NVnVV1qwXyKDUK5iLKjWqBGkN/MAoAi3W/YOTXNloAAAAASUVORK5CYII=")

(def react-keys
  {:enter      "Enter"
   :arrow-down "ArrowDown"
   :arrow-up    "ArrowUp"
   :arrow-right "ArrowRight"
   :tab "Tab"})

(def selected-bg-color
  {:selected-bg-color "#dcdbfa"
   :!selected-bg-color "white"})

(def icon
  {:search-icon search-icon})


(def default-params
  {:app-state (reagent/atom {:cur-index 0 :cur-input ""})

   :can-enter-new? true
   :display-size 10
   :placeholder "Input anything here..."
   :autofocus? true

   :dropdown-classes nil
   :input-classes nil
   :background-input-classes nil
   :click-submit-classes nil
   :parent-div-classes nil
   :id nil
   :style? true

   :dropdown-style
   {:position "absolute" :right "0" :left "0"   :top "4em" :box-shadow "grey 1px 2px 1px 0px" :background "white" :overflow "hidden"  :border "none" :z-index "999"}
   :input-style
   {:width "100%" :height "2em" :font-size "1.5em" :padding-left "10px" :background "transparent" :border "none"}
   :background-input-style
   {:width "100%" :height "2em" :font-size "1.5em" :padding-left "10px" :color "gray" :position "absolute" :top "0" :right "0" :left "0" :bottom "0" :z-index "-1" :background "white"
    :border "none"}
   :click-submit-style
   {:position "absolute" :right "0" :top "0.3em" :padding "0.5em"}
   :parent-div-style
   {:position "relative" :box-shadow "1px 1px 1px gray"}

   :general-style
   (fn [sl-bg-color] (str ".reagent-autocomplete-input:focus{outline:none;}.reagent-autocomplete-item:hover{background: " sl-bg-color "!important;}"))

   :render-click-submit-fn
   (fn [can-submit? on-click]
     (if can-submit?
       [:div
        {:on-click on-click
         :style {:color (:background-color-selected selected-bg-color) :cursor "pointer"}}
        [:img {:src (:search-icon icon)}]]))
   :compare-fn
   (fn [cur-input item]
     "Checking whether input matches with the item"
     (cljs-string/starts-with?   item  cur-input))
   :validate-fn
   (fn [data] true)
   :submit-fn
   (fn [data]
     "When user clicks enter:"
     (.log js/console "Submitted now: " data))
   :render-fn
   (fn [cur-input selected? text sl-bg-color !sl-bg-color]
     [:div.reagent-autocomplete-item {:style {:padding "0.5em" :background (if  selected? sl-bg-color !sl-bg-color) :font-size "1em" :cursor "pointer"}}
      [:span cur-input]
      [:b (apply str (drop (count cur-input) text))]])})


(defn- e->content
  [e]
  (str
    (aget e "target" "value")))

(defn- change-cur-input
  [app-state input]
  (swap! app-state
    (fn [db]
      (-> db
          (assoc :cur-input input)
          (assoc :cur-index 0)))))

(defn- submit-policy
  [event-input complete-placeholder can-enter-new? has-auto-complete? wrap-submit-fn]
  (cond
    has-auto-complete?
    (wrap-submit-fn complete-placeholder)
    can-enter-new?
    (wrap-submit-fn event-input)
    :else false))

(defn autocomplete-widget
  [completions params]
  (let [{:keys [app-state dropdown-classes input-classes background-input-classes
                click-submit-classes parent-div-classes id style?
                can-enter-new? display-size placeholder autofocus?
                dropdown-style input-style background-input-style parent-div-style
                click-submit-style general-style
                selected-bg-color !selected-bg-color
                compare-fn validate-fn submit-fn render-fn  render-click-submit-fn]}
        (merge icon selected-bg-color default-params params)
        wrap-submit-fn
        (fn [input]
          (if-not (empty? input)
            (do
              (submit-fn input)
              (swap! app-state assoc :cur-input ""))
            identity))]
    (cond
      style? (fn [completions params]
               (let [aps @app-state
                     cur-input (:cur-input aps)
                     cur-index (:cur-index aps)
                     filtered-items
                     (if-not (empty? cur-input)
                       (for [[index item] (map list (range) (take display-size (filter (partial compare-fn cur-input)  completions)))]
                         {:selected? (= index cur-index)
                          :name   item})
                       [])
                     complete-placeholder (or (:name (first (filter :selected? filtered-items))) "")
                     has-auto-complete? (seq complete-placeholder)]
                 (.log js/console "style")
                 [:div {:style parent-div-style}
                  [:style (general-style selected-bg-color)]
                  [:div {:style click-submit-style}
                   [render-click-submit-fn
                    (submit-policy cur-input complete-placeholder can-enter-new? has-auto-complete?
                                   #(not (empty? %)))
                    #(submit-policy cur-input complete-placeholder can-enter-new? has-auto-complete? wrap-submit-fn)]]
                  [:input.reagent-autocomplete-input
                   {:value cur-input
                    :placeholder placeholder
                    :auto-focus autofocus?
                    :style input-style
                    :type "text"
                    :on-change
                    #(if (validate-fn (e->content %))
                       (change-cur-input app-state (e->content %))
                       identity)
                    :on-key-down
                    #(condp = (aget % "key")
                       (:enter react-keys)
                       (do (submit-policy (e->content %) complete-placeholder can-enter-new? has-auto-complete? wrap-submit-fn) true)
                       (:arrow-down react-keys)
                       (swap! app-state assoc :cur-index (mod (inc cur-index) (count filtered-items)))
                       (:arrow-up react-keys)
                       (swap! app-state assoc :cur-index (mod (dec cur-index) (count filtered-items)))
                       (:arrow-right react-keys)
                       (change-cur-input app-state complete-placeholder)
                       (:tab react-keys)
                       (do
                         (cond
                           (= 1 (count filtered-items))
                           (change-cur-input app-state complete-placeholder)
                           :else
                           (swap! app-state assoc :cur-index (mod (inc cur-index) (count filtered-items))))
                         (.call (aget % "preventDefault") %))
                       identity)}]
                  [:input.reagent-autocomplete-input {:style  background-input-style :disabled true
                                                      :placeholder complete-placeholder}]
                  (if (not (empty? filtered-items))
                    [:div {:style dropdown-style}
                     (for [item filtered-items]
                       ^{:key (:name item)}
                       [:div {:on-click #(wrap-submit-fn (:name item))}
                        [render-fn cur-input (:selected? item) (:name item) selected-bg-color !selected-bg-color]])]
                    [:div])]))
      :else (fn [completions params]
              (let [aps @app-state
                    cur-input (:cur-input aps)
                    cur-index (:cur-index aps)
                    filtered-items
                    (if-not (empty? cur-input)
                      (for [[index item] (map list (range) (take display-size (filter (partial compare-fn cur-input)  completions)))]
                        {:selected? (= index cur-index)
                         :name   item})
                      [])
                    complete-placeholder (or (:name (first (filter :selected? filtered-items))) "")
                    has-auto-complete? (not (empty? complete-placeholder))]
                [:div {:class parent-div-classes
                       :id id}
                 [:style (general-style selected-bg-color)]
                 [:div {:class click-submit-classes}
                  [render-click-submit-fn
                   (submit-policy cur-input complete-placeholder can-enter-new? has-auto-complete?
                                  #(not (empty? %)))
                   #(submit-policy cur-input complete-placeholder can-enter-new? has-auto-complete? wrap-submit-fn)]]
                 [:input.reagent-autocomplete-input
                  {:value cur-input
                   :placeholder placeholder
                   :auto-focus autofocus?
                   :class input-classes
                   :type "text"
                   :on-change
                   #(if (validate-fn (e->content %))
                      (change-cur-input app-state (e->content %))
                      identity)
                   :on-key-down
                   #(condp = (aget % "key")
                      (:enter react-keys)
                      (do (submit-policy (e->content %) complete-placeholder can-enter-new? has-auto-complete? wrap-submit-fn) true)
                      (:arrow-down react-keys)
                      (swap! app-state assoc :cur-index (mod (inc cur-index) (count filtered-items)))
                      (:arrow-up react-keys)
                      (swap! app-state assoc :cur-index (mod (dec cur-index) (count filtered-items)))
                      (:arrow-right react-keys)
                      (change-cur-input app-state complete-placeholder)
                      (:tab react-keys)
                      (do
                        (cond
                          (= 1 (count filtered-items))
                          (change-cur-input app-state complete-placeholder)
                          :else
                          (swap! app-state assoc :cur-index (mod (inc cur-index) (count filtered-items))))
                        (.call (aget % "preventDefault") %))
                      identity)}]
                 [:input.reagent-autocomplete-input {:class background-input-classes :disabled true
                                                     :style {:z-index "-1"}
                                                     :placeholder complete-placeholder}]
                 (if (seq filtered-items)
                   [:div {:class dropdown-classes}
                    (for [item filtered-items]
                      ^{:key (:name item)}
                      [:div {:on-click #(wrap-submit-fn (:name item))}
                       [render-fn cur-input (:selected? item) (:name item) selected-bg-color !selected-bg-color]])]
                   [:div])])))))
