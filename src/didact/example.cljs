(ns didact.example
  (:require [didact.reagent :as r]))

(defn- child-comp [{:keys [msg]}]
  [:div
   "Hi, I am 'child2-comp"
   [:div
    "I've got '" msg "'"]])

(defn- nested-click-counter []
  (let [counter_ (r/ratom 0)
        on-click (fn []
                   (reset! counter_
                           (inc @counter_)))]
    (fn []
      [:div
       {:style {:padding 5
                :border "1px black solid"}}
       "This is child counter"
       [:br]
       [:button
        {:onClick on-click}
        "Clicked "
        @counter_
        " time(s)"]])))

(defn- click-counter []
  (let [counter_ (r/ratom 0)
        on-click (fn []
                   (reset! counter_
                           (inc @counter_)))]
    (fn []
      [:div
       {:style {:padding 5
                :border "1px black solid"}}
       "This is parent counter"
       [:br]
       [:button
        {:onClick on-click}
        "Clicked "
        @counter_
        " time(s)"]
       [nested-click-counter]])))

(defn- top-ranker [{:keys [ranker-list]}]
  (let [top-3 (take 3 ranker-list)]
    [:ul
     (for [[rank ranker] (map-indexed vector top-3)
           :when (some? ranker)]
       [:li
        {:key (str rank)}
        "Rank " (inc rank) ". " (str ranker)])]))

(defn- input-test []
  (let [value_ (r/ratom "")
        on-change (fn [^js evt]
                    (reset! value_
                            (-> evt .-target .-value)))]
    (fn []
      [:div
       [:input
        {:value @value_
         :onChange on-change
         :placeholder "Type here!"}]
       [:div
        "You typed:" @value_]])))

(defn app []
  [:div
   [:section
    [:h1
     "Basic component"]
    [:div
     {:style {:color "red"
              :backgroundColor "silver"
              :border "1px solid black"
              :padding 10}}
     "Hello, I am 'app'"]]

   [:section
    [:h1
     "Array child:"]
    [:ul
     (for [i (range 10)]
       [:li
        {:key i}
        (str "Hi from " i)])]]

   [:section
    [:h1
     "User component:"]
    [child-comp
     {:msg "message from app"}]]

   [:section
    [:h1
     "comp-2 component:"]
    [click-counter]]
   
   [:section
    [:h1
     "Infinity list test"]
    [top-ranker
     {:ranker-list (cycle ["A" "B" "C"])}]]

   [:section
    [:h1
     "Input"]
    [input-test]]])

(defn ^:export init []
  (r/render #'app
          (js/document.getElementById "app")))

(defn ^:dev/after-load reload []
  (let [dom (js/document.getElementById "app")]
    (r/unmount-component-at-node dom)
    (r/render #'app
              dom)))
