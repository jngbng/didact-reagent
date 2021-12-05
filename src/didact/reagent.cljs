(ns didact.reagent
  (:require ["react" :as react]
            ["react-dom" :as react-dom]
            [goog.object]))

(def ^:dynamic *captured* (atom []))

(defn- notify-deref-watcher!
  "Add `derefed` to `*captured*`."
  [derefed]
  (swap! *captured*
         conj
         derefed))

(deftype RAtom [atom_]
  IAtom

  IDeref
  (-deref [this]
    (notify-deref-watcher! this)
    (deref atom_))

  IReset
  (-reset! [_this new-value]
    (reset! atom_ new-value))

  IWatchable
  (-notify-watches [_this old new]
    (-notify-watches atom_ old new))
  (-add-watch [_this key f]
    (add-watch atom_ key f))
  (-remove-watch [_this key]
    (remove-watch atom_ key)))

(defn ratom
  "Like clojure.core/atom, except that it keeps track of derefs."
  [x]
  (->RAtom (atom x)))

(declare as-element)

(defn- make-element [argv component jsprops first-child-idx]
  (apply react/createElement
         component
         jsprops
         (->> argv
              (drop first-child-idx)
              (map as-element))))

(defn- hiccup-element [tag argv]
  (let [component (name tag)
        props (second argv)
        has-props? (map? props)
        jsprops (or (when has-props?
                      (clj->js props))
                    #js {})
        first-child-idx (if has-props? 2 1)]
    (make-element argv
                  component
                  jsprops
                  first-child-idx)))

(defn- do-render [^js this]
  (let [^IFn render-fn (.-reagentRender this)
        res (let [v (-> this .-props .-argv)]
              (apply render-fn
                     (rest v)))]
    (cond
      (vector? res) (as-element res)
      (ifn? res) (do
                   (set! (.-reagentRender this) res)
                   (recur this))
      :else res)))

(defn- add-watcher [this ratoms]
  (when (seq ratoms)
    (doseq [deps-ratom_ ratoms]
      (add-watch deps-ratom_
                 this
                 (fn [_key _atom _old _new]
                   (.forceUpdate this))))
    (goog.object/set this
                     "_ratoms"
                     ratoms)))

(defn- clear-watcher [this]
  (when-some [^clj prev-ratoms (goog.object/get this
                                                "_ratoms")]
    (doseq [deps-ratom_ prev-ratoms]
      (remove-watch deps-ratom_
                    this))))

(defn- create-class [render-fn]
  (let [cmp (fn [props context updater]
              (this-as this
                (.call react/Component
                       this props context updater)
                this))]
    (goog.object/extend (.-prototype cmp)
      ;; inherit
      (.-prototype react/Component)
      ;; override & define
      #js {:render
           (fn render []
             (this-as this
               (clear-watcher this)
               (let [ratoms_ (atom [])]
                 (binding [*captured* ratoms_]
                   (let [res (do-render this)
                         ratoms @ratoms_]
                     (add-watcher this
                                  ratoms)
                     res)))))

           :reagentRender
           render-fn

           :componentWillUnmount
           (fn componentWillUnmount []
             (this-as this
               (clear-watcher this)))

           :shouldComponentUpdate
           (fn shouldComponentUpdate [nextprops _nextstate]
             (this-as c
               (let [old-argv (.. c -props -argv)
                     new-argv (.-argv nextprops)
                     noargv (or (nil? old-argv)
                                (nil? new-argv))]
                 (or noargv
                     (not= old-argv
                           new-argv)))))})

    (set! (.. cmp -prototype -constructor) cmp)

    (when-let [display-name (.-name render-fn)]
      (set! (.-displayName cmp) display-name))

    cmp))

(def ^:private react-class-cache-key "cached")

(defn- cached-react-class [tag]
  (goog.object/get tag
                   react-class-cache-key))

(defn- cache-react-class [tag react-class]
  (goog.object/set tag
                   react-class-cache-key
                   react-class)
  react-class)

(defn- fn-to-class [tag]
  (let [react-class (create-class tag)]
    (cache-react-class tag
                       react-class)))

(defn- as-class [tag]
  (if-some [cached-class (cached-react-class tag)]
    cached-class
    (fn-to-class tag)))

(defn- fn-to-element
  "클래스 컴포넌트"
  [tag v]
  (let [class-comp (as-class tag)
        jsprops #js {:argv v}]
    (react/createElement class-comp
                         jsprops)))

(defn- vec-to-elem [v]
  (let [tag (first v)]
    (if (keyword? tag)
      (hiccup-element tag
                      v)
      (fn-to-element tag
                     v))))

(defn- expand-seq [v]
  (-> (map as-element
           v)
      (into-array)))

(defn- as-element
  "Compile reagent component to react component"
  [comp]
  (cond
    (vector? comp) (vec-to-elem comp)
    (seq? comp) (expand-seq comp)
    :else comp))

(defn render [comp container]
  (let [react-comp (as-element (if (fn? comp)
                                 (comp)
                                 comp))]
    (react-dom/render react-comp
                      container)))

(defn unmount-component-at-node [container]
  (react-dom/unmountComponentAtNode container))
