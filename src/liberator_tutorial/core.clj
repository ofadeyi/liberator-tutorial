(ns liberator-tutorial.core
  (:require [liberator.core :refer [resource defresource]]
            [liberator.dev :refer [wrap-trace]]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.core :refer [defroutes ANY]]
            [clojure.data.json :as json]
            [clojure.java.io :as io])
  (:import (java.net URL)))

;; Using functions to deref a Ref type
(def counter (ref 0))

;; Variations of Resource definitions
(defresource example
             :available-media-types ["text/plain"]
             :handle-ok "This is the example")

;; Parametrized Resources
(defresource parameter [txt]
             :available-media-types ["text/plain"]
             :handle-ok (fn [_] (format "The text is %s" txt)))

;; POST
(def posts (ref []))

;; PATCH
(def content (ref ["Replace part or all of this string."]))

;; All together (full resource)
;; convert the body to a reader. Useful for testing in the repl
;; where setting the body to a string is much simpler.
(defn body-as-string [ctx]
  (if-let [body (get-in ctx [:request :body])]
    (condp instance? body
      java.lang.String body
      (slurp (io/reader body)))))

;; For PUT and POST parse the body as json and store in the context
;; under the given key.
(defn parse-json [ctx key]
  (when (#{:put :post} (get-in ctx [:request :request-method]))
    (try
      (if-let [body (body-as-string ctx)]
        (let [data (json/read-str body)]
          [false {key data}])
        {:message "No body"})
      (catch Exception e
        (.printStackTrace e)
        {:message (format "IOException: %s" (.getMessage e))}))))

;; For PUT and POST check if the content type is json.
(defn check-content-type [ctx content-types]
  (if (#{:put :post} (get-in ctx [:request :request-method]))
    (or
      (some #{(get-in ctx [:request :headers "content-type"])}
            content-types)
      [false {:message "Unsupported Content-Type"}])
    true))

;; we hold a entries in this ref
(defonce entries (ref {}))

;; a helper to create a absolute url for the entry with the given id
(defn build-entry-url [request id]
  (URL. (format "%s://%s:%s%s/%s"
                (name (:scheme request))
                (:server-name request)
                (:server-port request)
                (:uri request)
                (str id))))


;; create and list entries
(defresource list-resource
             :available-media-types ["application/json"]
             :allowed-methods [:get :post]
             :known-content-type? #(check-content-type % ["application/json"])
             :malformed? #(parse-json % ::data)
             :post! #(let [id (str (inc (rand-int 100000)))]
                       (dosync (alter entries assoc id (::data %)))
                       {::id id})
             :post-redirect? true
             :location #(build-entry-url (get % :request) (get % ::id))
             :handle-ok #(map (fn [id] (str (build-entry-url (get % :request) id)))
                              (keys @entries)))

;; retrieve, update and dalete entry
(defresource entry-resource [id]
             :allowed-methods [:get :put :delete]
             :known-content-type? #(check-content-type % ["application/json"])
             :exists? (fn [_]
                        (let [e (get @entries id)]
                          (if-not (nil? e)
                            {::entry e})))
             :existed? (fn [_] (nil? (get @entries id ::sentinel)))
             :available-media-types ["application/json"]
             :handle-ok ::entry
             :delete! (fn [_] (dosync (alter entries assoc id nil)))
             :malformed? #(parse-json % ::data)
             :can-put-to-missing? false
             :put! #(dosync (alter entries assoc id (::data %)))
             :new? (fn [_] (nil? (get @entries id ::sentinel))))

(defroutes app
           (ANY "/" [] (resource))

           ;; Defining routes
           (ANY "/foo-res" [] (resource :available-media-types ["text/html"]
                                        :handle-ok "<html>Hello, Internet.</html>"))

           ;; Defining routes with functions
           (ANY "/foo-func" []
             (resource :available-media-types ["text/html"]
                       :handle-ok
                       (fn [ctx]
                         (format "<html>It's %d milliseconds since the beginning of the epoch.</html>"
                                 (System/currentTimeMillis)))))

           ;; Using functions to deref a Ref type
           (ANY "/foo-func-ref" []
             (resource :available-media-types ["text/html"]
                       :handle-ok (fn [_] (format "The counter is %d" @counter))))

           ;; Variations of Resource definitions
           (ANY "/foo" [] example)

           ;; Parametrized Resources
           (ANY "/bar/:txt" [txt] (parameter txt))

           ;; Example decision graph
           (ANY "/secret" []
             (resource :available-media-types ["text/html"]
                       :exists?
                       (fn [ctx]
                         (= "tiger" (get-in ctx [:request :params "word"])))
                       :handle-ok "You found the secret word!"
                       :handle-not-found "Uh, that's the wrong word. Guess again!"))
           (ANY "/choice" []
             (resource :available-media-types ["text/html"]
                       :exists?
                       (fn [ctx]
                         (if-let [choice
                                  (get {"1" "rock" "2" "paper" "3" "scissors"}
                                       (get-in ctx [:request :params "choice"]))]
                           {:choice choice}))
                       :handle-ok
                       (fn [ctx]
                         (format "<html>Your choice: &quot;%s&quot;.</html>"
                                 (get ctx :choice)))
                       :handle-not-found
                       (fn [ctx]
                         (format "<html>There is no value for the option &quot;%s&quot;"
                                 (get-in ctx [:request :params "choice"] "")))))

           ;; Content Negotiation
           (ANY "/babel" []
             (resource :available-media-types ["text/plain" "text/html"
                                               "application/json" "application/clojure;q=0.9"]
                       :handle-ok
                       #(let [media-type
                              (get-in % [:representation :media-type])]
                          (condp = media-type
                            "text/plain" "You requested plain text"
                            "text/html" "<html><h1>You requested HTML</h1></html>"
                            {:message    "You requested a media type"
                             :media-type media-type}))
                       :handle-not-acceptable "Uh, Oh, I cannot speak those languages!"))

           ;; Conditional Requests
           ;; Last-Modified
           (ANY "/timehop" []
             (resource
               :available-media-types ["text/plain"]
               ;; timestamp changes every 10s
               :last-modified (* 10000 (long (/ (System/currentTimeMillis) 10000)))
               :handle-ok (fn [_] (format "It's now %s" (java.util.Date.)))))
           ;; Etags
           (ANY "/changetag" []
             (resource
               :available-media-types ["text/plain"]
               ;; etag changes every 10s
               :etag (let [i (int (mod (/ (System/currentTimeMillis) 10000) 10))]
                       (.substring "abcdefhghijklmnopqrst" i (+ i 10)))
               :handle-ok (format "It's now %s" (java.util.Date.))))

           ;; POST
           (ANY "/postbox" []
             (resource
               :allowed-methods [:post :get]
               :available-media-types ["text/html"]
               :handle-ok (fn [ctx]
                            (format (str "<html>Post text/plain to this resource.<br>\n"
                                         "There are %d posts at the moment.</html>")
                                    (count @posts)))
               :post! (fn [ctx]
                        (dosync
                          (let [body (slurp (get-in ctx [:request :body]))
                                id (count (alter posts conj body))]
                            {::id id})))
               ;; actually http requires absolute urls for redirect but let's
               ;; keep things simple.
               :post-redirect? (fn [ctx] {:location (format "/postbox/%s" (::id ctx))})
               :etag (fn [_] (str (count @posts)))))
           (ANY "/postbox/:x" [x]
             (resource
               :allowed-methods [:get]
               :available-media-types ["text/html"]
               :exists? (fn [ctx] (if-let [d (get @posts (dec (Integer/parseInt x)))] {::data d}))
               :handle-ok ::data))

           ;; PATCH
           (ANY "/patchbox" []
             (resource
               :allowed-methods [:patch :get]
               :available-media-types ["text/html"]
               :handle-ok (fn [ctx]
                            (format (str "<html><body>\n"
                                         "The current content is:<br/>"
                                         (last @content)
                                         "Patch text/plain to this resource.<br/>"
                                         "Send a string in the format foo|bar.<br/>"
                                         "Any instance of the string to the left<br/>"
                                         "of the '|' will be replaced with the<br/>"
                                         "string to the right.<br/>"
                                         "There have been %d patches issued.<br/>"
                                         "</body></html>")
                                    (dec (count @content))))
               :patch-content-types ["text/plain"]
               :patch! (fn [ctx]
                         (dosync
                           (let [body (slurp (get-in ctx [:request :body]))
                                 parts (clojure.string/split body #"\|")
                                 replaced (clojure.string/replace
                                            (last @content)
                                            (re-pattern (first parts))
                                            (last parts))
                                 id (count (alter content conj replaced))]
                             {::id id})))))

           ;; All together (full resource)
           (ANY ["/collection/:id{[0-9]+}"] [id] (entry-resource id))
           (ANY "/collection" [] list-resource)
           )

(def handler
  (-> app
      wrap-params))

;; Debugging
;(def dbg-counter (atom 0))
;
;(defresource dbg-resource
;  :available-media-types ["text/plain"]
;  :allowed-methods [:get :post]
;  :handle-ok (fn [_] (format "The counter is %d" @dbg-counter))
;  :post! (fn [_] (swap! dbg-counter inc)))
;
;(defroutes dbg
;  (ANY "/dbg-count" [] dbg-resource))
;
;(def handler
;  (-> dbg
;      (wrap-trace :header :ui)))