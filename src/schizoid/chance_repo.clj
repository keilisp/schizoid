(ns schizoid.chance-repo
  (:require [schizoid.trigram-repo :as trig]
            [schizoid.tokenizer :as token]
            [clojure.edn :as edn]
            [taoensso.carmine :as car :refer (wcar)]))

(def server-connection {:pool {}
                        :spec {:host "localhost"
                               :port 6379
                               :timeout 4000}})
(defmacro wcar* [& body] `(car/wcar server-connection ~@body))

(def default-chance (-> "config.edn" slurp edn/read-string :bot :default-chance))

;; Execution error (NumberFormatException) at java.lang.Integer/parseInt (Integer.java:614).

(defn get-chance
  [chat-id]
  (let [key (format "chance:%s" chat-id)
        chance (wcar* (car/get key))]
    (if (some? chance)
      (Integer/parseInt chance)
      default-chance)))

(defn set-chance
  [chat-id new-chance]
  (let [key (format "chance:%s" chat-id)
        old-chance (wcar* (car/getset key new-chance))]
    (if (some? old-chance)
      (Integer/parseInt old-chance)
      default-chance)))
