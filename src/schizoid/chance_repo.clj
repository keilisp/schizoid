(ns schizoid.chance-repo
  (:require [schizoid.trigram-repo :as trig]
            [schizoid.tokenizer :as token]
            [clojure.edn :as edn]
            [taoensso.carmine :as car :refer (wcar)]))

(def config (-> "config.edn" slurp edn/read-string))

(def server-connection (-> config :redis))
(defmacro wcar* [& body] `(car/wcar server-connection ~@body))

(def default-chance (-> config :bot :default-chance))

;; Execution error (NumberFormatException) at java.lang.Integer/parseInt (Integer.java:614).

(defn get-chance
  "Get current chance of bot reply for `channel-id`."
  [channel-id]
  (let [key (format "chance:%s" channel-id)
        chance (wcar* (car/get key))]
    (if (some? chance)
      (Integer/parseInt chance)
      default-chance)))

(defn set-chance
  "Set new reply chance for `channel-id` and return old."
  [channel-id new-chance]
  (let [key (format "chance:%s" channel-id)
        old-chance (wcar* (car/getset key new-chance))]
    (if (some? old-chance)
      (Integer/parseInt old-chance)
      default-chance)))
