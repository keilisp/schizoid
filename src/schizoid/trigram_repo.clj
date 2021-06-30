(ns schizoid.trigram-repo
  (:require [clojure.string :as str])
  (:require [taoensso.carmine :as car :refer (wcar)]))

(def server-connection {:pool {}
                        :spec {:host "localhost"
                               :port 6379
                               :timeout 4000}})

(defmacro wcar* [& body] `(car/wcar server-connection ~@body))

;; FIXME refactor this abomination
(defn store
  [chat-id trigrams]
  (let [counter-key (format "trigrams:count:%s" chat-id) 
        new-trigs-count  (count (filter #(= 0 %)
                                        (map (fn [trigram]
                                               (let [key
                                                     (format "trigrams:%s:%s" chat-id (str/join "$" (butlast trigram)))]
                                                 (wcar* (car/exists key)))) trigrams)))]
    
    (wcar* (car/incrby counter-key new-trigs-count))

    (map (fn [trigram]
           (let [key
                 (format "trigrams:%s:%s" chat-id (str/join "$" (butlast trigram)))
                 last-word (last trigram)]
             (wcar* (car/sadd key last-word)))) trigrams)))


(defn get-random-reply
  [chat-id key stop-word]
  (let [key  (format "trigrams:%s:%s" chat-id key)
        reply (wcar* (car/srandmember key))]
    (when (not= reply stop-word)
      reply)))

