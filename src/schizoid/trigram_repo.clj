(ns schizoid.trigram-repo
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [taoensso.carmine :as car :refer (wcar)]))

;; TODO:
;; 1. extracs source and counter patterns to local variables
;; FIXME idk what to do with redis scan counter

(def config (-> "config.edn" slurp edn/read-string))

(def server-connection (-> config :redis))
(def separator (-> config :grammar :separator))
(def re-separator (re-pattern (str "\\" separator)))
(def max-results (-> config :grammar :max-words))

(defmacro wcar* [& body] `(car/wcar server-connection ~@body))

(defn update-trigs-counter
  "Update counter of trigrams for `channel-id`."
  [channel-id trigrams]
  (let [counter-key (format "trigrams:count:%s" channel-id)
        new-trigs-count (->>  trigrams
                              (map (fn [trigram]
                                     (let [pair (str/join separator (butlast trigram))
                                           key (format "trigrams:%s:%s" channel-id pair)]
                                       (wcar* (car/exists key)))))
                              (filter zero?)
                              count)]
    (log/info (format "%s new trigrams for %s" new-trigs-count channel-id))
    (wcar* (car/incrby counter-key new-trigs-count))))

(defn store-trigrams
  "Store new `trigrams` from `channel-id`."
  [channel-id trigrams]
  (doseq [trigram trigrams]
    (let [pair (str/join "$" (butlast trigram))
          key (format "trigrams:%s:%s" channel-id pair)
          last-word (last trigram)]
      (log/info (format "Stored %s for %s" trigrams channel-id))
      (wcar* (car/sadd key last-word)))))

(defn store
  "Update counter for `channel-id` and store new `trigrams`."
  [channel-id trigrams]
  ((juxt update-trigs-counter store-trigrams) channel-id trigrams))

(defn get-random-reply
  "Get random reply for `key` in `channel-id` from database."
  [channel-id key stop-word]
  (let [key  (format "trigrams:%s:%s" channel-id key)
        reply (wcar* (car/srandmember key))]
    (when (not= reply stop-word)
      reply)))

(defn count-chat-pairs
  "Count pairs for `channel-id`."
  [channel-id]
  (let [key (format "trigrams:count:%s" channel-id)]
    (Integer/parseInt (wcar* (car/get key)))))

(defn remove-keys
  "Remove all keys matching given `pattern`."
  [pattern]
  (let [[_ records] (wcar* (car/scan 0 :match pattern :count 1000))]
    (doall (map #(wcar* (car/del %)) records))))

(defn clear
  "Remove all trigrams from channel with `channel-id` from database."
  [channel-id]
  (let [pattern (format "trigrams:%s:*" channel-id)
        counter-key (format "trigrams:count:%s" channel-id)]
    (remove-keys pattern)
    (wcar* (car/del counter-key))))

(defn find-word
  "Search for words similar to given `similar-word` for `channel-id`."
  [channel-id similar-word]
  (let [format-pattern (format "trigrams:%s:" channel-id)
        search-pattern (format "trigrams:%s:*%s*" channel-id similar-word)
        [_ records] (wcar* (car/scan 0 :match search-pattern :count 1000))]
    (take 10 (vec (flatten (map (fn [record]
                                  (let [pair (str/split (str/replace record format-pattern "")
                                                        re-separator)]
                                    (filter #(str/starts-with? % similar-word) pair))) records))))))

(defn remove-word
  "Remove words with exact match to given `exact-word` for `channel-id`"
  [channel-id exact-word]
  (let [first-key (format "trigrams:%s:%s%s*" channel-id exact-word separator)
        second-key (format "trigrams:%s:*%s%s" channel-id separator exact-word)]
    (remove-keys first-key)
    (remove-keys second-key)))


