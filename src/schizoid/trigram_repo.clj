(ns schizoid.trigram-repo
  (:require [clojure.string :as str])
  (:require [taoensso.carmine :as car :refer (wcar)])
  ;; (:import [java.util.regex Pattern])
  )

;; TODO:
;; 1. extracs source and counter patterns to local variables
;; 2. do something about separtor -> regex

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

(defn count-chat-pairs
  [chat-id]
  (let [key (format "trigrams:count:%s" chat-id)]
    (Integer/parseInt (wcar* (car/get key)))))

;; TODO add destructuring for records
(defn remove-keys
  [pattern]
  (let [[_ records] (wcar* (car/scan 0 :match pattern))]
    (map #(wcar* (car/del %)) records)))


(defn clear
  [chat-id]
  (let [pattern (format "trigrams:%s:*" chat-id)
        counter-key (format "trigrams:count:%s" chat-id)]
    (remove-keys pattern)
    (wcar* (car/del counter-key))))

(defn find-word
  [chat-id similar-word max-results]
  (let [format-pattern (format "trigrams:%s:" chat-id)
        search-pattern (format "trigrams:%s:*%s*" chat-id similar-word)
        [_ records] (wcar* (car/scan 0 :match search-pattern :count max-results))
        separator #"\$"]
    (take 10 (into [] (flatten (map (fn [record]
                              (let [pair (str/split (str/replace record format-pattern "") separator)]
                                (filter #(str/starts-with? % similar-word) pair))) records))))))

