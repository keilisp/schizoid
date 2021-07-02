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

;; FIXME
(defn store
  [chat-id trigrams]
  (let [counter-key (format "trigrams:count:%s" chat-id)
        get-new-trigs-count (fn [trigrams] (->>  trigrams
                                                 (map (fn [trigram]
                                                        (let [pair (str/join "$" (butlast trigram))
                                                              key (format "trigrams:%s:%s" chat-id pair)]
                                                          (wcar* (car/exists key)))))
                                                 (filter zero?)
                                                 count))

        new-trigs-count  (get-new-trigs-count trigrams)

        update-trigs-counter (fn [counter-key new-trigs-count]
                               (wcar* (car/incrby counter-key new-trigs-count)))

        store-trigrams (fn [trigrams]
                         (map (fn [trigram]
                                (let [pair (str/join "$" (butlast trigram))
                                      key (format "trigrams:%s:%s" chat-id pair)
                                      last-word (last trigram)]
                                  (wcar* (car/sadd key last-word)))) trigrams))]

    ;; FIXME
    (update-trigs-counter counter-key new-trigs-count)
    (store-trigrams trigrams)))

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
    (take 10 (vec (flatten (map (fn [record]
                                  (let [pair (str/split (str/replace record format-pattern "") separator)]
                                    (filter #(str/starts-with? % similar-word) pair))) records))))))

(defn remove-word
  [chat-id exact-word]
  (let [first-key (format "trigrams:%s:%s%s*" chat-id exact-word "$")
        second-key (format "trigrams:%s:*%s%s" chat-id "$" exact-word)]
    (remove-keys first-key)
    (remove-keys second-key)))


