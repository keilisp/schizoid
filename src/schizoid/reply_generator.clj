(ns schizoid.reply-generator
  (:require [schizoid.tokenizer :as token])
  (:require [clojure.string :as str])
  (:require [schizoid.trigram-repo :as trig]))

;; TODO FIXME HACK

(def *stop-word* 0)
(def *endsen*  #{\. \! \?})
(def *max-messages* 10)

;; FIXME need to extract chat id from message

(defn generate-sentence
  [chat-id pair]
  (let [init-words (loop [i 0
                          key (str/join "$" pair)
                          gen-words {:words []}]
                     (let [words (str/split key #"\$")
                           next-word (trig/get-random-reply chat-id key 0)]
                       (if (and (< i 50) (some? next-word))
                         (recur (inc i)
                                (str/join "$" (conj [] (nth words 1) next-word))
                                (assoc gen-words :words (conj (:words gen-words) (nth words 1)) :key key))
                         (assoc gen-words :key key))))
        last-word (last (str/split (:key init-words) #"\$"))]
    (let [words (set (if (not (contains? (init-words :words) last-word))
                       (conj (:words init-words) last-word)
                       (:words init-words)))
          filtered-words (vec (remove #{*stop-word*} words))
          sentence (str/trim (str/join " " words))
          sentence (if (not (some #{(last sentence)} *endsen*))
                     (str sentence (token/random-end-sentence-token))
                     sentence)]
      sentence)))

(defn generate-best-sentence
  [chat-id pair]
  (let [sentence  (loop [i 0
                         best-message ""]
                    (when (< i *max-messages*)
                      (let [generated (generate-sentence chat-id pair)]
                        (if (> (count generated) (count best-message))
                          (recur (inc i) generated)
                          best-message))))]
    (str/capitalize sentence)))

(defn generate
  [message]
  (let [words (token/extract-words message)
        trigrams (token/split-to-trigrams words)
        pairs (into [] (map #(vec (butlast %)))  trigrams)
        messages (into [] (map #(generate-best-sentence "123" %)) pairs)
        longest-message (last (sort-by count messages))
        longest-message (if (and longest-message (identical? longest-message (str/join "" words)))
                          nil
                          longest-message)]
    longest-message))
