(ns schizoid.reply-generator
  (:require [schizoid.tokenizer :as token]))

;; REVIEW into []
;; TODO
(defn generate
  [message]
  (let [words (token/extract-words message)
        trigrams (token/split-to-trigrams words)
        pairs (into [] (map #(into [] (butlast %)) trigrams))]
    pairs))
