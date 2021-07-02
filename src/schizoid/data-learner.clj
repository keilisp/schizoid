(ns schizoid.data-learner
  (:require [schizoid.trigram-repo :as trig])
  (:require [schizoid.tokenizer :as token]))

;; TODO extract chat-id from message metadata
(defn learn
  [message]
  (let [words (token/extract-words message)
        trigrams (token/split-to-trigrams words)]
    (trig/store "123" trigrams)))
