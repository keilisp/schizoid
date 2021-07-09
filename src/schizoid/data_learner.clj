(ns schizoid.data-learner
  (:require [schizoid.trigram-repo :as trig]
            [schizoid.tokenizer :as token]))

;; TODO maybe pass chat-id and content directly
(defn learn
  [message]
  (let [chat-id (:channel-id message)
        message-text (:content message)
        words (token/extract-words message-text)
        trigrams (token/split-to-trigrams words)]
    (trig/store chat-id trigrams)))
