(ns schizoid.data-learner
  (:require [schizoid.trigram-repo :as trig]
            [schizoid.tokenizer :as token]))

;; TODO maybe pass channel-id and content directly
(defn learn
  "Split `message` text to trigrams and store them."
  [message]
  (let [channel-id (:channel-id message)
        message-text (:content message)
        words (token/extract-words message-text)
        trigrams (token/split-to-trigrams words)]
    (trig/store channel-id trigrams)))
