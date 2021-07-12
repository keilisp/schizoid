(ns schizoid.data-learner
  (:require [schizoid.trigram-repo :as trig]
            [schizoid.tokenizer :as token]))

;; TODO maybe pass channel-id and content directly
(defn learn
  "Split `message` text to trigrams and store them."
  [{:keys [channel-id content author guild-id] :as event-data}]
  (let [words (token/extract-words content)
        trigrams (token/split-to-trigrams words)]
    (trig/store channel-id trigrams)))
