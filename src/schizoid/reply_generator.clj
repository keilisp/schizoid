(ns schizoid.reply-generator
  (:require [schizoid.tokenizer :as token]
            [clojure.string :as str]
            [schizoid.trigram-repo :as trig]
            [clojure.edn :as edn]))

(def stop-word (-> "config.edn" slurp edn/read-string :grammar :stop-word))
(def endsen (-> "config.edn" slurp edn/read-string :grammar :endsen))
(def max-messages (-> "config.edn" slurp edn/read-string :grammar :max-messages))

;; TODO FIXME HACK looks like an abomination to me
(defn generate-sentence
  "Generate sentence from given `pair`.
    For example, we recieve pair (hello, bot). We will convert it to the string 'hello$bot' and write it to the key.
    Then we iterate a maximum of 50 times. At each iteration:
        
     1. Convert the key back to a pair, 'hello $ bot' will become
        again (hello, bot)
     2. Add the second or first word of the pair
        to gen_words, depending on the size of gen_words (This is what
        forms the result)
     3. Get a random word using the hello $ bot
        pair.
     4.1. If the word is not found, then we interrupt the
        cycle, this means that this pair has no relations in the base
     4.2. If a word is found, for example, 'clown', then we form a
        new key from a pair, for example, there will be a bot$clown
        (key will ALWAYS consist of 2 words) and repeat
  "
  [channel-id pair]
  (let [init-words (loop [i 0
                          key (str/join "$" pair)
                          gen-words {:words []}]
                     (let [words (str/split key #"\$")
                           next-word (trig/get-random-reply channel-id key 0)]
                       (if (and (< i 50) (some? next-word))
                         (recur (inc i)
                                (str/join "$" (conj [] (nth words 1) next-word))
                                (assoc gen-words :words (conj (:words gen-words) (nth words 1)) :key key))
                         (assoc gen-words :key key))))

        last-word (last (str/split (:key init-words) #"\$"))]
    (let [words (set (if (not (contains? (init-words :words) last-word))
                       (conj (:words init-words) last-word)
                       (:words init-words)))
          filtered-words (vec (remove #{stop-word} words))
          sentence (str/trim (str/join " " words))
          sentence (if (not (some #{(last sentence)} endsen))
                     (str sentence (token/random-end-sentence-token))
                     sentence)]
      sentence)))

(defn generate-best-sentence
  "Generate few sentences and return the longest one."
  [channel-id pair]
  (let [sentence  (loop [i 0
                         best-message ""]
                    (when (< i max-messages)
                      (let [generated (generate-sentence channel-id pair)]
                        (if (> (count generated) (count best-message))
                          (recur (inc i) generated)
                          best-message))))]
    (str/capitalize sentence)))

(defn generate
  "Generate response base on given `message`."
  [message]
  (let [channel-id (:channel-id message)
        message-text (:content message)
        words (token/extract-words message-text)
        trigrams (token/split-to-trigrams words)
        pairs (into [] (map #(vec (butlast %)))  trigrams)
        messages (into [] (map #(generate-best-sentence channel-id %)) pairs)
        longest-message (last (sort-by count messages))
        longest-message (if (and longest-message (identical? longest-message (str/join "" words)))
                          nil
                          longest-message)]
    longest-message))
