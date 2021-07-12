(ns schizoid.tokenizer
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def config (-> "config.edn" slurp edn/read-string))

(def chain-len (-> config :grammar :chain-len))
(def stop-word (-> config :grammar :stop-word))

(def endsen (-> config :grammar :endsen))
(def garbage (-> config :grammar :garbage))


(defn append-stop-words
  "Append `stop-word` to the `words` after the end of the sentence."
  [words]
  (reduce (fn [res word]
            (if (some #(= (last word) %) endsen)
              (conj res word stop-word)
              (conj res word))) [] words))

(defn stop-word-to-end
  "Append `stop-word` to the end of the `words` if not already."
  [words]
  (if (not= (last words) stop-word)
    (conj words stop-word)
    words))

(defn split-to-trigrams
  "Split `words` to trigrams."
  [words]
  (let [with-stop-words ((comp stop-word-to-end append-stop-words) words)]
    (vec (for [i (range (- (count with-stop-words) chain-len))]
           (let [j (+ i chain-len 1)]
             (subvec with-stop-words i j))))))

(defn prettify-word
  "Lowercase and remove `garbage` from word."
  [word]
  (let [lowercased (str/lower-case word)
        last-char (if-not (some #(= (last word) %) endsen)
                    ""
                    (last lowercased))
        prettified (-> lowercased
                        (remove garbage)
                        str/join)]
    (if (and (seq prettified)
             (> (count prettified) 2))
      (str prettified last-char)
      nil)))

(defn extract-words
  "Extract words from `message`, delete garbage-entities and `prettify`."
  [message]
  (let [words (str/split message #" ")

        ;; FIXME 
        url-pattern #"(?i)^(?:(?:https?|ftp)://)(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)(?:\.(?:[a-z\u00a1-\uffff0-9]-*)*[a-z\u00a1-\uffff0-9]+)*(?:\.(?:[a-z\u00a1-\uffff]{2,}))\.?)(?::\d{2,5})?(?:[/?#]\S*)?$"

        email-pattern #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"

        user-mention-pattern #"<@!\d+>"
        channel-mention-pattern #"<#\d+>"
        ;; FIXME probably would be cool not to trim emotes and store pairs with them
        emotes-pattern #":\S+:"
        slash-commands-pattern #"/\S+"

        words-without-entities (filter #(not (or (re-matches user-mention-pattern %)
                                                 (re-matches channel-mention-pattern %)
                                                 (re-matches emotes-pattern %)
                                                 (re-matches slash-commands-pattern %)
                                                 (re-matches email-pattern %)
                                                 (re-matches url-pattern %))) words)]

    (->> words-without-entities
         (map #(prettify-word %))
         (remove nil?)
         vec)))

(defn random-end-sentence-token []
  "Return random symbol from `endsen` to end the sentence with."
  (rand-nth (vec endsen)))


