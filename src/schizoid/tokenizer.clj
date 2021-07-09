(ns schizoid.tokenizer
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def chain-len (-> "config.edn" slurp edn/read-string :grammar :chain-len))
(def stop-word (-> "config.edn" slurp edn/read-string :grammar :stop-word))

(def endsen (-> "config.edn" slurp edn/read-string :grammar :endsen))
(def garbage (-> "config.edn" slurp edn/read-string :grammar :garbage))

(defn append-stop-words
  [words]
  (reduce (fn [res word]
            (if (some #(= (last word) %) endsen)
              (conj res word stop-word)
              (conj res word))) [] words))

(defn stop-word-to-end
  [words]
  (if (not= (last words) stop-word)
    (conj words stop-word)
    words))

(defn split-to-trigrams
  [words]
  (let [with-stop-words ((comp stop-word-to-end append-stop-words) words)]
    (vec (for [i (range (- (count with-stop-words) chain-len))]
           (let [j (+ i chain-len 1)]
             (subvec with-stop-words i j))))))

(defn prettify-word
  [word]
  (let [lowercased (str/lower-case word)
        last-char (if (not (some #(= (last word) %) endsen))
                    ""
                    (last lowercased))
        prettified (str/join (remove garbage lowercased))]
    ;; FIXME what is 2 and why to > and not >=
    (if (and (seq prettified) (> (count prettified) 2))
      (str prettified last-char)
      nil)))

(defn extract-words
  [message]
  (let [words (str/split message #" ")]
    (vec (remove nil? (map #(prettify-word %) words)))))

(defn random-end-sentence-token []
  (rand-nth (vec endsen)))


