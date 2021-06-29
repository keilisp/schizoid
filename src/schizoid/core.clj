(ns schizoid.core
  (:gen-class))

(require '[clojure.string :as str])

(def *chain-lein* 2)
(def *stop-word* (byte 0x00))
;; (def *stop-word* "stopwrd")
;; (def *stop-word* 0)

;; TODO change to set
(def *endsen*  #{ \. \! \? })
(def *garbage* #{ \« \< \{ \( \[ \. \! \? \\ \- \— \] \/ \& \^ \# \$ \| \* \  \№ \; \: \) \} \> \» })


(defn tokenizer-append-stop-words
  [words]
  (reduce (fn [res word]
            (if (some #(= (last word) %) *endsen*)
               (conj res word *stop-word*)
               (conj res word))) [] words))

(defn tokenizer-stop-word-to-end
  [words]
  (if (not= (last words) *stop-word*)
    (conj words *stop-word*)
    words))

(defn tokenizer-split-to-trigrams
  [words]
  (let [with-stop-words ((comp tokenizer-stop-word-to-end tokenizer-append-stop-words ) words)]
    (into [] (for [i (range (- (count with-stop-words) *chain-lein*))]
               (let [j (+ i *chain-lein* 1)]
                 (subvec with-stop-words i j))))))

(defn tokenizer-prettify-word
  [word]
  (let [lowercased (str/lower-case word)
        last-char (if (not (some #(= (last word) %)*endsen*))
                    ""
                    (last lowercased))
        prettified (str/join (remove *garbage* lowercased))]
    (if (and (not (empty? prettified)) (>= (count prettified) 2))
      (str prettified last-char)
      nil)))

(defn tokenizer-extract-words
  [message]
  (let [words (str/split message #" ")]
    (into [] (remove nil? (map #(tokenizer-prettify-word %) words)))))

(defn tokenizer-random-end-sentence-token []
  (rand-nth *endsen*))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
