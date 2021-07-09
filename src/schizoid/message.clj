(ns schizoid.message
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [schizoid.chance-repo :as chance]))

(def anchors (-> "config.edn" slurp edn/read-string :bot :anchors))


;;;; Message objects

;;; Just random message with text and mentions
;; {:mention-everyone false,
;;  :mentions [{:username "Tance",
;;              :public-flags 0,
;;              :member {:roles [],
;;                       :mute false,
;;                       :joined-at "2021-05-21T16:29:32.855000+00:00",
;;                       :hoisted-role nil,
;;                       :deaf false},
;;              :id "593186395246428161",
;;              :discriminator "9673",
;;              :avatar "9e53066f021e224d63e072f2cec16045"}
;;             {:username "mediocreBot",
;;              :public-flags 0,
;;              :member {:roles ["839645752523358250"],
;;                       :mute false,
;;                       :joined-at "2021-05-05T23:32:27.389000+00:00",
;;                       :hoisted-role nil,
;;                       :deaf false},
;;              :id "615126051680681994",
;;              :discriminator "8020",
;;              :bot true,
;;              :avatar "9db02128c4cdb60212aae416b63f0d0b"}
;;             {:username "Schizoid",
;;              :public-flags 0,
;;              :member {:roles [],
;;                       :mute false,
;;                       :joined-at "2021-05-21T17:27:38.284000+00:00",
;;                       :hoisted-role nil,
;;                       :deaf false},
;;              :id "845341783965565028",
;;              :discriminator "0904",
;;              :bot true,
;;              :avatar "0b8e2600823f411510d2e5f2099ff135"}],
;;  :pinned false,
;;  :content "test message with menitons: <@!593186395246428161> <@!615126051680681994> , threads: <#839645600798343192> , mention of bot itself <@!845341783965565028> , slash commands: /test /qwer , bot aliases: shiz шизоид шиз",
;;  :attachments [],
;;  :mention-roles [],
;;  :type 0,
;;  :referenced-message nil,
;;  :guild-id "839645600798343188",
;;  :author {:username "M-x 𝜆",
;;           :public-flags 0,
;;           :id "399637644843155457",
;;           :discriminator "1471",
;;           :avatar "75ba0a3639e2a5b614d44005c76ee0fc"},
;;  :member {:roles [],
;;           :mute false,
;;           :joined-at "2021-05-05T23:31:51.146000+00:00",
;;           :hoisted-role nil,
;;           :deaf false},
;;  :components [],
;;  :id "862644490321788948",
;;  :channel-id "839645600798343192",
;;  :embeds [],
;;  :timestamp "2021-07-08T10:41:13.652000+00:00",
;;  :flags 0,
;;  :nonce "862644489364176896",
;;  :tts false,
;;  :edited-timestamp nil

;;;; TODO FIXME pass necessary args from event-data directly 


(defn has-text?
  [message]
  (let [text (:content message)]
    (and (some? text) (not (str/blank? text)))))

(defn is-sticker?
  [message]
  (let [stickers (:sticker_itmes message)]
    (and (some? stickers) (not (empty? stickers)))))

(defn was-edited?
  [message]
  (some? (:edited-timestamp message)))

(defn has-mentions?
  [message]
  (not (empty? (:mentions message))))

(defn has-attachments?
  [message]
  (not (empty? (:attachments message))))

(defn has-anchors?
  [message]
  (let [text (:content message)
        words-vec (str/split (str/trim text) #" ")
        mentions (map #(:username %) (:mentions message))]
    (or (some #{"Sсhizoid"} mentions)
        (->> anchors
             (map #(str/includes? text %))
             (some #{true})))))

;; TEST
(defn is-reply-to-bot?
  [message]
  (let [referenced-message (:referenced-message message)]
    (and (some? referenced-message)
         (= "Schizoid" (:username (:author referenced-message))))))

;; FIXME
(defn is-random-answer?
  [message]
  (< (rand-int 100) (chance/get-chance (:channel-id message))))

(defn should-answer?
  [message]
  (or (has-anchors? message)
      (is-reply-to-bot? message)
      (is-random-answer? message)))
