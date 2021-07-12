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
;;;; TODO FIXME consider adding if statements and return `false`


(defn has-text?
  "Check if message is not empty."
  [{:keys [content]}]
  (and (some? content)
       (not (str/blank? content))))

(defn is-command?
  "Check if message is command for bot."
  [{:keys [content]}]
  (re-matches #"!\w+.*$" content))

(defn was-edited?
  "Check if message was edited."
  [{:keys [edited-timestamp]}]
  (some? edited-timestamp))

(defn has-mentions?
  "Check if message mentions other users or bots."
  [{:keys [mentions]}]
  (not (empty? mentions)))

(defn has-attachments?
  "Check if message has attachments (photos, vides, etc..)"
  [{:keys [attachments]}]
  (not (empty? attachments)))

(defn has-anchors?
  "Check if message mentions Bot."
  [{:keys [content mentions]}]
  (let [words-vec (str/split (str/trim content) #" ")
        mentions (map #(:username %) mentions)]
    (or (some #{"Schizoid"} mentions)
        (->> anchors
             (map #(str/includes? content %))
             (some #{true})))))

;; TEST
(defn is-reply-to-bot?
  "Check if message has reference to Bot's message."
  [{:keys [referenced-message]}]
  (and (some? referenced-message)
        (= "Schizoid" (:username (:author referenced-message)))))

;; FIXME
(defn is-random-answer?
  "Check if replay chance for this channel is high enough."
  [{:keys [channel-id]}]
  (< (rand-int 100)
     (chance/get-chance channel-id)))

(defn should-answer?
  "Check if bot should answer to this message."
  [message]
  (or (has-anchors? message)
      (is-reply-to-bot? message)
      (is-random-answer? message)))
