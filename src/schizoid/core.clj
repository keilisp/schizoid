(ns schizoid.core
  (:require [schizoid.tokenizer       :as token]
            [schizoid.reply-generator :as reply]
            [schizoid.data-learner    :as dlearner]
            [schizoid.chance-repo     :as chance]
            [schizoid.trigram-repo    :as trig]
            [schizoid.message         :as message]
            [clojure.string           :as str]
            [clojure.edn              :as edn]
            [clojure.core.async       :as async]
            [discljord.connections    :as conns]
            [discljord.messaging      :as msgs]
            [discljord.permissions    :as perm]
            [discljord.formatting     :as fmt]
            [discljord.events         :as events]
            [clojure.tools.logging    :as log]
            [discljord.events.state :refer [prepare-guild]]
            [discljord.util :refer [parse-if-str]])
  (:gen-class))

(def bot-token (-> "secrets.edn" slurp edn/read-string :bot-token))

(def state (atom nil))
(def mode (atom {:mode 'polling}))

#_(defn handle-message
    [event-type event-data]
 ;; [event-type {{bot :bot} :author :keys [channel-id content]}]
    (if (= (:content event-data) "!disconnect")
      (async/put! (:connection @state) [:disconnect])
      (when-not (:author (:bot event-data))
        (let [should-answer? (message/should-answer? event-data)
              channel-id (:channel-id event-data)]
          (when should-answer? (msgs/trigger-typing-indicator! (:messaging @state) channel-id))
          (dlearner/learn event-data)
          (when should-answer?
            (when-let [reply-text (reply/generate event-data)]
              (msgs/create-message! (:messaging @state) channel-id :content reply-text)))))))

;; REVIEW version with async/go block
(defn has-permissions? [user-id channel-id permissions]
  (let [{:keys [guild-id] :as channel} (async/<!! (msgs/get-channel! (:messaging @state) channel-id))
        member (async/<!! (msgs/get-guild-member! (:messaging @state) guild-id user-id))
        guild (-> (async/<!! (msgs/get-guild! (:messaging @state) guild-id))
                  (assoc :channels [(update channel :permission-overwrites
                                            (fn [overrides]
                                              (mapv #(-> % (update :allow parse-if-str)
                                                         (update :deny parse-if-str))
                                                    overrides)))])
                  (assoc :members [member])
                  (update :roles (fn [roles] (mapv #(update % :permissions parse-if-str) roles)))
                  (prepare-guild))]
    (perm/has-permissions?
     permissions
     guild user-id channel-id)))

;; FIXME TODO fork discljord and implement slash commands (https://discord.com/developers/docs/interactions/slash-commands#authorizing-your-application)
(defn process-command
  [{:keys [channel-id content author guild-id member]}]
  (let [words (->   content
                    str/trim
                    (str/split #"\s"))
        [command & args] words
        guild (msgs/get-guild! (:messaging @state) guild-id)]
    (if (has-permissions? (:id author) channel-id #{:administrator})
      (case command
        "!stop-polling" (swap! mode assoc :mode 'chilling)
        "!start-polling" (swap! mode assoc :mode 'polling)
        "!mod_f" (if (not-empty args)
                   (let [finded (set (flatten (map #(->> %
                                                         str/trim
                                                         (trig/find-word channel-id)) args)))
                         reply (str/join "\n" finded)]
                     (println (str "finded: " finded))
                     (println (str "reply: " reply))
                     (if (str/blank? (str/trim reply))
                       (msgs/create-message! (:messaging @state) channel-id :content "Nothing found!")
                       (msgs/create-message! (:messaging @state) channel-id :content reply)))
                   (msgs/create-message! (:messaging @state) channel-id :content (format "%s You haven't supplied any words to find!" (fmt/mention-user author))))
        "!mod_d" (if (not-empty args)
                   (doall (map #(->> %
                                     str/trim
                                     (trig/remove-word channel-id)) args))
                   (msgs/create-message! (:messaging @state) channel-id :content (format "%s You haven't supplied any words for deletion!" (fmt/mention-user author)))))
      (msgs/create-message! (:messaging @state) channel-id :content (format "%s You're not an administrator!" (fmt/mention-user author))))))

(defn learn-message
  "Handler just to learn on user's messages."
  [event-type {{bot :bot} :author :keys [channel-id content author guild-id] :as event-data}]
  (log/info (format "[Chat %s] message length %s" channel-id (count content)))
  (when-not bot
    (if (message/is-command? event-data)
      (process-command event-data)
      (when (= (:mode @mode) 'polling)
        (dlearner/learn event-data)))))

(def handlers
  {:message-create [#'learn-message]})

(defn -main
  "Main loop for Bot."
  [& args]
  (let [event-ch (async/chan 100)
        connection-ch (conns/connect-bot! bot-token event-ch :intents #{:guilds :guild-members :guild-bans :guild-emojis
                                                                        :guild-integrations :guild-webhooks :guild-invites
                                                                        :guild-voice-states :guild-presences :guild-messages
                                                                        :guild-message-reactions :guild-message-typing
                                                                        :direct-messages :direct-message-reactions
                                                                        :direct-message-typing})
        messaging-ch (msgs/start-connection! bot-token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (log/info "Bot started!")
    (reset! state init-state)
    (try (events/message-pump! event-ch (partial events/dispatch-handlers #'handlers))
         (finally
           (msgs/stop-connection! messaging-ch)
           (conns/disconnect-bot! connection-ch)))))
