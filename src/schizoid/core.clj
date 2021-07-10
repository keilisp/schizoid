(ns schizoid.core
  (:require [schizoid.tokenizer :as token]
            [schizoid.reply-generator :as reply]
            [schizoid.data-learner :as dlearner]
            [schizoid.chance-repo :as chance]
            [schizoid.trigram-repo :as trig]
            [schizoid.message :as message]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.async    :as async]
            [discljord.connections :as conns]
            [discljord.messaging   :as msgs]
            [discljord.events :as events]
            [clojure.tools.logging :as log])
  (:gen-class))

(def bot-token (-> "secrets.edn" slurp edn/read-string :bot-token))

(def state (atom nil))
(def mode (atom {:mode 'polling}))

#_(defn handle-message
    [event-type event-data]
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

(defn learn-message
  "Handler just to learn on user's messages."
  [event-type event-data]
  (log/info (format "[Chat %s] message length %s" (:channel-id event-data) (count (:content event-data))))
  (when (= (:content event-data) "!stop-polling")
    (swap! mode assoc :mode 'chilling))
  (when (= (:content event-data) "!start-polling")
    (swap! mode assoc :mode 'polling))
  (when (= (:mode @mode) 'polling)
    (when-not (:bot event-data)
      (let [channel-id (:channel-id event-data)]
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
