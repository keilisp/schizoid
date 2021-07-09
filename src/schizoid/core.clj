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
            [discljord.messaging   :as mess]
            [discljord.events :as events])
  (:gen-class))

(def bot-token (-> "secrets.edn" slurp edn/read-string :bot-token))

(def state (atom nil))

#_(defn handle-message
  [event-type event-data]
  (if (= (:content event-data) "!disconnect")
    (async/put! (:connection @state) [:disconnect])
    (when-not (:bot event-data)
      (let [should-answer? (message/should-answer? event-data)
            chat-id (:channel-id event-data)]
        (when should-answer? (mess/trigger-typing-indicator! (:messaging @state) chat-id))
        (dlearner/learn event-data)
        (when should-answer?
          (when-let [reply-text (reply/generate event-data)]
            (mess/create-message! (:messaging @state) chat-id :content reply-text)))))))

(defn learn-message
  [event-type event-data]
  (when-not (:bot event-data)
    (let [chat-id (:channel-id event-data)]
      (dlearner/learn event-data))))

(def handlers
  {:message-create [#'learn-message]})

(defn -main
  [& args]
  (let [event-ch (async/chan 100)
        connection-ch (conns/connect-bot! bot-token event-ch :intents #{:guilds :guild-members :guild-bans :guild-emojis
                                                                        :guild-integrations :guild-webhooks :guild-invites
                                                                        :guild-voice-states :guild-presences :guild-messages
                                                                        :guild-message-reactions :guild-message-typing
                                                                        :direct-messages :direct-message-reactions
                                                                        :direct-message-typing})
        messaging-ch (mess/start-connection! bot-token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (reset! state init-state)
    (try (events/message-pump! event-ch (partial events/dispatch-handlers #'handlers))
         (finally
           (mess/stop-connection! messaging-ch)
           (conns/disconnect-bot! connection-ch)))))
