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

(defn greet-or-disconnect
  [event-type {{bot :bot} :author :keys [channel-id content]}]
  (if (= content "!disconnect")
    (async/put! (:connection @state) [:disconnect])
    (when-not bot
      (mess/create-message! (:messaging @state) channel-id :content "Hello, World!"))))

(defn send-emoji
  [event-type {:keys [channel-id emoji]}]
  (when (:name emoji)
    (mess/create-message! (:messaging @state) channel-id
                          :content (if (:id emoji)
                                     (str "<:" (:name emoji) ":" (:id emoji) ">")
                                     (:name emoji)))))

(def handlers
  {:message-create [#'greet-or-disconnect]
   :message-reaction-add [#'send-emoji]})

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
         (conns/disconnect-bot! connection-ch))))
