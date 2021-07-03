(ns schizoid.core
  (:require [schizoid.tokenizer :as token])
  (:require [schizoid.reply-generator :as reply])
  (:require [schizoid.data-learner :as dlearner])
  (:require [schizoid.chance-repo :as chance])
  (:require [schizoid.trigram-repo :as trig])
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn])
  (:require [clojure.core.async    :as async])
  (:require [discljord.connections :as conns])
  (:require [discljord.messaging   :as mess])
  (:gen-class))

(def bot-token (-> "secrets.edn" slurp edn/read-string :bot-token))

(defn -main
  [& args]
  (let [event-ch      (async/chan 100)
        connection-ch (conns/connect-bot! bot-token event-ch :intents #{:guilds :guild-members :guild-bans :guild-emojis
                                                                        :guild-integrations :guild-webhooks :guild-invites
                                                                        :guild-voice-states :guild-presences :guild-messages
                                                                        :guild-message-reactions :guild-message-typing
                                                                        :direct-messages :direct-message-reactions
                                                                        :direct-message-typing})
        message-ch    (mess/start-connection! bot-token)]
    (try
      (loop []
        (let [[event-type event-data] (async/<!! event-ch)]
          (println "🎉 NEW EVENT! 🎉")
          (println "Event type:" event-type)
          (println "Event data:" (pr-str event-data))
          (recur)))
      (catch Exception e (str "Caught Exception: " (.getMessage e)))
      (finally
        (mess/stop-connection! message-ch)
        (conns/disconnect-bot!  connection-ch)
        (async/close!           event-ch)))))

