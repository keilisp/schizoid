(ns schizoid.message-handler
  (:require [schizoid.reply-generator :as reply]
            [schizoid.data-learner :as dlearner]
            [schizoid.chance-repo :as chance]
            [schizoid.message :as message]
            [discljord.messaging   :as mess]))

(defn process-message
  [conn chat-id message]
  (let [should-answer? (message/should-answer? message)]
    (when should-answer? (mess/trigger-typing-indicator! conn chat-id))
    (dlearner/learn message)
    (when should-answer?
      (when-let [reply-text (reply/generate message)]
        (mess/create-message! conn chat-id :content reply-text)))))

(defn process-sticker
  [conn chat-id message]
  (when (or (message/has-anchors? message)
            (message/is-reply-to-bot? message))
    (mess/create-message! conn chat-id :content "sticker")))

(defn handle
  [conn message]
  (let [chat-id (:channel-id message)
        chance (chance/get-chance chat-id)]
    (if (and (message/has-text? message)
             (message/was-edited? message))
      (process-message conn chat-id message)
      (when (message/is-sticker? message)
        (process-sticker conn chat-id message)))))


