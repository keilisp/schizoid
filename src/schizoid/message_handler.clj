(ns schizoid.message-handler
  (:require [schizoid.reply-generator :as reply]
            [schizoid.data-learner :as dlearner]
            [schizoid.chance-repo :as chance]
            [schizoid.message :as message]
            [discljord.messaging   :as mess]))

;;;; FIXME 

(defn process-message
  "Handler for processing message and replying if possible."
  [conn channel-id message]
  (let [should-answer? (message/should-answer? message)]
    (when should-answer? (mess/trigger-typing-indicator! conn channel-id))
    (dlearner/learn message)
    (when should-answer?
      (when-let [reply-text (reply/generate message)]
        (mess/create-message! conn channel-id :content reply-text)))))

(defn process-sticker
  "Handle for processing stickers,"
  [conn channel-id message]
  (when (or (message/has-anchors? message)
            (message/is-reply-to-bot? message))
    (mess/create-message! conn channel-id :content "sticker")))

(defn handle
  "General handler. TODO: rewrite as discljord handler."
  [conn message]
  (let [channel-id (:channel-id message)
        chance (chance/get-chance channel-id)]
    (if (and (message/has-text? message)
             (message/was-edited? message))
      (process-message conn channel-id message)
      (when (message/is-sticker? message)
        (process-sticker conn channel-id message)))))


