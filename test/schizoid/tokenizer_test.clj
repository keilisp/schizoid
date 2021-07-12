(ns schizoid.tokenizer-test
  (:require [clojure.test :refer :all]
            [schizoid.tokenizer :refer :all]))

(deftest prettify-word-test
  (testing "prettify-word"
    (is (nil? (prettify-word "te")))
    (is (nil? (prettify-word "t")))
    (is (nil? (prettify-word "")))
    (is (= (prettify-word "  «({[* & ;; <t!e\t#s\nt-w№o$r:^d... ]})»  ? </ \\ | | / !")
           "test-word!"))))

(deftest extract-words-test
  (testing "extract-words"
    (is (= [] (extract-words "")))
    (is (= ["test"
            "message"
            "with"
            "menitons"
            "threads"
            "emotes"
            "slash"
            "commands"
            "links"
            "emails"
            "words"
            "with"
            "garbage"
            "bla-bla-bla"]
           (extract-words "test message with menitons: <@!593186395246428161> , threads: <#839645600798343192> , emotes: <:FeelsGayMan:01010110011> , slash commands: /test /qwer , links: https://www.fsf.org/ , emails: blabla@test.com , words with garbage: bl#$a)-bl!.a-bl^a")))))

(deftest append-stop-words-test
  (testing "append-stop-words"
    (is (= [] (append-stop-words [])))
    (is (= ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great"]
           (append-stop-words ["hello" "world!" "how" "are" "you" "all" "doing?" "great"])))
    (is (= ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great." "0"]
           (append-stop-words ["hello" "world!" "how" "are" "you" "all" "doing?" "great."])))))

(deftest stop-word-to-end-test
  (testing "stop-word-to-end"
    (is (= ["0"] (stop-word-to-end [])))
    (is (= ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great" "0"]
           (stop-word-to-end ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great"])))
    (is (= ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great" "0"]
           (stop-word-to-end ["hello" "world!" "0" "how" "are" "you" "all" "doing?" "0" "great" "0"])))))

(deftest split-to-trigrams-test
  (testing "split-to-trigrams"
    (is (= [] (split-to-trigrams [])))
    (is (= [["test" "message" "with"]
            ["message" "with" "mentions"]
            ["with" "mentions" "threads"]
            ["mentions" "threads" "emotes"]
            ["threads" "emotes" "slash"]
            ["emotes" "slash" "commands"]
            ["slash" "commands" "links"]
            ["commands" "links" "emails"]
            ["links" "emails" "words"]
            ["emails" "words" "with"]
            ["words" "with" "garbage"]
            ["with" "garbage" "bla-bla-bla"]
            ["garbage" "bla-bla-bla" "0"]]

           (split-to-trigrams ["test"
                               "message"
                               "with"
                               "mentions"
                               "threads"
                               "emotes"
                               "slash"
                               "commands"
                               "links"
                               "emails"
                               "words"
                               "with"
                               "garbage"
                               "bla-bla-bla"])))

    (is (= [["test" "message" "with"]
            ["message" "with" "mentions!"]
            ["with" "mentions!" "0"]
            ["mentions!" "0" "threads"]
            ["0" "threads" "emotes"]
            ["threads" "emotes" "slash"]
            ["emotes" "slash" "commands."]
            ["slash" "commands." "0"]
            ["commands." "0" "links"]
            ["0" "links" "emails?"]
            ["links" "emails?" "0"]
            ["emails?" "0" "words"]
            ["0" "words" "with"]
            ["words" "with" "garbage"]
            ["with" "garbage" "bla-bla-bla"]
            ["garbage" "bla-bla-bla" "0"]]

           (split-to-trigrams ["test"
                               "message"
                               "with"
                               "mentions!"
                               "threads"
                               "emotes"
                               "slash"
                               "commands."
                               "links"
                               "emails?"
                               "words"
                               "with"
                               "garbage"
                               "bla-bla-bla"])))))
