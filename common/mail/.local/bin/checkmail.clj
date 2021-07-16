#!/usr/bin/env bb

(ns script
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

(defn notify-email []
  (let [output (sh "notmuch" "count" "tag:unread")
        count (:out output)]
    (sh "notify-send"
        "-a" "emacsclient"
        "-i" "emacs"
        "-i" "/usr/share/icons/Adwaita/64x64/status/mail-unread-symbolic.symbolic.png"
        (format "You have %s unread emails!" count))))

(defn sync-mail []
  (sh "mbsync" "-CVa")
  (sh "notmuch" "new"))

(defn -main [& _args]
  (let [output (sh "nmcli" "networking" "connectivity")]
    (if (= "full" (str/trim (:out output)))
      (do (sync-mail)
          (notify-email))
      (println "No internet connection."))))

(-main)
