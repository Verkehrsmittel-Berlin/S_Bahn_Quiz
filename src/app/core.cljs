(ns app.core
  (:require
   [uix.core :as uix :refer [$ defui]]
   [uix.dom]))

(defui option [{:keys [children answer set-answer! no]}]
  ($ :a.btn.btn-outline-success {:class (when (= no answer) "active")
                                 :on-click #(set-answer! no)}
     children))

(def question-list [{:question "Wo endet die S26?"
                     :answers ["Blankenburg und Teltow Stadt"
                               "Yorkstr. und Südkreuz"
                               "Flughafen Berlin Brandenburg und Waßmannsdorf"
                               "Gesundbrunnen und Potsdamer Platz"]}
                    {:question "Wann wurde die Br. 485 ausgemustert?"
                     :answers ["12. November 2023"
                               "1. Dezember 2023"
                               "12. Dezember 2023"
                               "1. Januar 2024"]}
                    {:question "Wie viele S-Bahn Linien gibt es in Berlin?"
                     :answers ["16"
                               "85"
                               "9"
                               "15"]}
                    {:question "Wie viele Triebfahrzeugführer*innen gibt es bei der S-Bahn Berlin? (Stand: 2023)"
                     :answers ["1.278"
                               "3.005"
                               "178"
                               "2.865"]}
                    {:question "Wie viele Viertelzüge der Baureihe 480 sind im Betrieb? (Stand 28. Juli 2024)"
                     :answers ["70"
                               "110"
                               "115"
                               "90"]}
                    {:question "Wann fuhr die erste S Bahn auf der Stadtbahn nach der Wiedervereinigung von Berlin & Deutschland?"
                     :answers ["2. Juli 1990"
                               "1. Juli 1990"
                               "3. Oktober 1990"
                               "4. Oktober 1990"]}
                    {:question "Welche S Bahn hat 2023 im Durchschnitt die meiste Verspaetung bekommen?"
                     :answers ["S25"
                               "S1"
                               "S8"
                               "S85"]}
                    {:question "Wann wurde die S-Bahn Berlin als GmbH gegründet?"
                     :answers ["Am 1. Januar 1995"
                               "Am 1. Januar 1994"
                               "Am 1. Januar 2002"
                               "Am 2. Januar 2002"]}])

(defn randomize-questions []
  (->> question-list
       (sort-by #(rand))
       vec))

(defui question [{:keys [answer set-answer! on-submit]
                  {:keys [question answers]} :question}]
  (let [[rand-answers set-rand-answers!] (uix/use-state [])
        [begin _] (uix/use-state (js/Date.))
        [timeout-pct set-timeout-pct!] (uix/use-state 0)]
    (uix/use-effect
     (fn []
       (set-rand-answers! (->> (map-indexed (fn [idx answer-text] [idx answer-text]) answers)
                               (sort-by (fn [_] (rand))))))
     [answers])
    (uix/use-effect
     (fn []
       (js/setTimeout (fn []
                        (let [now (js/Date.)
                              elapsed (- now begin)
                              pct (/ (* elapsed 100)
                                     20000)]
                          (set-timeout-pct! (if (< pct 100) pct 100))))
                      100)
       (when (= timeout-pct 100)
         (on-submit)))
     [timeout-pct])
    ($ :div
       ($ :div {:style {:background "red"
                        :height "3px"
                        :width (str timeout-pct "%")}})
       ($ :h2 question)
       ($ :div.d-grid.gap-2
          (map (fn [[idx answer-text]]
                 ($ option {:set-answer! set-answer!, :answer answer, :no (+ idx 1), :key (str "answer" idx)} answer-text))
               rand-answers))
       ($ :div.action
          ($ :button.btn.btn-primary {:on-click on-submit
                                      :disabled (not answer)} "Abschicken")))))

(defn format-date [^js/Date d]
  (str (.getDate d)
       "."
       (inc (.getMonth d))
       "."
       (.getFullYear d)
       " "
       (.getHours d)
       ":"
       (.getMinutes d)))

(defui highscore [{:keys [points profile game-id]}]
  (let [current-score {:points points
                       :name (:name profile)
                       :line (:line profile)
                       :date (-> (js/Date.)
                                 (format-date))
                       :game-id game-id}
        stored-score (-> (js/localStorage.getItem "highscore")
                         js/JSON.parse
                         (js->clj {:keywordize-keys true})
                         vec)
        game-ids (->> stored-score
                      (map :game-id)
                      set)
        _ (prn stored-score)
        score (if (contains? game-ids game-id)
                stored-score
                (->> (conj stored-score current-score)
                     (sort-by :points)
                     reverse
                     (take 5)))]
    (uix/use-effect
     (fn []
       (->> score
            clj->js
            js/JSON.stringify
            (js/localStorage.setItem "highscore")))
     [score])
    ($ :<>
       ($ :h2 "Highscore")
       ($ :table.highscore
          ($ :tr
             ($ :th "Punkte")
             ($ :th "Name")
             ($ :th "Linie")
             ($ :th "Datum"))
          (map-indexed (fn [idx {:keys [points name line date]}]
                         ($ :tr {:key idx}
                            ($ :td points)
                            ($ :td name)
                            ($ :td line)
                            ($ :td date)))
                       score)))))

(defui result [{:keys [correct? points next-question! last-question? restart! profile game-id]}]
  (let [[display-points set-display-points!] (uix/use-state (if correct?
                                                              (- points 5)
                                                              points))]
    (uix/use-effect
     (fn []
       (when correct?
         (js/setTimeout #(set-display-points! (- points 4)) 400)
         (js/setTimeout #(set-display-points! (- points 3)) 500)
         (js/setTimeout #(set-display-points! (- points 2)) 600)
         (js/setTimeout #(set-display-points! (- points 1)) 700)
         (js/setTimeout #(set-display-points! points) 800)))
     [points])

    ($ :div.result
       ($ :h1 display-points " Punkte")
       ($ :div
          (if correct?
            "Super! Deine Antwort ist richtig."
            "Deine Antwort ist leider falsch. Probiers beim nächsten mal.")
          ($ :div
             (if last-question?
               ($ :<>
                  (if (>= display-points 20)
                    "Du hast das Quiz geschafft! Du bist ein echter Profi!"
                    "Du hast das Quiz fast geschafft. Gib nicht auf!")
                  ($ highscore {:points points
                                :profile profile
                                :game-id game-id})
                  ($ :div.action
                     ($ :button.btn.btn-outline-danger.btn-lg {:on-click restart!} "Noch mal spielen")))
               ($ :div.action
                  ($ :button.btn.btn-primary {:on-click #(next-question!)} "Nächste Frage"))))))))

(defui home [{:keys [set-screen!]}]
  ($ :div
     ($ :h1 "Willkommen zum S-Bahn-Quiz!")
     ($ :div "Von Verkehrsmittel Berlin")
     "Herzlich Willkommen bei meinem Quiz. Wenn du alle Fragen richtig beantwortest, dann kennst du Berlin und seine Verkehrsmittel gut."
     ($ :div.action
        ($ :button.btn.btn-outline-danger.btn-lg {:on-click #(set-screen! :question)} "Level 1"))
     ($ :div.beta "BETA Version 0.0.7 | Was ist neu? Neue Hintergrundbilder, Weihnachts Update, Favicon, \"Login\".")))

(defui login [{:keys [set-screen! profile set-profile!]}]
  ($ :div
     ($ :h1 (str "Hallo"
                 (when (:name profile)
                   (str " " (:name profile)))
                 ", erstelle dein Profil!"))
     ($ :div
        ($ :label.form-label {:for "name"} "Name")
        ($ :input.form-control {:id "name"
                                :value (or (:name profile)
                                           "")
                                :on-change (fn [event]
                                             (let [value (-> event .-target .-value (subs 0 20))]
                                               (set-profile! (assoc profile :name value))))}))
     ($ :div
        ($ :label.form-label {:for "line"} "Lieblings S-Bahn Linie")
        ($ :select.form-select {:id "line"
                                :on-change (fn [event]
                                             (let [value (-> event .-target .-value)]
                                               (set-profile! (assoc profile :line value))))}
           (mapv (fn [line]
                   ($ :option {:key line
                               :selected (= line (:line profile))} line))
                 ["Keine Linie" "S1" "S2" "S25" "S26" "S3" "S5" "S7" "S75" "S8" "S85" "S9" "S41" "S42" "S45" "S46" "S47"])))
     ($ :div.hint "Deine Daten werden im Browser gespeichert. Nur du kannst sie sehen!")
     ($ :div.action
        ($ :button.btn.btn-outline-danger.btn-lg
           {:on-click (fn []
                        (when (:name profile)
                          (js/localStorage.setItem "name" (:name profile)))
                        (js/localStorage.setItem "line" (:line profile))
                        (set-screen! :home))}
           "Starten"))))

(defui backgrounds []
  (let [[opacity set-opacity!] (uix/use-state 0)
        [images set-images!] (uix/use-state ["WeihnachtenS.png" "bg_erkner.webp" "Br485Bild1.jpg" "bg_hh.webp" "bg_tds.webp"])
        transition 1000
        frames 24.0
        wait-time 10000]
    (uix/use-effect
     (fn []
       (dotimes [n 24]
         (js/setTimeout (fn []
                          (set-opacity! (/ n frames)))
                        (+ wait-time (* n (/ transition frames)))))
       (js/setTimeout
        (fn []
          (set-opacity! 0)
          (set-images! (-> (rest images)
                           (vec)
                           (conj (first images)))))
        (+ wait-time transition)))
     [images])
    ($ :<>
       ($ :div.background {:style {:background-image (str "url(" (first images) ")")}})
       ($ :div.background {:style {:background-image (str "url(" (second images) ")")
                                   :opacity opacity}}))))
(defui app []
  (let [[answer set-answer!] (uix/use-state nil)
        [screen set-screen!] (uix/use-state :login)
        [questions set-questions!] (uix/use-state (randomize-questions))
        [points set-points!] (uix/use-state 0)
        [profile set-profile!] (uix/use-state {:name (js/localStorage.getItem "name")
                                               :line (js/localStorage.getItem "line")})
        [game-id set-game-id!] (uix/use-state (-> (random-uuid)
                                                  str))]
    ($ :<>
       ($ :div.wrap
          ($ :div.app
             (case screen
               :login ($ login {:profile profile
                                :set-profile! set-profile!
                                :set-screen! set-screen!})
               :home ($ home {:set-screen! set-screen!})
               :result ($ result {:correct? (= answer 1)
                                  :points points
                                  :profile profile
                                  :last-question? (= 1 (count questions))
                                  :game-id game-id
                                  :next-question! (fn []
                                                    (set-screen! :question)
                                                    (set-questions! (rest questions))
                                                    (set-answer! nil))
                                  :restart! (fn []
                                              (set-questions! (randomize-questions))
                                              (set-points! 0)
                                              (set-screen! :question)
                                              (set-answer! nil)
                                              (set-game-id! (-> (random-uuid)
                                                                str)))})
               :question ($ question {:answer answer
                                      :set-answer! set-answer!
                                      :on-submit (fn []
                                                   (when (= answer 1)
                                                     (set-points! (+ 5 points)))
                                                   (set-screen! :result))
                                      :question (first questions)})))
          (when-not (= :login screen)
            ($ :div.login
               (str "Hallo " (:name profile))
               ($ :div "☃️❤️ " (:line profile)))))
       ($ backgrounds))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))
