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
                    {:question "Welche Linien fahren über Südkreuz und Gesundbrunnen?"
                     :answers ["S2, S25, S26, S41, S42"
                               "S1, S2, S25, S26"
                               "S2, S41, S42, S45, S46"
                               "S3, S5, S7, S9, S75"]}
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

(defui result [{:keys [correct? points next-question! last-question? restart!]}]
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
        ($ :button.btn.btn-outline-danger.btn-lg {:on-click #(set-screen! :question)} "Zum Quiz"))
     ($ :div.beta "BETA Version 0.0.4 | Was ist neu? Ein Nochmal-Spielen-Button, ein 30 Sekunden Timer. Viel Spass mit der neuen Version!")))

(defui app []
  (let [[answer set-answer!] (uix/use-state nil)
        [screen set-screen!] (uix/use-state :home)
        [questions set-questions!] (uix/use-state (randomize-questions))
        [points set-points!] (uix/use-state 0)]
    ($ :div.app
       (case screen
         :home ($ home {:set-screen! set-screen!})
         :result ($ result {:correct? (= answer 1)
                            :points points
                            :last-question? (= 1 (count questions))
                            :next-question! (fn []
                                              (set-screen! :question)
                                              (set-questions! (rest questions))
                                              (set-answer! nil))
                            :restart! (fn []
                                        (set-questions! (randomize-questions))
                                        (set-points! 0)
                                        (set-screen! :question)
                                        (set-answer! nil))})
         :question ($ question {:answer answer
                                :set-answer! set-answer!
                                :on-submit (fn []
                                             (when (= answer 1)
                                               (set-points! (+ 5 points)))
                                             (set-screen! :result))
                                :question (first questions)})))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))
