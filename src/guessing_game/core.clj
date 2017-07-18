(ns guessing-game.core
  (:gen-class)
  (:refer-clojure :exclude [rand-int])
  (:require [clojure.string :as str]
            [guessing-game.prize :refer [prize]]))

(defn complain-and-die!
  []
  (println "Failure to comply with instructions will be punished by (my) termination")
  (Thread/sleep 1500)
  (println "I hope you're happy")
  (Thread/sleep 900)
  (println "Goodbye")
  (Thread/sleep 900)
  (System/exit 1))

(defn get-int
  [msg]
  (print msg)
  (flush)
  (try (-> (read-line)
         Long/parseLong)
    (catch NumberFormatException _
      (complain-and-die!))))

(defn get-guess
  [lower-bound upper-bound]
  (let [msg (format "What's your guess? (%d-%d): " lower-bound upper-bound)]
    (get-int msg)))

(defn get-decision
  [description]
  (print (str description " [Y/n]: "))
  (flush)
  (let [result (str/lower-case (read-line))]
    (case result
      ("y" "") true
      "n" false
      (complain-and-die!))))

(defn- rand-int
  [lo hi]
  (+ lo (clojure.core/rand-int (- hi lo))))

(defn new-target
  [lo hi]
  (rand-int lo hi))

(defn round-result
  "Takes the target number for the game, and the guess number for the round, and
  returns the result of the round:
    :high - the user's guess was higher than the target
    :low - the user's guess was lower than the target
    :equal - the user's guess is equal to the target"
  [target guess]
  (cond
    (= target guess) :equal
    (> target guess) :low
    (< target guess) :high))

(defn report-round!
  [result rounds-remaining]
  (case result
    :high (println "Your guess was too high. You have" rounds-remaining "guesses remaining")
    :low (println "Your guess was too low. You have" rounds-remaining "guesses remaining")))

(declare new-game)

(defn end-game!
  "Based on the value of result, either congratulate or ridicule the user, and
  then exit."
  [lower-bound upper-bound rounds target result]
  (case result
    :failure (do
               (println "Pitiful human, the number in my mind was" (str target "!"))
               (println "Better luck next time, fool!"))
    :success (do
               (println "You guessed my number! It would have stayed secret if it weren't for you meddling humans!")
               (Thread/sleep 1500)
               (println prize)))
  (if (get-decision "Would you like to play again?")
    (new-game lower-bound upper-bound rounds)
    (println "That was fun. See you next time!")))

(defn do-round!
  "Run a round of the game. If the user wins, tell the user and exit. If the
  user lost, tell the user and exit. If they have more guesses, recurse to the
  next round."
  [lower-bound upper-bound rounds target guess total-guesses]
  (let [result (round-result target guess)
        won? (= result :equal)
        end! (partial end-game! lower-bound upper-bound rounds target)
        remaining (- rounds total-guesses)]
    (case result
      :equal (end! :success)
      :high (report-round! :high remaining)
      :low (report-round! :low remaining))
    (when-not won?
      (if (pos? remaining)
        (do-round! lower-bound, upper-bound, rounds, target
                   (get-guess lower-bound upper-bound)
                   (inc total-guesses))
        (end! :failure)))))

(defn new-game
  [lower-bound upper-bound rounds]
  (let [target (rand-int lower-bound upper-bound)]
    (do-round! lower-bound, upper-bound, rounds, target
               (get-guess lower-bound upper-bound)
               1)))

(defn print-tutorial!
  [lower-bound upper-bound rounds]
  (println "This is a guessing game.")
  (println "I will think of a number between" lower-bound "and" 
          (str upper-bound ", and then you have") 
          rounds "chances to guess the number.")
  (println "After each guess, I'll tell you whether your guess was high or low."))

(defn log 
  [x] 
  (Math/log x))

(defn log_2
  [x]
  (/ (log x) (log 2)))

(defn min-chances
  [upper-bound lower-bound]
   (let [dist (- upper-bound lower-bound)]
   (int (log_2 dist))))

(defn get-level
  []
  (get-int "What level of difficulty would you like? (1-3): "))

(defn level->difficulty
  [input]
  (cond
    (= input 1) :easy
    (= input 2) :moderate
    (= input 3) :hard
    :else complain-and-die!))

(defn get-rounds
  [hi lo]
  (let [level (level->difficulty (get-level))]
  (case level
    :easy (inc (min-chances hi lo))
    :moderate  (min-chances hi lo)
    :hard (dec (min-chances hi lo)))))

(defn -main
  [& args]
  (let [lo 0
        hi 100
        chances (get-rounds hi lo)]

    (print-tutorial! lo hi chances)
    (new-game lo hi chances)
    (System/exit 0)))
