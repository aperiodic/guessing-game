# guessing-game

Guess the number that the computer's thinking of!

From the command line:
```sh
lein run
```

From the REPL:
```clj
(require '[guessing-game.core :refer [new-game]])

(let [lo 0
      hi 100
      rounds 7]
  (new-game lo hi rounds))
```
