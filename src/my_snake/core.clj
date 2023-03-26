(ns my-snake.core
  (:import
    (java.awt Color Dimension)
    (javax.swing JPanel JFrame Timer JOptionPane)
    (java.awt.event ActionListener KeyListener KeyEvent)))



;;;;;;;;;;;;;;;
;;  helpers  ;;
;;;;;;;;;;;;;;;

(defn vec-operate [func vec1 vec2]
  (into [] (map func vec1 vec2)))

(defn head [snake] (first (:body snake)))

;; map direction keys to vectors. This assumes a board with 0,0 at top left corner
(def direction-to-delta
  {:right [1 0]
   :left [-1 0]
   :up [0 -1]
   :down [0 1]})

;; map key to direction
(def keyevent-to-direction
  {KeyEvent/VK_LEFT :left
   KeyEvent/VK_RIGHT :right
   KeyEvent/VK_UP :up
   KeyEvent/VK_DOWN :down})

;;;;;;;;;;;;;;;;;;
;;  Initialize  ;;
;;;;;;;;;;;;;;;;;;


;; create initial snake

(defstruct cell-struct :x :y)
(defstruct snake-struct :body :direction)

(defn create-center-cell [width height]
  [(quot width 2) (quot height 2)])

(defn append-cell [{:keys [body direction]}]
  (let* [tail (last body)
         delta (direction direction-to-delta)
         new-tail (into [] (map - tail delta))]
    (struct
     snake-struct (conj body new-tail) direction)))

(defn build-snake-body [initial-snake length]
  (loop [counter 0
         snake initial-snake]
    (if (>= counter length)
      snake
      (recur (inc counter) (append-cell snake)))))


(defn create-snake [{:keys [width height]} length]
  (let [head (create-center-cell width height)
        body [head]
        initial-direction (rand-nth [:left :right :up :down])
        initial-snake (struct snake-struct body initial-direction)]
    (build-snake-body initial-snake length)))

;; generate fruit
(defn generate-random-cell [upper-lim]
  (into [] (take 2 (repeatedly #(rand-int upper-lim)))))

(defn generate-fruit [n-fruit size]
  (into [] (take n-fruit (repeatedly #(generate-random-cell size)))))


;; Move the snake
(defn prepend-new-head [snake]
  (let* [body (:body snake)
         head (first body)
         delta ((:direction snake) direction-to-delta)
         new-head (vec-operate + head delta)]
    (apply conj [new-head] body)))

(defn eat-fruit?
  "True if snake head is on fruit, else nil"
  [snake fruit]
  (let [head (first (:body snake))]
    (some #{[71 61]} fruit)))

(defn valid-direction? [snake new-dir] ;; TODO refactor 'delta' as 'dir-vector' (i.e. direction vector)
  "True if new-dir is not opposite to current-dir, else nil"
  (let [current-dir (:direction snake)
        [current-delta new-delta] (map direction-to-delta [current-dir new-dir])
        sum-deltas (vec-operate + current-delta new-delta)] ;;
    (some #(not= 0 %) sum-deltas))) ;;

;; (valid-direction? snake :right)

(defn change-position [fruit snake]
  (let* [new-body (prepend-new-head snake)
         new-snake (assoc snake :body new-body)]
    (if (eat-fruit? new-snake fruit)
      new-snake
      (assoc new-snake :body (butlast new-body)))))

;; (change-position (move-snake snake :left fruit) :left fruit)


(defn change-direction
  "Check if direction is valid and return snake with new direction"
  [snake direction]
  (if (valid-direction? snake direction)
    (assoc snake :direction direction)
    snake))

(defn move-snake
  "Get key event move the snake, check move outcome and proceed accordingly"
  [board fruit new-direction snake]
  (->> (change-direction snake new-direction) ;; check if direction is valid and assoc snake with direction
       (change-position fruit))) ;; move snake according to direction and whether fruit has been eaten or not
       ;; (finalize-move board enough-fruit)))


(defn hit-a-wall?
  "True if snake head is out of the board bounds, else false"
  [snake {:keys [width height]}]
  (let [[x y] (head snake)]
    (or (> x width)
        (> y height)
        (< x 0)
        (< y 0))))

;; (hit-a-wall? snake {:width 40 :height 30}) => true

(defn hit-self? [snake]
  (let [body-no-head (rest (:body snake))
        head (head snake)]
     (some #{head} body-no-head)))


(defn game-over? [snake board]
  (or (hit-a-wall? snake board)
      (hit-self? snake)))


(defn belly-full?
  "True if snake has eaten enough fruit to move to next stage, else false"
 [snake enough-fruit]
 (let [snake-len (count (:body snake))]
   (= snake-len enough-fruit)))

;; (belly-full? snake 10) => false

(defn next-stage
  "Change some of the game parameters and start the next stage"
  [stage board]
  (printf "Going to next stage %s!" (+ 1 stage))
  (create-snake board 3))


(defn game-over-new-game
  "Print a message and start a new game"
  [board]
  (print "You loose this time!")
  (create-snake board 3))


(defn get-direction ;; TODO
  "Get keyevent and return direction"
  [key-event]
  :up)


(defn move-outcome ;; maybe `after-move` is a better name?
  "Check the outcome of snake move, and act accordingly"
  [board enough-fruit snake]
  (let [game-over (game-over? snake board)
        belly-full (belly-full? snake enough-fruit)]
    (cond
      (game-over (game-over-new-game board))
      (belly-full (next-stage 1 board))
      :else snake)))


(defn game [] nil)
;; (game-move board snake 1 10 fruit)
;; before: {:body [[50 50] [49 50] [48 50]], :direction :right}
;; after:  {:body ([50 49] [50 50] [49 50]), :direction :up}



;; Sandbox
;;;;;;;;;;
;; example snake for testing
(def board {:width 100 :height 100})
(def snake (create-snake {:width 100 :height 100} 2))#'my-snake.core/snake
(def fruit (generate-fruit 10 100))

;; board {:width 100, :height 100}
;; snake{:body [[50 50] [50 51] [50 52]], :direction :up}
