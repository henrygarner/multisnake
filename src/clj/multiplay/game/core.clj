(ns multiplay.game.core
  (:require [multiplay.game.params :refer [game-dimension]]
            [clojure.set :refer [difference]]))

(def number-of-apples 5)

(def initial-game-state
   {:apples (set (for [x [0 (dec game-dimension)]
                       y [0 (dec game-dimension)]
                       z [0 (dec game-dimension)]]
                   [x y z]))
    :snakes []})

(def dirs {:right [1 0]
           :left [-1 0]
           :up [0 -1]
           :down [0 1]})

(defn random-color
  ([]
     (->> #(+ 127 (rand-int 127))
          (repeatedly 3)
          vec))
  ([forbidden]
     (let [colors (repeatedly #(random-color))
           forbidden (set forbidden)]
       (->> colors distinct (remove forbidden) first))))

(defn neib-cell [cell dir]
  (let [[new-x new-y new-z] (map + cell dir)]
    [(mod (+ new-x game-dimension) game-dimension)
     (mod (+ new-y game-dimension) game-dimension)
     (mod (+ new-z game-dimension) game-dimension)]))

(defn rand-cells [occupied]
  (->> #(vector (rand-int game-dimension)
                (rand-int game-dimension)
                (rand-int game-dimension))
       repeatedly
       (remove occupied)
       distinct))

(defn collapse [xs d]
  (case d
    0 [(get xs 1) (get xs 2)]
    1 [(get xs 0) (get xs 2)]
    2 [(get xs 0) (get xs 1)]))

(defn collapsed [xs d]
  (into {} (map (fn [x] [(collapse x d) x]) xs)))

(defn heads-2d [snakes d]
  (map #(-> (:body %) first (collapse d)) snakes))

(def orientation->collapsed-dir
  {0 2
   1 0
   2 1})

(defn move-snake [{:keys [body dir orientation] :as snake} apples]
  (let [new-head (neib-cell (first body) dir)
        collapse-dir (get orientation->collapsed-dir orientation)
        new-body (if ((-> (collapsed apples collapse-dir) keys set)
                      (collapse new-head collapse-dir))
                   (cons new-head body)
                   (cons new-head (butlast body)))]
    (assoc snake :body new-body)))

(defn eat-apples [{:keys [body orientation]} apples]
  (let [collapse-dir (get orientation->collapsed-dir orientation)
        head-2d (-> body first (collapse collapse-dir))
        apples-2d (collapsed apples collapse-dir)]
    (set (vals (dissoc apples-2d head-2d)))))

(defn update-apples [snakes apples]
  (let [apples (reduce #(eat-apples %2 %1) apples snakes)
        to-add (- number-of-apples (count apples))]
    (if (pos? to-add)
      (let [occupied (set (concat apples (mapcat :body snakes)))]
        (->> (rand-cells occupied)
         (take to-add)
         (into apples)))
      apples)))

(defn update-world [world]
  (let [{:keys [snakes apples]} world
        new-snakes (map #(move-snake % apples) snakes)
        occupied (frequencies (mapcat :body new-snakes))
        dead? (fn [snake] (> (occupied (first (:body snake))) 1))
        alive (remove dead? new-snakes)]
    {:snakes (vec alive)
     :apples (update-apples alive apples)}))

(defn add-snake [world id]
  (let [{:keys [apples walls snakes]} world
        occupied (set (concat apples walls (mapcat :body snakes)))
        snake-head (first (rand-cells occupied))
        colors (map :color snakes)
        snake {:body [snake-head]
               :dir [0 1 0]
               :id id
               :rotation [0 0 0]
               :orientation 0
               :color (random-color colors)}]
    (update-in world [:snakes] conj snake)))

(defn remove-snake [world id]
  (letfn [(remove-by-id [snakes]
            (remove #(= id (:id %)) snakes))]
   (update-in world [:snakes] remove-by-id)))

(defn update-player [player-to-update dir {:keys [snakes]}]
  (map (fn [{:keys [id] :as snake}]
         (if (= id player-to-update)
           (assoc snake :dir dir)
           snake)) snakes))

(defmulti handle-rotation
  (fn [rotation command] command))

(defmethod handle-rotation :up [rotation command]
  (case (map #(mod % 360) rotation)
    [0 0 0]   [2 + "ZYX"]
    [0 0 90]  [2 + "ZYX"]
    [0 0 180] [2 + "ZYX"]
    [0 0 270] [2 + "ZYX"]

    [0 90 0]  [2 + "YZX"]
    [0 + "XYZ"]))

(defmethod handle-rotation :right [rotation command]
  (case (map #(mod % 360) rotation)
    [0 0 0]   [1 + "ZYX"]
    [0 90 0]  [1 + "ZYX"]
    [0 180 0] [1 + "ZYX"]
    [0 270 0] [1 + "ZYX"]
    [2 + "XYZ"]))

(defn flip [[i f e]]
  (if (= f +)
    [i - e]
    [i + e]))

(defmethod handle-rotation :down [rotation command]
  (flip (handle-rotation rotation :up)))

(defmethod handle-rotation :left [rotation command]
  (flip (handle-rotation rotation :right)))

(defn update-rotation [{:keys [snakes]} player-to-update direction]
  (map (fn [{:keys [id] :as snake}]
         (if (= id player-to-update)
           (let [[i f e] (handle-rotation (:rotation snake) direction)]
             (-> (update-in snake [:rotation] update-in [i] f 90)
                 (assoc :euler-order e)))
           snake)) snakes))

(defn update-orientation [{:keys [snakes]} player-id orientation]
  (map (fn [{:keys [id] :as snake}]
         (if (= id player-id)
           (assoc snake :orientation orientation)
           snake)) snakes))

(defmulti handle-command
  (fn [game-state command]
    (prn "handle-command" command game-state)
    (first command)))

(defmethod handle-command :default
  [game-state [command id]]
  game-state)

(defmethod handle-command :player/leave
  [game-state [command id]]

  (assoc game-state :snakes
         (vec (remove #(= (:id %) id) (:snakes game-state)))))

(defmethod handle-command :player/join
  [game-state [command id name]]
  (add-snake game-state id))

(def headings
  {[:up 0] [0 1 0]
   [:up 1] [0 0 -1]
   [:up 2] [1 0 0]
   [:left 0] [-1 0 0]
   [:left 1] [0 1 0]
   [:left 2] [0 0 1]
   [:right 0] [1 0 0]
   [:right 1] [0 -1 0]
   [:right 2] [0 0 -1]
   [:down 0] [0 -1 0]
   [:down 1] [0 0 1]
   [:down 2] [-1 0 0]})

(defn update-heading [player-id direction {:keys [snakes]}]
  (map (fn [{:keys [id orientation] :as snake}]
         (if (= id player-id)
           (let [heading (get headings [direction orientation])]
             (assoc snake :dir heading))
           snake)) snakes))

(defmethod handle-command :player/up
  [game-state [command id]]
  (assoc game-state :snakes (update-heading id :up game-state)))

(defmethod handle-command :player/down
  [game-state [command id]]
  (assoc game-state :snakes (update-heading id :down game-state)))

(defmethod handle-command :player/left
  [game-state [command id]]
  (assoc game-state :snakes (update-heading id :left game-state)))

(defmethod handle-command :player/right
  [game-state [command id]]
  (assoc game-state :snakes (update-heading id :right game-state)))

(defmethod handle-command :player/orbit-up
  [game-state [command id]]
  #_(assoc game-state :snakes (update-rotation game-state id :up))
  game-state)

(defmethod handle-command :player/orbit-down
  [game-state [command id]]
  (assoc game-state :snakes (update-orientation game-state id 1)))

(defmethod handle-command :player/orbit-left
  [game-state [command id]]
  (assoc game-state :snakes (update-orientation game-state id 0)))

(defmethod handle-command :player/orbit-right
  [game-state [command id]]
  (assoc game-state :snakes (update-orientation game-state id 2))) 

(defn- handle-commands
  [game-state commands]
  (reduce (fn [current-state command]
            (handle-command current-state command))
          game-state commands))

;;; TODO - Put your server side game logic here!!

(defn advance
  "Given a game-state and some inputs, advance the game-state one
  tick"
  [game-state commands]
  (let [new-game-state (-> game-state
                           (handle-commands commands))]
    (update-world new-game-state)))
