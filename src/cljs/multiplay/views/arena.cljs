(ns multiplay.views.arena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async
             :refer [chan sliding-buffer alts! >! <! timeout close!]]
            [multiplay.utils :refer [log]]
            [multiplay.game.params :refer [game-dimension]]
            [goog.events]
            [goog.dom]
            [three]
            [tween]))

; Canvas reference: http://www.w3schools.com/tags/ref_canvas.asp

;;; TODO - complete you client side rendering game play here!!

(def voxel-edge 20)

(def window-width
  (.-innerWidth js/window))

(def window-height
  (.-innerHeight js/window))

(def camera
  (let [camera (new THREE/OrthographicCamera (/ window-width -2) (/ window-width 2) (/ window-height 2) (/ window-height -2) 1 100000)
        pi (.-PI js/Math)]
    (set! (.-x (.-position camera))
          1600)
    (set! (.-y (.-position camera))
          0)
    (set! (.-z (.-position camera))
          0)
    (.lookAt camera (clj->js {:x 0 :y 0 :z 0}))
    camera))

(def scene
  (new THREE/Scene))

(def renderer
  (new THREE/CanvasRenderer))

(def mouse2d
  (new THREE/Vector3 0 10000 0.5))

(def projector
  (new THREE/Projector))

(def cube
  (new THREE/CubeGeometry voxel-edge voxel-edge voxel-edge))

(defn make-voxel [[x y z] color]
  (let [brush-materials (clj->js [(new THREE/MeshBasicMaterial (clj->js {:vertexColors THREE/VertexColors :opacity 1.0 :color color}))
                                  (new THREE/MeshBasicMaterial (clj->js {:color 0x000000 :wireframe true}))
                                  ])
        brush (.createMultiMaterialObject THREE/SceneUtils cube brush-materials)]
    (set! (.-isBrush brush) true)
    (set! (.-x (.-position brush)) x)
    (set! (.-y (.-position brush)) y)
    (set! (.-z (.-position brush)) z)
    (set! (.-overdraw brush) true)
    brush))

(defn animate []
  (js/requestAnimationFrame animate)
  (.render renderer scene camera)
  (.update js/TWEEN))

(defn deg->rad [deg]
  (* deg (/ Math/PI 180)))

(def offset-3d
  (- (* (/ game-dimension 2) voxel-edge) (/ voxel-edge 2)))

(defn to-3d-coords [[x y z]]
  [(- (* x voxel-edge) offset-3d) (- (* y voxel-edge) offset-3d) (- (* z voxel-edge) offset-3d)])

(defn create-context []
  (let [container (.createElement js/document "div")]
    (.setSize renderer window-width window-height)
    (.appendChild container (.-domElement renderer))
    container))

(defn draw-line [geometry x y z]
  (.push (.-vertices geometry)
         (new THREE/Vector3 x y z)))

(defn draw-grid [container scene]
  (let [extent (* (/ game-dimension 2) voxel-edge)
        material (new THREE/LineBasicMaterial
                      (clj->js {:color 0xaaaaaa}))
        geometry (new THREE/Geometry)]
    (doseq [i (range (- extent) (inc extent) voxel-edge)]
      (draw-line geometry 0 (- extent) i)
      (draw-line geometry 0 extent i)
      (draw-line geometry 0 i (- extent))
      (draw-line geometry 0 i extent)
      (draw-line geometry (- extent) 0 i)
      (draw-line geometry extent 0 i)
      (draw-line geometry i 0 (- extent))
      (draw-line geometry i 0 extent)
      (draw-line geometry (- extent) i 0)
      (draw-line geometry extent i 0)
      (draw-line geometry i (- extent) 0)
      (draw-line geometry i extent 0)
      (let [line (new THREE/Line geometry material)]
        (set! (.-type line) THREE/LinePieces)
        (.add container line)))))

(defn remove-all [scene]
  (doseq [child (.-children scene)]
    (.remove scene child)))

(defn draw-3d-walls [container walls]
  (doseq [wall walls]
    (.add container (make-voxel (to-3d-coords wall) 0x000000))))

(defn draw-3d-apples [container apples]
  (doseq [apple apples]
    (.add container (make-voxel (to-3d-coords apple) 0xFF0000))))

(defn draw-3d-snakes [container snakes player-id]
  (doseq [snake snakes
          voxel (:body snake)]
    (let [color (if (= player-id (:id snake)) 0x0000ff 0x000000)]
      (.add container (make-voxel (to-3d-coords voxel) color)))))

(def vecs
  [(THREE/Vector3. 0 1 0)
   (THREE/Vector3. 1 0 0)
   (THREE/Vector3. 0 0 1)])

(defn update-rotation [container orientation]
  (let [v (get vecs orientation)
        quat (.-quaternion container)
        start (.clone quat)
        end (.setFromAxisAngle (THREE/Quaternion.)
                               v
                               (* 90 (/ (.-PI js/Math) 180)))
        
        interpolator (clj->js {:interpolant 0})
        update #(.slerp
                 THREE/Quaternion start end quat
                 (.-interpolant interpolator))
        rot-forward (clj->js {:interpolant 1})
        forward (.easing
                 (.onUpdate (.to (TWEEN/Tween. interpolator)
                                 rot-forward 1000)
                            update)
                 (.-Out (.-Quintic (.-Easing js/TWEEN))))]    
    (.start forward)))

(defn update-3d-view [container game-state]
  ;; (draw-3d-walls (:walls game-state))
  (draw-3d-snakes container (:snakes game-state) (:player-id game-state))
  (draw-3d-apples container (:apples game-state))
  (let [player (first (filter #(= (:id %) (:player-id game-state))
                                (:snakes game-state)))]
      (update-rotation container
                       (:orientation player))))

(def arena
  (let [obj (THREE/Object3D.)]
    (set! (.-useQuaternion obj) true)
    obj))

(defn render []
  (.log js/console "Rendering")
  (.render renderer scene camera))

(defn create! []
  (let [c (chan (sliding-buffer 1))
        container (create-context)
        ;; arena (THREE/Object3D.)
        ]
    (.appendChild (.-body js/document) container)
    (.add scene arena)
    (animate)
    (go (loop [game-state (<! c)]
          (remove-all arena)
          (draw-grid arena scene)
          (update-3d-view arena game-state)
         
          (recur (<! c))))
    c))
