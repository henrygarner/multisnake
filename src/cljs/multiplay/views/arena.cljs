(ns multiplay.views.arena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async
             :refer [chan sliding-buffer alts! >! <! timeout close!]]
            [multiplay.game.params :refer [game-dimension]]
            [three]
            [tween]
))

(def voxel-edge 20)

(def window-width
  (.-innerWidth js/window))

(def window-height
  (.-innerHeight js/window))

(def offset-3d
  (- (* (/ game-dimension 2) voxel-edge) (/ voxel-edge 2)))

(defn make-camera []
  (let [camera (THREE/OrthographicCamera. (/ window-width -2) (/ window-width 2)
                                          (/ window-height 2) (/ window-height -2) 1 100000)]
    (set! (.-position camera) (clj->js {:x 1600 :y 0 :z 0}))
    (.lookAt camera (clj->js {:x 0 :y 0 :z 0}))
    camera))

(defn make-voxel [[x y z] color]
  (let [brush-materials (clj->js [(new THREE/MeshBasicMaterial (clj->js {:color color}))
                                  (new THREE/MeshBasicMaterial (clj->js {:color 0x000000 :wireframe true}))])
        brush (.createMultiMaterialObject THREE/SceneUtils (THREE/CubeGeometry. voxel-edge voxel-edge voxel-edge) brush-materials)]
    (set! (.-isBrush brush) true)
    (set! (.-position brush) (clj->js {:x x :y y :z z}))
    (set! (.-overdraw brush) true)
    brush))

(defn draw-bounds [container]
  (let [size     (* voxel-edge game-dimension)
        geometry (THREE/CubeGeometry. size size size)
        material (THREE/MeshBasicMaterial. (clj->js {:wireframe true :color 0x000000}))]
    (.add container (THREE/Mesh. geometry material))))

(defn to-3d-coords [[x y z]]
  [(- (* x voxel-edge) offset-3d) (- (* y voxel-edge) offset-3d) (- (* z voxel-edge) offset-3d)])

(defn make-context [renderer]
  (let [container (.createElement js/document "div")]
    (.setSize renderer window-width window-height)
    (.appendChild container (.-domElement renderer))
    container))

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

(defn update-rotation [container orientation]
  (let [orientations [(THREE/Vector3. 0 1 0)
                      (THREE/Vector3. 1 0 0)
                      (THREE/Vector3. 0 0 1)]
        vec (get orientations orientation)
        quat (.-quaternion container)
        start (.clone quat)
        end (.setFromAxisAngle (THREE/Quaternion.)
                               vec
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
                 (.. js/TWEEN -Easing -Quintic -Out))]    
    (.start forward)))

(defn draw-game-state [container game-state]
  (draw-bounds container)
  (draw-3d-snakes container (:snakes game-state) (:player-id game-state))
  (draw-3d-apples container (:apples game-state))
  (let [player (first (filter #(= (:id %) (:player-id game-state))
                              (:snakes game-state)))]
    (update-rotation container
                     (:orientation player))))

(defn make-arena [quaternion]
  (let [obj (THREE/Object3D.)]
    (set! (.-useQuaternion obj) true)
    (set! (.-quaternion obj) quaternion)
    obj))

(defn animate! [scene camera renderer]
  (.render renderer scene camera)
  (js/TWEEN.update)
  (js/requestAnimationFrame #(animate! scene camera renderer)))

(defn create! []
  (let [c (chan (sliding-buffer 1))
        scene (THREE/Scene.)
        camera (make-camera)
        quaternion (THREE/Quaternion.)
        renderer (THREE/CanvasRenderer.)]
    (.appendChild (.-body js/document) (make-context renderer))
    (animate! scene camera renderer)
    (go (loop [previous-arena (make-arena quaternion)]
          (let [game-state (<! c)
                new-arena (make-arena quaternion)]
            (.remove scene previous-arena)
            (draw-game-state new-arena game-state)
            (.add scene new-arena)
            (recur new-arena))))
    c))
