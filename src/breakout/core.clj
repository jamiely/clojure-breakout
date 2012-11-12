(ns breakout.core
  (:use breakout.game)
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class :main true))

;; world settings
(def world-bounds (atom [600 400]))

;; The amount to move the paddle by
(def paddle-offset (atom [0 0]))
(def paddle-offset-multiplier (atom 40))

(defn handle-input [#^KeyEvent event]
  (condp = (.getKeyCode event)
    KeyEvent/VK_LEFT (swap! paddle-offset #(map + [-1 0] %))
    KeyEvent/VK_RIGHT (swap! paddle-offset #(map + [1 0] %))))

(defn input-listener []
  (proxy [ActionListener KeyListener] []
    (actionPerformed [e])
    (keyPressed [e] (handle-input e))
    (keyReleased [e])
    (keyTyped [e])))

;; got this draw method from the clojure tetris implementation
(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g (.getDrawGraphics buffer)]
    (try
      (draw-fn g)
      
      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw-ball [ball #^Graphics g]
  (let [{{orig-x :x, orig-y :y} :origin, radius :radius} ball
        diameter (* 2 radius)
        adjust-radius #(- % radius)
        x (adjust-radius orig-x)
        y (adjust-radius orig-y)]
    (doto g
      (.setColor Color/YELLOW)
      (.fillOval x y diameter diameter))))

(defn draw-paddle [paddle #^Graphics g]
  (let [origin (:origin paddle)
        x (:x origin)
        y (:y origin)
        size (:size paddle)
        width (:width size)
        height (:height size)]
    (doto g
      (.setColor Color/GREEN)
      (.fillRect x y width height))))

(defn draw-game [blocks paddle ball score]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/RED)
      (.fillRect 0 0 600 400))
    
    (draw-paddle paddle g)
    (draw-ball ball g)))

;; Updates the location of the paddle
(defn update-paddle [paddle offset]
  (let [origin (:origin paddle)
        x (:x origin)
        y (:y origin)
        multiplier @paddle-offset-multiplier
        delta-x (* multiplier (nth @offset 0))
        delta-y (* multiplier (nth @offset 1))
        new-x (+ x delta-x)
        new-y (+ y delta-y)]
    (assoc paddle :origin {:x new-x :y new-y})))

(defn world-bounds-collision? [ball]
  (let [{{x :x, y :y} :origin} ball
        [world-width world-height] @world-bounds]
    (or (> 0 x) (> 0 y) (< world-width x) (< world-height y))))

(defn world-bounds-adjust-ball [ball]
  (let [{{x :x, y :y} :origin} ball]
    (assoc ball :origin {:x 300 :y 250})))

;; Updates the ball velocity and location
(defn update-ball [ball]
  (let [origin (:origin ball)
        x (:x origin)
        y (:y origin)
        velocity (:velocity ball)
        vx (:x velocity)
        vy (:y velocity)
        new-x (+ x vx)
        new-y (+ y vy)]
    (cond
     ;; adjust the ball if it has collided with the world bounds
     (world-bounds-collision? ball)
     (world-bounds-adjust-ball ball)
     
     :default
     (assoc ball :origin {:x new-x :y new-y}))))

(defn -main [& args]
  (let [frame (JFrame. "Breakout")
        canvas (Canvas.)
        ball {:origin {:x 300 :y 250} :radius 5 :velocity {:x 1 :y -1} }
        paddle {:origin {:x 300 :y 300} :size {:width 50 :height 10}}]
    
    (doto frame
      (.setSize 600 400)
      (.add canvas)
      (.show))
    
    (doto canvas
      (.createBufferStrategy 2)
      (.addKeyListener (input-listener))
      (.show)
      (.requestFocus))

    (loop [score 0
           old-time (System/currentTimeMillis)
           paddle paddle
           ball ball]
      (reset! paddle-offset [0 0])
      (Thread/sleep 10)
      
      (.setTitle frame (str "Breakout: " score))
      (let [blocks (atom nil)
            current-time (System/currentTimeMillis)
            new-time (long (if (> (- current-time old-time) 250)
                             current-time
                             old-time))
            updated-paddle (update-paddle paddle paddle-offset)
            updated-ball (update-ball ball)]

        (draw canvas (draw-game blocks updated-paddle updated-ball score))

        :default
        (recur
         (+ score 1)
         new-time
         updated-paddle
         updated-ball)))))
