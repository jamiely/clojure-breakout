(ns breakout.core
  (:use breakout.game)
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class :main true))

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
  (let [origin (:origin ball)
        radius (:radius ball)
        diameter (* 2 radius)
        x (- (:x origin) radius)
        y (- (:y origin) radius)]
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

(defn -main [& args]
  (let [frame (JFrame. "Breakout")
        canvas (Canvas.)
        ball {:origin {:x 300 :y 250} :radius 5}
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
            ball ball
            current-time (System/currentTimeMillis)
            new-time (long (if (> (- current-time old-time) 250)
                             current-time
                             old-time))
            updated-paddle (update-paddle paddle paddle-offset)]

        (draw canvas (draw-game blocks updated-paddle ball score))

        :default
        (recur
         (+ score 1)
         new-time
         updated-paddle
         ball)))))
