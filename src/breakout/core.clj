(ns breakout.core
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class :main true))

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

(defn draw-game [blocks paddle ball score]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/RED)
      (.fillRect 0 0 600 400)
    )))

(defn -main [& args]
  (let [frame (JFrame. "Breakout")
        canvas (Canvas.)]
    
    (doto frame
      (.setSize 600 400)
      (.add canvas)
      (.show))
    
    (doto canvas
      (.createBufferStrategy 2)
      (.show)
      (.requestFocus))

    (loop [score 0
           old-time (System/currentTimeMillis)]
      (Thread/sleep 10)
      (.setTitle frame (str "Breakout: " score))
      (let [blocks (atom nil)
            paddle (atom nil)
            ball (atom nil)
            current-time (System/currentTimeMillis)
            new-time (long (if (> (- current-time old-time) 250)
                             current-time
                             old-time))]
        
        (draw canvas (draw-game blocks paddle ball score))

        :default
        (recur
         (+ score 1)
         new-time)))))
