(ns breakout.core
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class :main true))

(defn -main [& args]
  (let [frame (JFrame. "Breakout")
        canvas (Canvas.)]
    (doto frame
      (.setSize 600 400)
      (.show))
    (loop [score 0
           old-time (System/currentTimeMillis)]
      (Thread/sleep 10)
      (.setTitle frame (str "Breakout: " score))
      (let [current-time (System/currentTimeMillis)
            new-time (long (if (> (- current-time old-time) 250)
                             current-time
                             old-time))]
        :default
        (recur
         (+ score 1)
         new-time)))))
