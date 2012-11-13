(ns breakout.game)

;; world settings
(def world-bounds (atom [600 350]))

;; The amount to move the paddle by
(def paddle-offset (atom [0 0]))
(def paddle-offset-multiplier (atom 40))

;; Ball settings
(def ball-speed 5)

(def block-size (atom {:width 50 :height 20}))

(defn default-ball []
  {:origin {:x 300 :y 250}
   :radius 5
   :color :yellow
   :velocity {:x ball-speed :y (- ball-speed)} })
(defn default-paddle []
  {:origin {:x 300 :y 300} :size {:width 50 :height 10} :color :green})

(defn default-blocks []
  (let [{width :width, height :height, :as size} @block-size
        colors [:yellow :green :blue :magenta :red :orange :cyan :black]]
    (for [x (range 1 11)
          y (range 1 5)]
      {:origin {:x (* x width) :y (* y height)}
       :size size
       :color (nth colors (mod (+ (* y 5) x) (count colors)))})))

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

;; Adjusts the ball when it goes out of bounds
(defn world-bounds-adjust-ball [ball]
  (let [{{x :x, y :y} :origin {vx :x, vy :y} :velocity} ball
        [world-width world-height] @world-bounds
        over-x (> x world-width)
        over-y (> y world-height)
        under-x (< x 0)
        under-y (< y 0)
        vx-sign (/ vx (Math/abs vx))
        vy-sign (/ vy (Math/abs vy))
        new-vx-sign (if over-x
                      -1
                      (if under-x
                        1
                        vx-sign))
        new-vy-sign (if over-y
                      -1
                      (if under-y
                        1
                        vy-sign))
        new-x (if over-x
                (- world-width (- x world-width))
                (if under-x
                  (Math/abs x)
                  x))
        new-y (if over-y
                (- world-height (- y world-height))
                (if under-y
                  (Math/abs y)
                  y))
        new-origin {:x new-x :y new-y}
        new-velocity {:x (* ball-speed new-vx-sign) :y (* ball-speed new-vy-sign)}]
    
    (assoc (assoc ball :origin new-origin)
      :velocity new-velocity)))

(defn pt-hash [[x y]] {:x x :y y})
(defn pt-from-hash [{x :x y :y}] [x y])

(defn add-pts [a b]
  (map + a b))

(defn add-pts-hash [a b]
  (let [a-pt (pt-from-hash a)
        b-pt (pt-from-hash b)]
    (pt-hash (add-pts a-pt b-pt))))

(defn between? [a b c]
  (or (and (<= a b) (<= b c))
      (and (>= a b) (>= b c))))

(defn apply-velocity-to-origin [obj]
  (let [{velocity :velocity, origin :origin} obj]
    (assoc obj :origin (add-pts-hash velocity origin))))

;; handles collisions of the paddle and ball
;; y = mx + b
;; 200 = -1 * 450 + b
;; 195 = -1 * 455 + b
;; b = 650
;; y = m * x + b
;; y - b = m * x
;; (y - b) / m = x

(defn rect-ball-collision? [paddle ball]
  (let [{{paddle-y :y, paddle-x :x} :origin,
         {paddle-width :width, paddle-height :height} :size} paddle
        {{ball-x :x, ball-y :y} :origin,
         {ball-vx :x, ball-vy :y} :velocity} ball
         ball-next-y (+ ball-y ball-vy)
        paddle-y-max (+ paddle-y paddle-height)
        within-y? (or
                   (between? ball-y paddle-y ball-next-y)
                   (between? ball-y paddle-y-max ball-next-y))]
    
    (if (not within-y?)
      ;; we can stop right here since the ball won't collide with something
      ;; not between its y delta
      false
      
      ;; compute the x coordinate where the ball will cross the paddle's path
      (let [collision-give-x 2 ; some padding so the ball doesn't blow through a paddle corner
            m (/ ball-vy ball-vx)
            b (- ball-y (* m ball-x))
            ;; this is the x position of the ball if it was at the y position of the paddle
            ball-future-x (/ (- paddle-y b) m)

            paddle-x-min (- paddle-x collision-give-x)
            paddle-x-max (+ paddle-x paddle-width collision-give-x)
                       
            within-x? (between? paddle-x-min ball-future-x paddle-x-max)]
        
        within-x?))))

(defn paddle-ball-collision? [paddle ball]
  (rect-ball-collision? paddle ball))

(defn rect-collision-adjust-ball [rect ball]
  (println (str "The ball collided with the rect:\nrect=" rect " ball=" ball))
  ;; when the ball collides, change the velocity to be upwards
  (let [{{x :x, y :y} :origin, {vx :x, vy :y} :velocity} ball
        {{rect-y :y} :origin {rect-width :width} :size} rect
        collide-y (if (>= vy 0) (+ rect-y rect-width) rect-y)
        anticipated-y (+ y vy)
        delta-y (- anticipated-y collide-y)
        new-vy (- vy)
        new-velocity {:x vx :y new-vy}
        new-y (+ y delta-y)
        new-origin {:x x :y new-y}]
    (assoc
      (assoc ball :velocity new-velocity)
      :origin
      new-origin)))

(defn paddle-collision-adjust-ball [paddle ball]
  (rect-collision-adjust-ball paddle ball))

(defn update-blocks [blocks ball]
  (filter #(not (rect-ball-collision? % ball)) blocks))

(defn blocks-ball-collision? [blocks ball]
  (let [block-collided (first (filter #(rect-ball-collision? % ball) blocks))]
    (if (nil? block-collided)
      nil
      (rect-collision-adjust-ball block-collided ball))))

;; Updates the ball velocity and location
(defn update-ball [ball paddle blocks]
  (cond
   ;; adjust the ball if it has collided with the world bounds
   (world-bounds-collision? ball)
   (world-bounds-adjust-ball ball)

   (paddle-ball-collision? paddle ball)
   (paddle-collision-adjust-ball paddle ball)

   :default
   (let [updated-ball (blocks-ball-collision? blocks ball)]
     (if (nil? updated-ball)
       (apply-velocity-to-origin ball)
       updated-ball))))