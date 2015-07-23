(ns pongctrl.core
  (:import [javax.swing JFrame JPanel])
  (:import [java.awt Image Graphics Graphics2D Color Dimension Font])
  (:import [java.awt.image BufferedImage WritableRaster])
  (:import [java.awt.font FontRenderContext])
  (:import [java.io BufferedReader BufferedWriter
            InputStreamReader OutputStreamWriter])
  (:gen-class))


(def default-params
  {:board_width 320
   :board_height 240
   :ball_edge 4
   :net_edge 2
   :bat_height 32
   :bat_edge_offs 6
   :score_font_size 12
   :score_win 15
   :max_cpu_speed 5
   :max_ball_speed 400
   :magnification 2})


(defn- props->map [p]
  ;; Hooray for having only int params
  (into
   {}
   (map (fn [[x y]]
          [(keyword x) (Integer/parseInt y)])
        p)))


(defn load-prop-file []
  (let [p (java.util.Properties.)
        f (clojure.java.io/resource "pongctrl.properties")]
    (if (nil? f)
      'missing-properties-file ;; and die miserably
      (with-open [fr (clojure.java.io/reader f)]
        (.load p fr)
        (merge default-params (props->map p))))))

(def params (atom nil))

(defn load-params []
  (reset! params (load-prop-file)))

(def score-font-v (atom nil))

(defn score-font []
  (when (nil? @score-font-v)
    (reset! 
     score-font-v
     (Font. Font/MONOSPACED Font/BOLD (:score_font_size @params))))
  @score-font-v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paint stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-window 
  "Crate a window"
  []
  (let [jp (proxy [JPanel] []
             (updateScreen [img]
               (. (. this getGraphics) drawImage 0 0 img nil)))]
    (.setPreferredSize 
     jp
     (Dimension. (* (:board_width @params) (:magnification @params))
                 (* (:board_height @params) (:magnification @params))))
    (doto (JFrame. "Pong2!")
      (.add jp)
      .pack
      .show)
    jp))

(defn blank-image []
  (BufferedImage. 
   (:board_width @params)
   (:board_height @params)
   (BufferedImage/TYPE_BYTE_GRAY)))

(defn- plot-rect! 
  "Plots a white rectangle centered at cx, cy"
  [cx cy width height im-graph]
  ;;(println cx cy width height im-graph)
  (. im-graph setColor Color/WHITE)
  (. im-graph fillRect 
     (- cx (/ width 2))
     (- cy (/ height 2))
     width
     height))

;; base plot functions
(defn- plot-square! [cx cy edge im-graph]
  (plot-rect! cx cy edge edge im-graph))

(defn- plot-net! [cx cy im-graph]
  (plot-square! cx cy (:net_edge @params) im-graph))

(defn- plot-ball! [cx cy im-graph]
  (plot-square! cx cy (:ball_edge @params) im-graph))

(defn- plot-bat! [cx cy im-graph]
  (plot-rect! cx cy 
              (:ball_edge @params)
              (:bat_height @params) im-graph))


(defn init-base-screen! 
  "Initialize the main screen, i.e.
  the base image.
  "
  [bscr]
  (let [xM (/ (:board_width @params) 2)
        yM (/ (:board_height @params) 2)
        ig (. bscr getGraphics)]
    ;; paint black
    (. ig setColor Color/BLACK)
    (. ig fillRect 0 0 
       (:board_width @params)
       (:board_height @params))
    ;; net is a square repeated in the middle of the field
    (doseq [y (concat (range yM (:board_height @params) (* (:net_edge @params) 2))
                      (range yM 0 (* (:net_edge @params) -2)))]
      (plot-net! xM y ig))
    bscr))

(defn- copy-image 
  [img]
  (let [wr (. img copyData nil)
        cm (. img getColorModel)
        ap (. img isAlphaPremultiplied)]
    (BufferedImage. cm wr ap nil)))

(defn draw-components!
  "Use the game state and params to update
  an image with all the `dynamic components`
  (ball, bats)"
  [img gstate]
  (let [cpi (copy-image img)]
    (plot-ball! (-> gstate :ball :x) (-> gstate :ball :y) (. cpi getGraphics))
    (plot-bat! (-> gstate :bats :player :x) (-> gstate :bats :player :y) (. cpi getGraphics))
    (plot-bat! (-> gstate :bats :cpu :x) (-> gstate :bats :cpu :y) (. cpi getGraphics))
    cpi))

(defn draw-score! 
  "Draw the score"
  [img gstate]
  (let [grph (.getGraphics img)]
    (. grph setColor Color/WHITE)
    (. grph setFont @score-font-v)
    (let [fm (.getFontMetrics grph) ;;(. grph getFontRenderContext)
          s1 (str (get-in gstate [:score :player]))
          s2 (str (get-in gstate [:score :cpu]))
          r1 (.getStringBounds fm s1 grph)
          r2 (.getStringBounds fm s2 grph)
          xM (/ (:board_width @params) 2)
          yM (/ (:board_height @params) 2)]
      (. grph drawString s1
         (int (- xM (.getWidth r1) (* (:ball_edge @params) 2)))
         (int (+ (.getHeight r1) (:ball_edge @params))))
      (. grph drawString s2
         (int (+ xM (* (:ball_edge @params) 2)))
         (int (+ (.getHeight r2) (:ball_edge @params))))
      ;; return modified instance for further use
      img)))


(defn update-screen! [wnd img]
  (. (. wnd getGraphics) drawImage img 0 0 nil))



;; slow and sloppy
(defn spit-image! 
  "Spits an image to output as an array of 0s and 1s.
  First line consists of 2 numbers, the width and height
  of the image and the rest of the data follow in scan line
  order.
  This function is slow and sloppy"
  [img outstream]
  (let [imgdata 
        (. (. (. img getRaster) getDataBuffer) getData)
        sb (new StringBuilder)
        cnt (atom 1)]
    (.write outstream  (str (:board_width @params) " " (:board_height @params) "\n"))
    (doseq [idx (range (* (:board_width @params) (:board_height @params)))]
      (do 
        (.append sb (if (< (nth imgdata idx) 0) "1" "0"))
        (if (= @cnt (:board_width @params))
          (do 
            (swap! cnt (fn [x] 1))
            (.append sb "\n"))
          (swap! cnt (fn [xv] (+ xv 1))))))
    (.write outstream (.toString sb))
    (.flush outstream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state creators / modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-ball
  []
  ;; this one is for making a new ball
  {:x (/ (:board_width @params) 2)
   :y (/ (:board_height @params) 2)
   :ux -30 ;; FIXME
   :uy (* (if (< (- (rand 2) 1) 0) -1 1)
          (int (rand 30)))})


(defn make-bats []
  ;; X coordinates are actually constants
  {:player {:y (/ (:board_height @params) 2)
            :x (:bat_edge_offs @params)}
   :cpu {:y (/ (:board_height @params) 2)
         :x (- (:board_width @params)
               (:bat_edge_offs @params))}})

(defn make-score []
  {:player 0
   :cpu 0})


(defn new-game-state []
  {:ball (make-ball)
   :bats (make-bats)
   :score (make-score)})


(defn x-scores [s who]
  (assoc-in s [:score who]
            (+ (-> s :score who) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "AI"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clip [x min max]
  (cond (< x min) min
        (> x max) max
        :else x))

(defn simple-ai 
  "`A.I` tries to follow the Y coordinate of the ball.
  However, it is speed limited, so the `player` will
  eventually win.
  `hit-offs` is an additional pixel offset. This is to
  help debugging."
  [gstate dt tag hit-offs]
  (let [target-y (:y (get-in gstate [:bats tag]))
        delta (- (get-in gstate [:ball :y]) target-y)]
    (assoc-in gstate
              [:bats tag :y]
              (clip 
               (cond (< delta 0) (+ target-y 
                                    (max delta (- (:max_cpu_speed @params))))
                     (>= delta 0) (+ target-y 
                                     (min delta (:max_cpu_speed @params))))
               (/ (:bat_height @params) 2)
               (- (:board_height @params) (/ (:bat_height @params) 2))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Original Pong notes:
;; - speed increases with time
;; - center segments reflect w/ 
;;   right angle while moving 
;;   closer to edge changes this

;; 2 walls, 2 bats (1 per ux direction),
;; so brute force

;; FIXME: convert to floats? 
;; there is a function below, but currently everythin
;; is using rationals.
;;

(defn fzero [x] 
  (< (Math/abs x) 1e-6))


(defn get-pong-refl 
  "Get the reflected uy modifier according
  to the bat \"segment\" of impact.
  Currently the bat is divided in 9 segments 
  with different reflection angles."
  [bat-cy ball-cy]
  ;; DY / segment-width and conv to integer
  (let [idx (int (/ (- bat-cy ball-cy) (/ (:bat_height @params) 9)))
        idx-abs (+ idx 4)]
    ;;(println "reflection @" idx " -> " idx-abs)
    (+ (nth [20 15 10 5 
             0
             5 -10 -15 -20] idx-abs))))


(defn get-segments
  "To facilitate collision calcs (well, hopefully).
  Segments are tagged as `horizontal` or `vertical`
  to facilitate computations"
  [gstate]
  (let [bh2 (/ (-> @params :bat_height) 2)
        be2 (/ (-> @params :ball_edge) 2)]
    {:player
     {:y0 (- (-> gstate :bats :player :y) bh2)
      :y1 (+ (-> gstate :bats :player :y) bh2)
      :x0 (+ (-> gstate :bats :player :x) be2)
      :x1 (+ (-> gstate :bats :player :x) be2)
      :type :vertical}
     :cpu
     {:y0 (- (-> gstate :bats :cpu :y) bh2)
      :y1 (+ (-> gstate :bats :cpu :y) bh2)
      :x0 (- (-> gstate :bats :cpu :x) be2)
      :x1 (- (-> gstate :bats :cpu :x) be2)
      :type :vertical}
     :ceil
     {:y0 0 :y1 0
      :x0 0 :x1 (:board_width @params)
      :type :horizontal}
     :floor
     {:y0 (:board_height @params) :y1 (:board_height @params)
      :x0 0 :x1 (:board_width @params)
      :type :horizontal}}))


(defn get-arrival-time [gstate seg rtype]
  (let [u (if (= rtype :horizontal)
            (get-in gstate [:ball :uy])
            (get-in gstate [:ball :ux]))
        c1 (if (= rtype :horizontal)
             (get-in gstate [:ball :y])
             (get-in gstate [:ball :x]))
        c2 (if (= rtype :horizontal)
             (:y0 seg)
             (:x0 seg))]
    (if-not (fzero u)
      (/ (- c2 c1) u)
      Double/NEGATIVE_INFINITY)))

(defn get-arrival-times [gstate]
  (into 
   {}
   (map (fn [[x y]]
          [x (merge 
              {:dt (get-arrival-time gstate y (:type y))}
              y)])
        (get-segments gstate))))


(defn intersects?
  "Check if the point is contained in the
  segment. Trivial, since we only consider
  the vertical segments (bats).
  Further, we assume that (since we build them
  that way) y0 < y1"
  [pt seg seg-type]
  ;;(println (:y pt) (:y0 seg) (:y1 seg) seg)
  (if (= seg-type :horizontal)
    true
    (and (> (:y pt) (:y0 seg))
         (< (:y pt) (:y1 seg)))))

(defn pos-at-time
  "Get the (expected) position of the ball
  at some time in the future"
  [gstate dt]
  (let [x0 (get-in gstate [:ball :x])
        y0 (get-in gstate [:ball :y])
        ux (get-in gstate [:ball :ux])
        uy (get-in gstate [:ball :uy])]
    {:x (+ x0 (* ux dt))
     :y (+ y0 (* uy dt))}))

(defn min-dt 
  [[k1 v1] [k2 v2]]
  (if (< (:dt v1) (:dt v2))
    [k1 v1]
    [k2 v2]))

(defn update-ball-in-state [gstate x y ux uy]
  (assoc gstate 
         :ball {:x x :y y :ux ux :uy uy}))


;; this one is extremely ugly
(defn get-next-pos 
  "get the next ball position given that 
  the elapsed time between updates is `dt`.
  Checks for a single intersection and recurs."
  [gstate dt]
  (let [tt (get-arrival-times gstate)]
    (let [min-filt (filter (fn [[k v]] (or (> (:dt v) 0)
                                           (and (< (:dt v) 0)
                                                (> (:dt v) -1e-6))))
                           tt)

          min-seg (if (> (count min-filt) 0) ;; just to catch a precision error
                    (reduce min-dt min-filt)
                    []) 
          ;; now, an empty vect means that we moved out of the boundaries.
          ;; we'll handle this as adistinct case
                   
          test-ball-pos (if-not (empty? min-seg) (pos-at-time gstate (:dt (second min-seg))) nil)
          norm-ball-pos (pos-at-time gstate dt)
          reflector (first min-seg)
          reflinfo (second min-seg)]
      ;; check if the available dt is enough so as to have
      ;; a collision
      (if (empty? min-seg)
        ;; return the normal track, so that we don't crash
        (update-ball-in-state gstate 
                              (:x norm-ball-pos)
                              (:y norm-ball-pos)
                              (-> gstate :ball :ux)
                              (-> gstate :ball :uy))
        
        (if (< dt (:dt reflinfo))
          ;; no, so return the new ball position
          (do 
            ;;(println "norm")
            (update-ball-in-state gstate 
                                  (:x norm-ball-pos)
                                  (:y norm-ball-pos)
                                  (-> gstate :ball :ux)
                                  (-> gstate :ball :uy)))
          ;; yes. we first need to check if it *actually* intersects
          ;; with the segment
          (if (intersects? test-ball-pos ;; point
                           (reflector tt) ;; segment
                           (:type reflinfo)) ;;segment type
            ;; intersects, so bounce and recur
            ;; yet another if here
            (if (or (= reflector :player)
                    (= reflector :cpu))
              ;; we have a uy modifier
              (do
                ;;(println "refl bat " reflector)
                (recur (update-ball-in-state 
                        gstate 
                        (:x test-ball-pos)
                        (:y test-ball-pos)
                        (- (-> gstate :ball :ux))
                        (+ (-> gstate :ball :uy)
                           (get-pong-refl (-> gstate :bats reflector :y)
                                          (:y test-ball-pos))))
                       (- dt (:dt reflinfo))))
              ;; standard wall (invert uy)
              (do
                ;;(println "refl wall " reflector)
                (recur (update-ball-in-state 
                        gstate 
                        (:x test-ball-pos)
                        (:y test-ball-pos)
                        (-> gstate :ball :ux) 
                        (- (-> gstate :ball :uy))) ;; easy reflection
                       (- dt (:dt reflinfo)))))
            ;; does not intersect, move to the fully
            ;; (offscreen) and let the remaining logic
            ;; sort things out.
            (update-ball-in-state gstate 
                                  (:x norm-ball-pos)
                                  (:y norm-ball-pos)
                                  (-> gstate :ball :ux)
                                  (-> gstate :ball :uy))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows and stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-tick-counter []
  (atom 0))

(defn inc-tick-counter [x]
  (+ x 1))

(defn reset-ticks [x]
  0)

(defn increase-ball-speed [gstate]
  (let [bux (-> gstate :ball :ux)]
    (assoc-in gstate [:ball :ux]
              (if (< (Math/abs bux) (:max_ball_speed @params))
                (if (< bux 0) 
                  (- bux 5)
                  (+ bux 5))
                bux))))

(defn read-user-input [gstate inpstream]
  (let [new-y (Integer/parseInt (clojure.string/trim (.readLine inpstream)))
        min-y (/ (:bat_height @params) 2)
        max-y (- (:board_height @params) min-y)]
    (assoc-in
     gstate
     [:bats :player :y]
     ;; FIXME: add small offset here?
     (clip new-y
           min-y
           max-y))))


(defn check-score-and-reset-ball 
  [gstate ticks]
  (cond (< (get-in gstate [:ball :x]) 0) 
        (do (reset! ticks 0) ;; ugly last min hack
            (assoc (x-scores gstate :cpu) :ball (make-ball)))
        
        (> (get-in gstate [:ball :x]) (:board_width @params))
        (do (reset! ticks 0)
            (assoc (x-scores gstate :player) :ball (make-ball)))
        :else gstate))

(defn check-ticks-and-ball-speed
  [gstate ticks]
  (if (> @ticks 100)
    (do 
      (swap! ticks reset-ticks)
      (increase-ball-speed gstate))
    (do
      (swap! ticks inc-tick-counter)
      gstate)))


(defn run-game [gstate dt tick-cnt base-img main-wnd s-inp s-out s-err]
  ;;(println gstate)
  (let [ii (draw-score! (draw-components! base-img gstate)
                        gstate)]
    (update-screen! main-wnd ii)
    (spit-image! ii s-inp))
  (cond (>= (get-in gstate [:score :player])
            (:win_score @params))
        (do (println "Player wins") gstate) ;;(System/exit 0))

        (>= (get-in gstate [:score :cpu])
            (:win_score @params))
        (do (println "CPU wins") gstate) ;; (System/exit 0))

        :else
        ;; state after moves and score-check
        (let [s1 (-> gstate
                     (get-next-pos dt)
                     ;;(simple-ai dt :player 12) ;; test
                     (read-user-input s-out)
                     (simple-ai dt :cpu 0)
                     (check-score-and-reset-ball tick-cnt))]
          (recur (check-ticks-and-ball-speed s1 tick-cnt)
                 dt tick-cnt base-img main-wnd s-inp s-out s-err))))


;; params and font
(load-params)
(score-font)

        
(defn -main
  [& args]
  (let [subproc (.start (ProcessBuilder. args))
        w (make-window)]
    (with-open [s-out (BufferedReader. (InputStreamReader. (.getInputStream subproc)))
                s-inp (BufferedWriter. (OutputStreamWriter. (.getOutputStream subproc)))
                s-err (BufferedReader. (InputStreamReader. (.getErrorStream subproc)))]
      (run-game (new-game-state) 0.05 (make-tick-counter)
                (init-base-screen! (blank-image))
                w
                s-inp
                s-out
                s-err))))

