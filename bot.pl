(defn spawn-item
  "Add an item to the current room if it belongs here (for bot use)."
  [item]
  (dosync
   (let [room-name (:name @player/*current-room*)
         item-key (keyword item)]
     (cond
       ;; В стартовой комнате не добавляем предметы
       (= room-name :start)
       (str "No items in start room.")
       
       ;; В hallway добавляем только detector
       (and (= room-name :hallway) (= item "detector"))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the hallway."))
         (str item " is already in the hallway."))
       
       ;; В promenade добавляем bunny и turtle
       (and (= room-name :promenade) 
            (or (= item "bunny") (= item "turtle")))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the promenade."))
         (str item " is already in the promenade."))
       
       ;; В closet добавляем только keys
       (and (= room-name :closet) (= item "keys"))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the closet."))
         (str item " is already in the closet."))
       
       :else
       (str item " doesn't belong in this room.")))))
