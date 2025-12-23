(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- generate-chest-coins []
  "Generate random amount of coins for a chest (5 to 30)"
  (+ 5 (rand-int 26)))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\r\nExits: " (keys @(:exits @player/*current-room*)) "\r\n"
       (str/join "\r\n" (map #(str "There is " % " here.\r\n")
                           @(:items @player/*current-room*)))))

(defn move
  "\"We gotta get out of this place...\" Give a direction."
  [direction]
  (dosync
   (let [dir-key (if (string? direction) 
                   (keyword direction) 
                   direction)
         current-room @player/*current-room*
         exits @(:exits current-room)
         target-name (exits dir-key)]
     
     (if (and target-name (not (nil? target-name)))
       (let [target (@rooms/rooms target-name)]
         (if target
           (do
             (move-between-refs player/*name*
                                (:inhabitants current-room)
                                (:inhabitants target))
             (ref-set player/*current-room* target)
             (look))
           "You can't go that way."))
       "You can't go that way."))))


(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\r\n"
       (str/join "\r\n" (seq @player/*inventory*))
       "\r\n\nCoins: " (player/get-coins))) 



(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (let [current-room @player/*current-room*
          item-key (keyword item)]
      (cond
        (= item "chest")
        (if (true? (:has-chest current-room))
          (str "There is a chest in this room!")
          "There is NO chest here.")

        :else
        (if ((:items current-room) item-key)
          (str item " is in this room.")
          (str item " is not in this room."))))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)
        formatted-message (str player/*name* ": " message)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (print formatted-message)
        (print "\r\n")
        (print player/prompt)
        (flush)))
    (str "You said: " message)))


(defn coins
  "Check your coin balance."
  []
  (str "You have " (player/get-coins) " coins."))

(defn open-chest
  "Open the chest in the room with a key."
  []
  (dosync
   (if @(:has-chest @player/*current-room*)
     (if (player/carrying? "keys")
       (let [coins-found (generate-chest-coins)]
         (alter player/*inventory* disj :keys)
         (ref-set (:has-chest @player/*current-room*) false)
         (player/add-coins coins-found)
         (str "You opened the chest with a keys and found " 
              coins-found " coins! The chest and key disappeared."))
       "You need a keys to open the chest.")
     "There is no chest here.")))

(defn sell
  "Sell an item for coins"
  [item]
  (dosync
   (if (player/carrying? item)
     (let [price (cond
                   (= item "bunny") 15
                   (= item "turtle") 20
                   :else 0)]
       (if (> price 0)
         (do
           (alter player/*inventory* disj (keyword item))
           (player/add-coins price)
           (str "Sold " item " for " price " coins. Total: " (player/get-coins)))
         "You can't sell this item."))
     (str "You're not carrying a " item "."))))

(defn add-experience [amount]
  "Add expirience"
  (dosync
   (alter player/*experience* + amount)
   (while (>= @player/*experience* (* 10 @player/*level*))
     (alter player/*experience* - (* 10 @player/*level*))
     (alter player/*level* inc)
     (str "Congratulations! You reached level " @player/*level* "!"))))

(defn buy
  "Buy experience potion for 20 coins"
  []
  (dosync
   (let [current-coins (player/get-coins)]
     (if (>= current-coins 20)
       (do
         (player/add-coins -20)
         (add-experience 10)
         (str "Bought experience potion! +10 XP. Coins left: " (player/get-coins)))
       (str "Not enough coins! Need 20, have " current-coins)))))

(defn shop
  "Show shop prices"
  []
  (str "SHOP:\r\n"
       "Sell prices:\r\n"
       "  bunny   - 15 coins\r\n"
       "  turtle  - 20 coins\r\n"
       "\r\nBuy:\r\n"
       "  XP Potion - 20 coins (+10 experience)\r\n"
       "\r\nYour coins: " (player/get-coins)))

(defn stats
  "Show your character stats"
  []
  (str "Level: " @player/*level* "\r\n"
       "Experience: " @player/*experience* "/" (* 10 @player/*level*) "\r\n"
       "Coins: " (player/get-coins)))
     

(defn spawn-item
  "Add an item to the current room if it belongs here (for bot use)."
  [item]
  (dosync
   (let [room-name (:name @player/*current-room*)
         item-key (keyword item)]
     (cond
       (= room-name :start)
       (str "No items in start room.")
       
       (and (= room-name :hallway) (= item "detector"))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the hallway."))
         (str item " is already in the hallway."))
       
       (and (= room-name :promenade) 
            (or (= item "bunny") (= item "turtle")))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the promenade."))
         (str item " is already in the promenade."))
       
       (and (= room-name :closet) (= item "keys"))
       (if (not (rooms/room-contains? @player/*current-room* item))
         (do (alter (:items @player/*current-room*) conj item-key)
             (str "Spawned " item " in the closet."))
         (str item " is already in the closet."))
       
       :else
       (str item " doesn't belong in this room.")))))


(defn help
  "Show available commands and what they do."
  []
  (str/join "\r\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))



(def commands (atom {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help
               "coins" coins
               "open-chest" open-chest
               "stats" stats
               "sell" sell
               "buy" buy
               "shop" shop
               "spawn-item" spawn-item}))


;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try 
    (let [[command & args] (.split input " +")
          cmd-fn (get @commands (.toLowerCase command))]
      (if cmd-fn
        (apply cmd-fn args)
        (str "Unknown command. Type 'help' for commands.")))
    (catch Exception e
      "You can't do that!")))
