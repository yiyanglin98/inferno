(ns inferno.core)
(require '[clojure.string :as str])

; basic helper functions
(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
  (let [output (str/split (str/replace (str/lower-case input) #"[?.!]" "") #" +")]
  (map keyword output))
  )

  (defn post-increment
  "Action: post-increment
   Returns the value of a variable from the state and increments it."
  [state var]
  (if-let [val (get-in state [:vars var])]
    [(update-in state [:vars var] inc) val]
    [(assoc-in state [:vars var] 1) 0]))

  (defn lookup-var
  "Given a state and a variable name, return the value of the variable
  if it has been defined, otherwise return 0."
  [state var]
  (let [re
    (cond (integer? var) var
          (keyword? var) (get-in state [:vars var])
          :else 0
      )]
      (vector state (if (= nil re) 0 re))
      ))

  (defn set-plus
  "Action: set-plus.  Set var = e1 + e2, return the sum as a result."
  [state var e1 e2]
  (let [val1 (lookup-var state e1)
  val2 (lookup-var state e2)
  r (+ (val1 1) (val2 1))]
  (vector (assoc-in state [:vars var] r) r))
  )

  (defn set-var
  "Action: set-var. Set var = e1.  Return the new value as a result."
  [state var e1]
  (set-plus state var e1 0)
  )

  (defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

; Initial items

(def init-items
 {:rusty-sword {:desc "A rusty sword. Merely better than nothing. Could be enhanced by furnance, probably."
            :name "rusty sword" }
  :furnance {:desc "Furnance. Use command (sharpen) to react with your rusty sword, if you have one."
            :name "furnance" }
  :sword {:desc "A sword. Seems pretty sharp."
            :name "sword" }
  :book {:desc "Wrote something on it. You can use command (read) to read it"
         :name "book"}
  :fast-food {:desc "Brought for $10. Eat it will increase 5 hp."
             :name "fast food"}
  :key {:desc "Key to the heaven. Just use in the right room to win the game!"
             :name "key"}
  })

; Initial map
(def init-map
  {
    :vestibule {:desc " getting into the gate of inferno. It's dark. You can only go north now. "
           :title "in the vestibule"
           :dir {:north :limbo}
           :contents #{}}

   :limbo {:desc " in an empty room called limbo. There is a rusty-sword on the ground! You can take it."
              :title "in the room of limbo"
              :dir {:south :vestibule
                    :west :lust
                    :north :gluttony
                    :east :greed}
              :contents #{:rusty-sword}}

    :lust {:desc " in a room with weird tempting singing. You can see a protable furnance nearby. You hear monster's howling in the north."
           :title "in the room of lust"
           :dir {:east :limbo
                 :north :wrath}
           :contents #{:furnance}}

    :wrath {:desc "in a room of danger! Wrath! You can either run away or attack the demon.
    Type attack to attack, or just go back to the south.
    You should get ready before attacking the demon."
              :title "in the room of wrath"
              :dir {:south :lust}
              :contents #{:demon}}

   :gluttony {:desc "in a room full of food called gluttony. There's a fast food seller. You can buy fast food using command buy to recover hp."
              :title "in the room of gluttony"
              :dir {:south :limbo}
              :contents #{:fast-food}}

    :greed {:desc "in a slimy room called greed. It's called the room of greed, for some REASON.
    Type attack to attack, or just go west / go east."
           :title "in the room of greed"
           :dir {:west :limbo
                 :east :fraud
                }
           :contents #{:slime}}

    :fraud {:desc " in a room with light ahead called fraud. The heaven is near but you need the key!
      There's a book on the floor ahead of the gate of heaven. "
           :title "in the room of fraud"
           :dir {:west :greed
                 :north :heaven}
           :contents #{:book}}

    :heaven {:desc "getting escaped from inferno! You are in the heaven now. Congrats!!
          Type anything to end the game!"
           :title "in the heaven"
           :dir {}
           :contents #{}}
   })

(def init-adventurer
  {:location :vestibule
   :inventory #{}
   :sanity 10
   :hp 10
   :money 0
   :tick 0
   :seen #{}})

(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (get-in state [:rooms location])]
    (if (contains? (get-in state [:adventurer :seen]) location)
            [state (:title the-map)]
            (do
              [(update-in state [:adventurer :seen] #(conj % location))
                            (:desc the-map)]))))

(defn been [sp]
  (do (println "You are" (sp 1))
    (sp 0)))

(defn take-food [state]
  (let [
        room (get-in state [:adventurer :location])
  ]
    (update-in (assoc-in state [:rooms room :contents] #{})
                 [:adventurer :inventory] #(conj % :fast-food))
  )

  )

(defn buy [state]
  (let [
        m (get-in state [:adventurer :money])
        room (get-in state [:adventurer :location])
        things (get-in state [:rooms room :contents])
  ]
  (cond
    (not= (get-in state [:adventurer :location]) :gluttony) (do (println "You are in the wrong place! Go to the room of gluttony
    to buy.") state)

    (< m 10) (do (println "Not enough money!")
      state
     )

    (> m 9) (do (println "You brought a burger for $10. You can [eat] it to recovered 5 hp.")
      (update-in (take-food state)
                 [:adventurer :money] - 10)
     )
  )
  )
)

(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:rooms location :dir]) dir)
        key? ((get-in state [:adventurer :inventory]) :key)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)

          (if (= (get-in state [:rooms location :dir dir]) :heaven)

              (if (nil? key?)
            (do (println "you need a key") state)

            (assoc-in state [:adventurer :location] dest)

          )
          (assoc-in state [:adventurer :location] dest)
          )
      )

      ))

(defn look
  [state sth]
  (if (nil? sth)
    (let [
          location (get-in state [:adventurer :location])]
    (do (println "You are " (get-in state [:rooms location :desc]))
    (println "Things in the room: " (get-in state [:rooms location :contents]))
    state)
    )

    (let [thing (init-items sth)]
      (if (nil? thing)
        (do (println "There's no such thing.") state)
        (do (println (get-in init-items [sth :desc])) state)
      )
    )
  )

  )

(defn ivt [state]
  (let [inv (get-in state [:adventurer :inventory])]
    (do (println (str (get-in state [:adventurer :inventory])))
    state)

  ))

(defn die? [state]
  (cond
    (< (get-in state [:adventurer :sanity]) 1) (println "You died.")
    (< (get-in state [:adventurer :hp]) 1) (println "You died.")
    :else nil
    )
  )

(defn modify [state san hp]

  (update-in (update-in state [:adventurer :sanity] - san) [:adventurer :hp] - hp))


(defn slime [state]

  (let [cur state]
  (do (println "You are attacking slime...")
    ;(def cur (assoc-in cur [:advanturer :sanity] (- (get-in cur [:adventurer :sanity]) 4)))

    (assoc-in cur [:advanturer :hp] (- (get-in cur [:adventurer :hp]) 8))
    (println "You got the $15! (if you are still alive...)")
    (println "your sanity: " (- (get-in cur [:adventurer :sanity]) 4))
    (println "your hp: " (- (get-in cur [:adventurer :hp]) 3))
    (update-in (modify cur 4 3) [:adventurer :money] + 15)
    )
)
)


(defn demon [state]

(let [cur state
      sword ((get-in state [:adventurer :inventory]) :sword)
]
  (do (println "You are attacking demon... If you don't have a better sword, you'll die.")
    ;(def cur (assoc-in cur [:advanturer :sanity] (- (get-in cur [:adventurer :sanity]) 4)))
    (if
      (nil? sword) (do (println "You should have a sword before attacking the demon! You are dying...")
        (modify cur 99 99)
      )

      (do
        (println "You got the key! (if you are still alive...)")
        (println "your sanity: " (- (get-in cur [:adventurer :sanity]) 3) )
        (println "your hp: " (- (get-in cur [:adventurer :hp]) 8))
        (update-in (modify cur 3 11) [:adventurer :inventory] #(conj % :key))
      )
    )
    )
)
  )

(defn attack [state]
  (let [location (get-in state [:adventurer :location])

  ]
  (cond (= location :wrath) (demon state)
        (= location :greed) (slime state)
        :else (do (println "Nothing to attack") state))
  ))

  (defn eat-helper [state]
    (let [sword ((get-in state [:adventurer :inventory]) :sword)]
      (if (nil? sword)
        (assoc-in state [:adventurer :inventory] #{})
        (assoc-in state [:adventurer :inventory] #{:sword})
      )

    )

    )
  (defn eat [state]
    (let [food? ((get-in state [:adventurer :inventory]) :fast-food)]
      (if
        (nil? food?) (do (println "You've got nothing to eat. Go and buy it!") state)
        (do (println "Recovered 5 hp.") (update-in (eat-helper state) [:adventurer :hp] + 5))
      )
    )
  )

  (defn take1
  "Take staff in the room. Return \"Nothing to take.\" when there's nothing to take."
  [state sth]
  (let [
    room (get-in state [:adventurer :location])
    things (get-in state [:rooms room :contents])
        ]
    (cond
      (nil? sth) (do (println "You need to say 'take sth'. ") state)
      (nil? (things sth)) (do (println "Nothing to take.") state)
      (= (things sth) :fast-food) (do (println "You need to use $10 to buy it.") state)
      :else (do (println "Picked up " sth)


                ;(assoc-in state [:adventurer :inventory] sth)
                (update-in (assoc-in state [:rooms room :contents] #{})
                 [:adventurer :inventory] #(conj % sth))
                )
      )))

  (defn drop1
  "Drop staff. Inversion of take."
  [state sth]
  (let [
    things (get-in state [:adventurer :inventory])
    room (get-in state [:adventurer :location])
    stuff (get-in state [:rooms room :contents])
        ]
        (cond
          (nil? sth) (do (println "You need to say 'drop sth'. ") state)
          (empty? things) (do (println "Nothing to drop.") state)
          (not (things sth)) (do (println "You don't have " sth) state)
          :else (do (println "Dropped " sth)
                (update-in (assoc-in state [:adventurer :inventory] #{})
                 [:rooms room :contents] #(conj % sth))
                )
      )
    )
  )

  (defn sharpen [state]
   (let [rusty ((get-in state [:adventurer :inventory]) :rusty-sword)
          furnance ((get-in state [:adventurer :inventory]) :furnance)
   ]
   (cond
    (nil? rusty) (do (println "you don't have rusty-sword to be sharpen.") state)
    (nil? furnance) (do (println "you don't have furnance to sharpen your sword.") state)
    :else
      (do (println "Your rusty-sword has been sharpened.")

      (update-in (assoc-in state [:adventurer :inventory] #{})
       [:adventurer :inventory] #(conj % :sword))
      )
   )
   )
  )

  (defn read1[state]
  (let [book ((get-in state [:adventurer :inventory]) :book)]
    (if (nil? book)
      (do (println "You don't have anything to read. ")
        state
      )
      (do (println "This game is inspired by Dante's Inferno. There are originally nine circles of hell, but I only made 6 of them.")
      (println "")
      (println "This game is also inspired by a video game called ‘Don't Starve’. It's really a nicely designed game. ")
      (println "")
      (println "I should have made this game more interesting. I have more in my mind, actually. But I am out of time.")
      (println "")
      (println "But anyway, thanks to all the professors (especially Mattox) for this wonderful semester. ")
      (println "Clojure it actually not that bad. :)")
      (println "")
        state
      )
    )
  )
  )

(defn add-tick [state]
(update-in state [:adventurer :tick] inc)
)

  (defn react
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
    (let [verb (first input-vector)
    subject (first (rest input-vector))
    add-state (add-tick state)
    ]

      (println "current tick: "(get-in add-state [:adventurer :tick]))
       (cond
        (= verb :go) (go add-state subject)


        (or (= verb :north) (= verb :n))
         (go add-state :north)
        (or (= verb :south) (= verb :s))
         (go add-state :south)
        (or (= verb :west) (= verb :w))
         (go add-state :west)
        (or (= verb :east) (= verb :e))
         (go add-state :east)
        (= verb :sharpen) (sharpen add-state)
        (= verb :eat) (eat add-state)
        (= verb :read) (read1 add-state)

        (= verb :buy) (buy add-state)
        (or (= verb :inventory) (= verb :i)) (ivt add-state)

        (= verb :attack) (attack add-state)

        (or (= verb :look) (= verb :examine)) (look add-state subject)

        (= verb :take) (take1 add-state subject)
        (= verb :drop) (drop1 add-state subject)


        :else (do (println "not a valid command!") state)
  )

     ))


  (defn repl
  [state]
    (loop [local-state state]
      (if (and
      (> (get-in local-state [:adventurer :hp]) 0)
      (> (get-in local-state [:adventurer :sanity]) 0)

      )
      (let [pl (been (status local-state))
          _  (println "What do you want to do?")
          command (read-line)]
          (cond (= (first (canonicalize command)) :quit) (println "Thank you for playing inferno!")
          (= (get-in local-state [:adventurer :location]) :heaven)
           (println "You win. Thanks for playing.")

            :else (recur (react pl (canonicalize command))))
          )
      (println "You died. Game over.")
      )
      ))

  (defn init-state []
    (do (println "Welcome to Inferno! You can either go north, east, west or south. You can quit anytime. "))
    {:rooms init-map
      :items {}
      :adventurer init-adventurer}
    )

  (defn main
  "Initialize the adventure"
  []
  (repl (init-state))
  )
