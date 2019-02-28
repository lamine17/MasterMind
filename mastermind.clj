(ns theme01-clojure.mastermind.mastermind
  (:use midje.sweet))



(defn code-secret [n]
  (loop [s []]
    (if(< (count s) n)
      (recur (conj s (rand-nth [:rouge :bleu :vert :jaune :noir :blanc])))
      s)))
      


(fact "Le `code-secret` est bien composé de couleurs."
      (every? #{:rouge :bleu :vert :jaune :noir :blanc} 
              (code-secret 4))
      => true)

(fact "Le `code-secret` a l'air aléatoire."
      (> (count (filter true? (map not= 
                                   (repeatedly 20 #(code-secret 4))
                                   (repeatedly 20 #(code-secret 4)))))
         0)
      => true)


(defn indications [ code prop]
  (let [ens (into #{} code)]
    (loop [p1 prop
           c1 code
           r []]
      (if(seq p1)
        (if(== ( compare (first p1) (first c1)) 0)
          (recur (rest p1) (rest c1) (conj r :good))
          (if(contains? ens (first p1))
            (recur (rest p1) (rest c1) (conj r :color))
            (recur (rest p1) (rest c1) (conj r :bad))))
        r))))
            




(fact "`indications` sont les bonnes."
      (indications [:rouge :rouge :vert :bleu] 
                   [:vert :rouge :bleu :jaune])
      => [:color :good :color :bad]
      
      (indications [:rouge :rouge :vert :bleu] 
                   [:bleu :rouge :vert :jaune])
      => [:color :good :good :bad]
      
      (indications [:rouge :rouge :vert :bleu] 
                   [:rouge :rouge :vert :bleu])
      => [:good :good :good :good]
      
      (indications [:rouge :rouge :vert :vert] 
                   [:vert :bleu :rouge :jaune])
      => [:color :bad :color :bad])



(defn frequences  [v]
  (loop [m {}
         v v]
    (if(seq v)
      (if(contains? m (first v))
        (recur (update m (first v) inc) (rest v))
        (recur (assoc m (first v) 1) (rest v)))
      m)))

    


(fact "les `frequences` suivantes sont correctes."
      (frequences [:rouge :rouge :vert :bleu :vert :rouge])
      => {:rouge 3 :vert 2 :bleu 1}
      
      (frequences [:rouge :vert :bleu])
      => {:rouge 1 :vert 1 :bleu 1}
      
      (frequences [1 2 3 2 1 4]) => {1 2, 2 2, 3 1, 4 1})


(defn freqs-dispo1 [r p]
  (loop[fr (frequences r)
        r r
        p p]
    (if(seq p)
      (if(== (compare (first p) :good) 0)
        (recur (update fr (first r) dec) (rest r) (rest p))
        (recur fr (rest r) (rest p)))
      fr)))


(fact "Les fréquences disponibles de `freqs-dispo` sont correctes."
      (freqs-dispo1 [:rouge :rouge :bleu :vert :rouge]
                   [:good :color :bad :good :color])
      => {:bleu 1, :rouge 2, :vert 0})


(defn filtre-indications [v1 v2 v3]
  (loop[fr (frequences v1)
        fp (frequences v2)
        v2 v2
        v3 v3
        v1 v1
        r []]
    (if(seq v3)
      (if (== (compare (last v3) :color) 0)
        (if(> (get fp (last v2)) (get fr (last v2)))
          (recur (update fr (last v2) inc) fp (butlast v2) (butlast v3) (butlast v1) (cons :bad r))
          (recur fr fp (butlast v2) (butlast v3) (butlast v1) (cons (last v3) r)))
        (recur fr fp (butlast v2) (butlast v3) (butlast v1) (cons (last v3) r)))
      r)))

    
            


(fact "Le `filtre-indications` fonctionne bien."
      (filtre-indications [:rouge :rouge :vert :bleu] 
                          [:vert :rouge :bleu :jaune]
                          [:color :good :color :bad])
      => [:color :good :color :bad]
      
      (filtre-indications [:rouge :vert :rouge :bleu] 
                          [:rouge :rouge :bleu :rouge]
                          [:good :color :color :color])
      => [:good :color :color :bad])
      


;; La variable globale TAILLE a pour role que d'indiquer la taille du mastermind

(def TAILLE 5)


(defn presentation []
  (println "Ceci est un jeu de mastermind, tapez les couleurs que vous souhaitez jouer parmi:\n:rouge :bleu :vert :jaune :noir :blanc. \nLa combinaison doit être composé de" TAILLE "couleurs.  \n"))

            
;; La fonction definir_mastermind permet de definir aleatoirement ou non une nouvelle combinaison 
;; gagnante.

(defn definir_mastermind 
  ([] (definir_mastermind (code-secret TAILLE)))
  ([code] (presentation) (def mastercode code)))

(defn gagner [v]
  (println "Vous avez gagner la partie, la bonne combinaison etait bel et bien:\n"v)
  (definir_mastermind))

;; La fonction gagner? determine si oui ou non la combinaison proposé par le jour 
;; retourne un vecteur d'indications ne contenant que des :good, auquel cas il
;; a gagné la partie, sinon il ne l'a toujours pas gagné.
;; Dans le cas où la bonne combinaison a été trouvé, on reinitiaise une nouvelle
;; combinaison pour le mastermind.

(defn gagner? [v]
  (loop[v v]
    (if(seq v)
      (if (not= (compare (first v) :good) 0)
        false
        (recur (rest v)))
      true)))

;; La fonction essaie renvoie un vecteur contennant l'indice 
;; des :good contenue dans un vecteur.

(defn essaie [ind]
  (loop[ind ind
        v 0
        l []]
    (if(not= (count ind) 0)
      (if(== (compare (first ind) :good) 0)
        (recur (rest ind) (+ v 1) (conj l v))
        (recur (rest ind) (+ v 1) l))
      l)))
      
;; La fonction rat retourne un vecteur r
;; contenant a chaque indice passé dans un 
;; tableau l la valeur c.

(defn rat [v l c]
  (loop[l l
        r v]
    (if(seq l)
      (recur (rest l) (assoc r (first l) c))
      r)))

;; La fonction pvector retourne un vecteur 
;; de taille i contenant uniquement des elements
;; c.

(defn pvector [c i]
  (loop[v []]
    (if(< (count v) i)
      (recur (conj v c))
      v)))

;; La fonction cle-valeur effectue une conversion cle-valeur <-> vecteur
;; c'est a dire que chaque indices de ce vecteur est initialisé a la valeur indiqué par la clé
;; de même valeur dans l"ensemble m passé en parametre ces indices etant les valeurs contenue
;; dans le tableau poité par cette clé.

;; exemple: {:rouge [1 2], :vert [0] } -> [:vert :rouge :rouge]

(defn cle-valeur [m]
  (loop[m m
        r (pvector :null TAILLE)]
    (if(seq m)
      (recur (rest m) (rat r (last (first m)) (first (first m))))
      r)))
       
;; La fonction mastermind est la fonction principal du mastermind.
;; Elle permet au joueur de passer en parametre la combinaison qu'il souhaite proposer.

(defn mastermind 
  ([c1 c2 c3 c4 c5] (mastermind (vec [c1 c2 c3 c4 c5])))
  ([v]
   (let [ ind (filtre-indications mastercode v (indications mastercode v))]
    (if(gagner? ind)
      (gagner v))
    (vec ind))))
  
;; La fonction couleurs-localisation permet au solver de localiser
;; la bonne localisation de chaque couleurs presente dans la combinaison.
   
(defn couleurs-localisation []
  (loop [couleurs [:rouge :bleu :vert :jaune :noir :blanc]
         e (assoc {} :rouge 0 :bleu 0 :vert 0 :jaune 0 :noir 0 :blanc 0)]
    (let [ ind (mastermind (pvector (first couleurs) TAILLE))]
      (if(> (count couleurs) 1)
        (recur (rest couleurs) (assoc e (first couleurs) (essaie ind)))
        (assoc e (first couleurs) (essaie ind))))))

;; Le solver renvoie la bonne combinaison et permet de resoudre le mastermind.

(defn solver []
  (let [ l (println "La bonne reponse est:")] (cle-valeur (couleurs-localisation))))


(fact "Le mastermind fonctionne correctement."
      (definir_mastermind [:blanc :rouge :vert :rouge :bleu])
      
      (mastermind [:blanc :noir :vert :rouge :bleu])
      => [:good :bad :good :good :good]
  
      (mastermind [:blanc :vert :rouge :rouge :bleu])
      => [:good :color :color :good :good]
  
  
      (mastermind [:blanc :rouge :rouge :rouge :bleu])
      => [:good :good :bad :good :good]
  
      (mastermind [:blanc :rouge :vert :rouge :bleu])
      => [:good :good :good :good :good]
      (let [code (code-secret TAILLE)]
        (definir_mastermind code)
        (mastermind code)   
        =>[:good :good :good :good :good]))
  


  
  
(fact "Le solveur du mastermind fonctionne correctement."
  (definir_mastermind [:noir :rouge :vert :rouge :bleu])
  
  (solver)
  => [:noir :rouge :vert :rouge :bleu]
  
  (let [code (code-secret TAILLE)]
    (definir_mastermind code)
    (solver)
    => code))
  

      
