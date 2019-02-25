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
      (if(> (count p1) 0)
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
    (if(> (count v) 0)
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
    (if(> (count p) 0)
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
    (if(> (count v3) 0)
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

(defn gagner [v]
  (println "Vous avez gagner la partie, la bonne combinaison etait bel et bien:\n"v))

(defn gagner? [v]
  (loop[v v]
    (if(> (count v) 0)
      (if (not= (compare (first v) :good) 0)
        false
        (recur (rest v)))
      true)))

(defn prendre-n [v n]
  (loop[v v
        n n
        r []]
    (if(> n 0)
      (recur (rest v) (- n 1) (conj r (first v)))
      r)))

(defn essaie [ind]
  (loop[ind ind
        v 0
        l []]
    (if(not= (count ind) 0)
      (if(== (compare (first ind) :good) 0)
        (recur (rest ind) (+ v 1) (conj l v))
        (recur (rest ind) (+ v 1) l))
      l)))
      

(defn rat [v l c]
  (loop[l l
        r v]
    (if(> (count l) 0)
      (recur (rest l) (assoc r (first l) c))
      r)))

(defn pvector [c i]
  (loop[v []]
    (if(< (count v) i)
      (recur (conj v c))
      v)))

(defn cle-valeur [m]
  (loop[m m
        r (pvector :null TAILLE)]
    (if(> (count m) 0)
      (recur (rest m) (rat r (last (first m)) (first (first m))))
      r)))
       
            
(defn definir_mastermind 
  ([] (definir_mastermind (code-secret TAILLE)))
  ([code] (presentation) (def mastercode code)))

(defn mastermind 
  ([c1 c2 c3 c4 c5] (mastermind (vec [c1 c2 c3 c4 c5])))
  ([v]
   (let [ ind (filtre-indications mastercode v (indications mastercode v))]
    (if(gagner? ind)
      (gagner v))
    (vec ind))))
  
   
(defn couleurs-localisation []
  (loop [couleurs [:rouge :bleu :vert :jaune :noir :blanc]
         e (assoc {} :rouge 0 :bleu 0 :vert 0 :jaune 0 :noir 0 :blanc 0)]
    (let [ ind (mastermind (pvector (first couleurs) TAILLE))]
      (if(> (count couleurs) 1)
        (recur (rest couleurs) (assoc e (first couleurs) (essaie ind)))
        (assoc e (first couleurs) (essaie ind))))))



(defn solver []
  (let [ l (println "La bonne reponse est:")] (cle-valeur (couleurs-localisation))))


(fact "Le mastermind fonctionne correctement."
      (definir_mastermind [:blanc :rouge :vert :rouge :bleu])
      
      (mastermind [:blanc :noir :vert :rouge :bleu])
      => [:good :bad :good :good :good]
  
      (mastermind [:blanc :vert :rouge :rouge :bleu])
      => [:good :color :color :good :good]
  
      (mastermind [:blanc :rouge :vert :rouge :bleu])
      => [:good :good :good :good :good]
  


  
  
  (fact "Le solveur du mastermind fonctionne correctement."
      (definir_mastermind [:noir :rouge :vert :rouge :bleu])
      
      (solver)
      => [:noir :rouge :vert :rouge :bleu]))
      
  
  

      
