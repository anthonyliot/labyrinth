--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                                                                      --
--        DEVOIR HASKELL 2 : PARCOURS D'UN LABYRINTHE                   --
--                                                                      --
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                                                                      --
-- Realisation : Anthony Liot -> anthony.liot@gmail.com              --
--             : Jean Frederic LAMOURY -> jlamoury@etu.info.unicaen.fr  --
--                                                                      --
--             3eme annee de License informatique                       --
--                                                                      --
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                  SOURCE UTILISEE:                                    --
-- TD du Jeudi 7 decembre -> realisation d'un labyrinthe                --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                                                                      --
-- 			INTERFACE UTILISATEUR ET AFFICHAGE DU LABYRINTHE. 			--
--                                                                      --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--        MAIN : Ici le commencement de tout ...	                    --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--C'est le premier programme qui se lance lorsque l'on lance l'executable
--Ici on nous propose trois possibilites
--Premierement : On realise son propre labyrinthe
--Deuxiemement : On repond a des questions (coordonne , largeur , liste d'obstacles)
--Troisiemement : On utilise un des trois labyrinthe test pre-existant 

main = do 

	--On demande de choisir 
	putStr "\nWALKING THE LABYRINTH: \n\n"  
	putStr "1 --> Create your labyrinth \n"
	putStr "2 --> Answers some questions \n"
	putStr "3 --> Use pregenerated labyrinth \n"
	putStr "\nEnter your choice <Int> : \n"
	
	ch <- getInt
	choix ch 
		where 
		
			--Cette methode est celle appellee dans le main
			--elle lance le main correspondant au choix de l'utilisateur

			choix :: (Eq a, Num a) => a -> IO Char
			choix ch 
				| ch == 1 = main1
				| ch == 2 = main2
				| otherwise = main3

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--        MAIN1 : On a choisi de creer son propre labyrinthe            --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--On a choisi de realiser son propre labyrinthe : 
--					-> les 0 sont les espaces 
--					-> les 1 sont les murs
--					-> le 2 est l'etat initial
--					-> le 3 est l'etat final
--					-> Une chaine de caractere pour valider

main1 =do

	--On demande de choisir une methode
	
	putStr "\nChoose your algorithm\n  -> 1 : Sum\n  -> 2 : Weighted average\n  -> 3 : Min Max\n  -> 4 : Lexicographical\n  -> 5 : Min Deviation\n  -> 6 : Sum of Bornes\n  -> 7 : A* Standard Euclid\n  -> 8 : A* Standard Manhatan\n  -> 9 : A* Standard Approximation\n\n"
	
	putStr "Enter your choice <Int> : \n"
	methode <- getInt

	--On demande de realiser son labyrinther
	putStr "\nPlease respect the syntax, otherwise the program will emd\n"
	putStr " You can only place one starting point and one ending point\n\n"
	putStr " 0 -> Empty space \n"
	putStr " 1 -> Wall \n"
	putStr " 2 -> Init State \n"
	putStr " 3 -> Output State \n"
	putStr " 4 -> Valid the labyrint \n\n"
	putStr "You can begin : \n\n"
		
	--On lance la methode permettant d'inserer la premiere ligne et la methode.
	
	ligneLaby 0 0 (negate 1,negate 1) (negate 1,negate 1) [] methode
	
	--Un probleme ces poses lors de la realisation de l'executable, la fenetre ce ferme une fois le labyrintht dessine
	--Pour mettre le prog en "pause", on le fait tout simplement attendre en caractere d'ou l�tilite du getChar ci-dessous
	putStr "\n-- Finish --\n"
	getChar

		
--Cette methode est celle qui est appelle dans le main1
--Celle-ci permet de recuperer les chaines de 01001 qui represente le labyrinthe

ligneLaby larg haut eI eF listMur methode = 
			do
			input <- getLine                              
			dessinLaby input larg haut eI eF listMur methode

-- Recupere la ligne tape par l'utilisateur
-- Et recursivement lit caractere par caractere
-- Cette methode creer l'etat Initial, l'etat Final et le labyrinthe

dessinLaby :: String -> Int -> Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Int -> IO ()
dessinLaby input larg haut eI eF mur methode
	| larg < (length input) = dessinLaby input (length input) haut eI eF mur methode
	
	-- Un probleme est apparu pour recuperer le dernier caractere de la ligne pour cela on a cree un fonction Bis
    | tail input == "" = dessinLabyBis input larg haut eI eF mur methode
	| head input == '0' = dessinLaby (tail input) larg haut eI eF mur methode  
	| head input == '1' = dessinLaby (tail input) larg haut eI eF (((larg - length input),haut):mur) methode  
	
	-- On a mis un et unique etat initial
    | head input == '2' && eI == (negate 1,negate 1) = dessinLaby (tail input) larg haut ((larg - length input),haut) eF mur methode 
	
	-- On a mis un et unique etat Final	
	| head input == '3' && eF == (negate 1,negate 1) = dessinLaby (tail input) larg haut eI ((larg - length input),haut) mur methode
	
	-- Si l'on insere un caractere inconnu, Une erreur apparait et le programme s'arrete
    | otherwise = erreur
	
	where
	
		-- cette Methode Bis recupere le dernier caractere de la ligne et en lance une deuxieme ligne
		
		dessinLabyBis :: String -> Int -> Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Int -> IO ()
		dessinLabyBis input larg haut eI eF mur methode
				| input == "4" = creerLaby larg haut eI eF mur methode  
				| input == "0" = ligneLaby larg (haut+1) eI eF mur methode
				| input == "1" = ligneLaby larg (haut+1) eI eF (((larg - length input),haut):mur) methode
				| input == "2" && eI == (negate 1,negate 1) = ligneLaby larg (haut+1) ((larg - length input),haut) eF mur methode
				| input == "3" && eF == (negate 1,negate 1) = ligneLaby larg (haut+1) eI ((larg - length input),haut) mur methode
				-- Si l'on insere un caractere inconnu, Une erreur apparait et le programme s'arrete
				| otherwise = erreur
			where
			
				-- a partir des infos obtenus par dessinLaby on peut lancer dessineTab 
				-- On peut verifier que l'on a bien placer un etat initial et un etat final
				-- On supposera que l'utilisateur realisera un labyrinthe ayant un chemin possible
				
				creerLaby :: Int -> Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Int -> IO ()
				creerLaby largeur hauteur eI eF lesMurs meth
						| eI == eF = erreur
						| otherwise = do
								putStr "\n"
								dessineTab (eI,(largeur,hauteur,lesMurs)) (eF,(largeur,hauteur,lesMurs)) meth
	
-- Lors de la creation du labyrinthe, si on insere un caractere inconnu, le prog s'arrete	
erreur :: IO ()
erreur = putStrLn("Invalid syntax!")

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--        MAIN2 : On a choisi de repondre aux questions                 --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--On a choisi de repondre aux questions : 
--					-> un couple 1 etatInitial x <Int>
--					               etatInitiel y <Int>				
--					-> un couple 2 etatFinal x <Int>
--								   etatFinal y <Int>
--					-> une largeur Int
--					-> une liste de mur [(x,y),(x1,y1),(x2,y2)] <[(Int,Int)]>
--					-> une methode

main2 = do 
	--On demande d'inserer l'etat Initial que l'on garde ensuite dans les variables etatInitX et etatInitY
	
	putStr "\nInit State x \n"
	etatInitX <- getInt
	putStr "Init State y \n"
	etatInitY <- getInt
	
	--On demande d'inserer l'etat Final que l'on garde ensuite dans les variables etatFinX et etatFinY
	
	putStr "\nOuptut State x \n"	
	etatFinX <- getInt
	putStr "Ouptut State y \n"
	etatFinY <- getInt
	
	--On demande d'inserer ll largeur et la hauteut de la grille que l'on garde ensuite dans les variable largeur et hauteur
	
	putStr "\nWidth of the labyrinth \n"	
	largeur <- getInt
	putStr "Height of the labyrinth \n"
	hauteur <- getInt
	
	--On demande d'inserer la liste d'obstacle sous forme d'une liste de position
	
	putStr "\nList of walls [(x,y),(x1,y1),...,(xn,yn)] \n"	
	listMur <- getList
		
	--On demande de choisir une methode
	
	putStr "\nChoose your algorithm\n  -> 1 : Sum\n  -> 2 : Weighted average\n  -> 3 : Min Max\n  -> 4 : Lexicographical\n  -> 5 : Min Deviation\n  -> 6 : Sum of Bornes\n  -> 7 : A* Standard Euclid\n  -> 8 : A* Standard Manhatan\n  -> 9 : A* Standard Approximation\n\n"
	putStr "Enter your choice <Int> : \n"	
	methode <- getInt
	putStr "\n"
	
	--On lance le debut du prog qui dessine le labyrinthe en format Ascii 
	--dessinetab prend en arg un etat Initial, un etat Final, une Methode
	--On constate que on ajout a la liste d'obstacles, un autre obstacles ????
	--Non ce n'est pas stupides, cela permet d'avoir une liste d'au moins un obstacles pour eviter les bug avec les filter
	--sur une liste vide. Cette obstacle n'est d'ailleur pas visible car toujours en dehors de la grille
	--Maintenant on peut lancer un labyrinthe sans aucun obstacles.
	
	dessineTab ((etatInitX,etatInitY),(largeur,hauteur,listMur++[(largeur,hauteur)])) ((etatFinX,etatFinY),(largeur,hauteur,listMur++[(largeur,hauteur)])) methode
	
	--Un probleme ces poses lors de la realisation de l'executable, la fenetre ce ferme une fois le labyrintht dessine
	--Pour mettre le prog en "pause", on le fait tout simplement attendre en caractere d'ou l�tilite du getChar ci-dessous
	putStr "\n-- Finish --\n"
	getChar
	
--Cette methode est celle qui est appelle dans le main2
--Celle-ci appelle sa fonction Bis qui possede des accumulateur

dessineTab :: Etat -> Etat -> Int -> IO ()
dessineTab eI eF methode =
 	putStr (dessineTabBis (largeur (getLab eI)) (hauteur (getLab eI)) (listeMur (getLab eI)) (chemin methode eI eF)  0 0)
	where
	
		--dessineTabBis bien que paressant, un peu lourd a digerer, elle est pourtant toute simple
		--elle s'occuppe de recuperer les elements de la grille (largeur hauteur obstacles) un chemin et les accumulateurs
		--Cette fonction retournera seulement un chaine de caractere formant la grille et le chemin parcouru
		
		dessineTabBis :: Int -> Int -> [Position] -> [Position] -> Int -> Int -> String
		dessineTabBis l h mur chemin accX accY
		
			-- ->Dans tous ces cas on place le charactere desirer et on le concatene en lan�ant un appel recursif sur la fonction
			-- ->en incrementant l'accumulateur correspondant a la situation
		
			--cas1 c'est une bordure horizontale (haut ou bas)
			| (accY == 0 && accX <= l+1) || (accY == h+1 && accX < l+2) = "-"++dessineTabBis l h mur chemin (accX + 1) accY
			--cas2 c'est une bordure verticale (gauche ou droite)
			| accX == 0 || accX == l + 1 = "|"++dessineTabBis l h mur chemin (accX + 1) accY
			--cas3 c'est l'etat final
			| accX < l+2 && (accX-1,accY-1) == head chemin = "F"++dessineTabBis l h mur chemin (accX + 1) accY	
			--cas4 c'est l'etat initial
			| accX < l+2 && (accX-1,accY-1) == last chemin = "I"++dessineTabBis l h mur chemin (accX + 1) accY	
			--cas5 c'est par ici que passe le chemin
			| accX < l+2 && elem (accX-1,accY-1) chemin == True = "."++dessineTabBis l h mur chemin (accX + 1) accY	
			--cas6 c'est un obstacles
			| accX < l+2 && elem (accX-1,accY-1) mur == True = "#"++dessineTabBis l h mur chemin (accX + 1) accY
			--cas7 rien, on n'est seulement dans la grille
			| accX < l+2 = " "++dessineTabBis l h mur chemin (accX + 1) accY
			--cas 8 on a finit de dessiner toute la grille, on passe a la ligne
			| accY == h+1 = "\n"
			--Sinon cas9 c'est la fin d'une ligne donc passe a la ligne, on lance la ligne suivante
			| otherwise = "\n"++""++dessineTabBis l h mur chemin 0 (accY + 1)
			
		--Chemin recupere la liste de chemin parcouru par la methode choisit au debut du programme
		--Il prend le Int entree et retourne la methode correspondante
		--Au cas ou, on voudrai seulement contrarier la machine en entrant un entier different de ceux propose
		--Le parcoursApprox ( A* standard ) est utilise par defaut

		chemin :: Int -> Etat -> Etat -> [Position]
		chemin methode eI eF
				| methode == 1 = parcours1 eI eF
				| methode == 2 = parcours2 eI eF
				| methode == 3 = parcours3 eI eF
				| methode == 4 = parcours4 eI eF
				| methode == 5 = parcours5 eI eF
				| methode == 6 = parcours6 eI eF
				| methode == 7 = parcoursEuclid eI eF
				| methode == 8 = parcoursManhat eI eF
				| otherwise = parcoursApprox eI eF
	
--------------------------------------------------------------------------
--------------------------------------------------------------------------
--        MAIN3 : On a choisi d'utiliser les labyrinthes tests          --
--------------------------------------------------------------------------
--------------------------------------------------------------------------
	
--On a choisi d'utiliser un labyrinthe test : 
--					-> on choisi le labyrinthe <Int>
--					-> on choisi la methode <Int>

main3 = do 

	--On demande de choisir le labyrinthe test que l'on veut utiliser
	
	putStr " 1)+++++++      2)+++++++++++++++       3)++++++++++++++++++++++++++++++++++++++ "
	--   |     |        |I     #     F|         |#             ##     ##            #|	\n   | ### |        |# ##  #  ## #|         |I#  #     ##      #      ##    #  #F|	\n   |    F|        |#  #  #  #  #|         |  #  #   #  #    # #    #  #  #  #  |	\n   | ### |        |#  #  #  #  #|         |      #      #  #   #  #     #      |	\n   |  #  |        |#  #     #  #|         |  #    #      ##     ##     #    #  |	\n   |     |        |#  #######  #|         | #      #                  #      # |	\n   |I    |        |#           #|         |#        #        #       #        #|	\n   +++++++        +++++++++++++++         |          #      # #     #          |	\n                                          |#          ######   #####          #| 	\n                                          | ######           #           ##### |    \n                                          ++++++++++++++++++++++++++++++++++++++    \n"	
	
	putStr "\nEnter your choice <Int> : \n"
		
	lab <- getInt
		
	--On demande de choisir une methode
	
	putStr "\nChoose your algorithm\n  -> 1 : Sum\n  -> 2 : Weighted average\n  -> 3 : Min Max\n  -> 4 : Lexicographical\n  -> 5 : Min Deviation\n  -> 6 : Sum of Bornes\n  -> 7 : A* Standard Euclid\n  -> 8 : A* Standard Manhatan\n  -> 9 : A* Standard Approximation\n\n"
	putStr "\nEnter your choice <Int> : \n"
	methode <- getInt
	
	putStr "\n"
	dessinTest lab methode
	
	--Un probleme ces poses lors de la realisation de l'executable, la fenetre ce ferme une fois le labyrintht dessine
	--Pour mettre le prog en "pause", on le fait tout simplement attendre en caractere d'ou l�tilite du getChar ci-dessous
	putStr "\n-- Finish --\n"
	getChar
	
--Cette methode est celle qui est appelle dans le main3

dessinTest :: Int -> Int -> IO ()
dessinTest numLab numMeth
		-- Labyrinthe 1
		| numLab == 1 = dessineTab ((0,6),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)])) ((4,2),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)])) numMeth
		-- Labyrinthe 2
		| numLab == 2 = dessineTab ((0,0),(13,7,[(6,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(12,1),(12,2),(12,3),(12,4),(12,5),(12,6),(2,1),(3,1),(6,1),(9,1),(10,1),(3,2),(6,2),(9,2),(3,3),(6,3),(9,3),(3,4),(9,4),(3,5),(4,5),(5,5),(6,5),(7,5),(8,5),(9,5)])) ((12,0),(13,7,[(6,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(12,1),(12,2),(12,3),(12,4),(12,5),(12,6),(2,1),(3,1),(6,1),(9,1),(10,1),(3,2),(6,2),(9,2),(3,3),(6,3),(9,3),(3,4),(9,4),(3,5),(4,5),(5,5),(6,5),(7,5),(8,5),(9,5)])) numMeth
		-- Labyrinthe 3
		| numLab == 3 = dessineTab ((0,1),(36,10,[(0,0),(1,1),(2,2),(2,4),(1,5),(0,6),(0,8),(1,9),(2,9),(3,9),(4,9),(5,9),(6,9),(4,1),(5,2),(6,3),(7,4),(8,5),(9,6),(10,7),(11,8),(12,8),(13,8),(14,8),(15,8),(16,8),(17,7),(18,6),(19,7),(20,8),(21,8),(22,8),(23,8),(24,8),(25,7),(26,6),(27,5),(28,4),(29,3),(30,2),(31,1),(18,9),(35,0),(34,1),(33,2),(33,4),(34,5),(35,6),(36,8),(35,9),(34,9),(33,9),(32,9),(31,9),(9,2),(10,1),(11,1),(12,2),(13,3),(14,4),(15,4),(16,3),(17,2),(18,1),(19,2),(20,3),(21,4),(22,4),(23,3),(24,2),(25,1),(26,1),(27,2),(14,0),(15,0),(22,0),(23,0)])) ((35,1),(36,10,[(0,0),(1,1),(2,2),(2,4),(1,5),(0,6),(0,8),(1,9),(2,9),(3,9),(4,9),(5,9),(6,9),(4,1),(5,2),(6,3),(7,4),(8,5),(9,6),(10,7),(11,8),(12,8),(13,8),(14,8),(15,8),(16,8),(17,7),(18,6),(19,7),(20,8),(21,8),(22,8),(23,8),(24,8),(25,7),(26,6),(27,5),(28,4),(29,3),(30,2),(31,1),(18,9),(35,0),(34,1),(33,2),(33,4),(34,5),(35,6),(36,8),(35,9),(34,9),(33,9),(32,9),(31,9),(9,2),(10,1),(11,1),(12,2),(13,3),(14,4),(15,4),(16,3),(17,2),(18,1),(19,2),(20,3),(21,4),(22,4),(23,3),(24,2),(25,1),(26,1),(27,2),(14,0),(15,0),(22,0),(23,0)])) numMeth	
		-- Si l'on rentre un chiffre ne correspondant pas a un labyrinthe, on lance le laby 1 par default
		| otherwise =  dessineTab ((0,6),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)])) ((4,2),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)])) numMeth

--------------------------------------------------------------------
--        ACCES AUX DONNEES ENTREES PAR L'UTILISATEUR.            --
--------------------------------------------------------------------
	
--getInt permet d'attendre une entree de type entier en cas d'entree differente on obtient une erreur
getInt :: IO Int
getInt = readLn

--getInt permet d'attendre une entree de type liste de position en cas d'entree differente on obtient une erreur
getList :: IO [(Int,Int)]
getList = readLn

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                                                                      --
--        Les fonctions principales pour tester un Algorithme A*        --
--                                                                      --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--Pour un algorithme A* standart il faut :

------------------------------------------------------------------
-- Un etat initial donnant la position de depart dans la grille --
------------------------------------------------------------------

--exemple d'etat inital
--etatInitial :: Etat
--etatInitial = ((0,6),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)]))

--Un etat final donnant la position d'arrivee dans la grille
--exemple d'etat Final
--etatFinal :: Etat
--etatFinal = ((4,2),(5,7,[(1,1),(2,1),(3,1),(1,3),(2,3),(3,3),(2,4)]))

---------------------------------------------------------------------------
-- Un estFinal qui permet de dire si l'etat courant est l'etat d'arriver --
---------------------------------------------------------------------------
estFinal :: Etat -> Etat -> Bool
estFinal etat etatFinal
		| etat == etatFinal = True
		| otherwise = False
		
---------------------------------------------------------------------------
-- Un estValide permettant de savoir si le deplacement est valide ou non --
---------------------------------------------------------------------------

estValide :: Laby -> Etat -> Bool
estValide l  e
	| (getX p) < 0 	 || (getX p) > ((largeur l) - 1) 	= False
	| (getY p) < 0	 || (getY p) > ((hauteur l) - 1)    = False
	| (isMur p l)						= False
	| otherwise							= True
	where
	p = getRobot e		

--------------------------------------------------------------------------
--Un successeur permettant de donner les successeur possibles d'un etat --
--------------------------------------------------------------------------

successeurs :: Etat -> [Position] -> [Etat]
successeurs e mem = filter (estValide lab)  [(x,lab) | 
				       		 x <- (filter (notIN mem) lmvt)]
        where
			robotX = getX (getRobot e)
			robotY = getY (getRobot e)
			lab = getLab e
			mvtH e = (robotX,(robotY - 1)) : []
			mvtB e = (robotX,(robotY + 1)) : []
			mvtG e = ((robotX - 1),robotY) : []
			mvtD e = ((robotX + 1),robotY) : []
			lmvt = mvtG e ++ mvtD e ++ mvtH e ++ mvtB e

---------------------------------------------------------------
-- Le calcul du chemin en respectant un criteres donnee 'op' --
---------------------------------------------------------------

profondeurA :: Etat -> File EtatHisto -> [Position] -> (EtatHisto -> EtatHisto -> Bool) -> [Position]
profondeurA etatF p lp op
        | isEmptyF p            = []
        | estFinal etat etatF   = robot : (getHisto (frontF p))
        | otherwise             =
        profondeurA etatF 
                (enqueueL
                        [(x,(robot:(getHisto (frontF p)))) | 
				    x <- (successeurs (getEtat (frontF p)) lp)]
                        (dequeue p)
			op
                )
                nlp
		op

        where
        etat = getEtat (frontF p)
        robot = getRobot etat
        nlp = robot : lp

------------------------------------------------------------------
--        Application de l'algorithme A* Standart               --
------------------------------------------------------------------

-- Pour realiser un algorithme A* standart, il faut seulement appliquer
-- l'algo a un critere, pour cela on va utiliser soit le crit�re distance euclidienne
-- soit la distance manhattan, soit l'approximation 

opEuclid :: Etat -> EtatHisto -> EtatHisto -> Bool
opEuclid ef e1 e2 = distFEuclid ef e1 < distFEuclid ef e2

parcoursEuclid :: Etat -> Etat -> [Position]
parcoursEuclid etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opEuclid etatF) ) [] (opEuclid etatF)

opManhat :: Etat -> EtatHisto -> EtatHisto -> Bool
opManhat ef e1 e2 = distFManhat ef e1 < distFManhat ef e2

parcoursManhat :: Etat -> Etat -> [Position]
parcoursManhat etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opManhat etatF) ) [] (opManhat etatF)

opApprox :: Etat -> EtatHisto -> EtatHisto -> Bool
opApprox  ef e1 e2 = distFApprox ef e1 > distFApprox ef e2

parcoursApprox  :: Etat -> Etat -> [Position]
parcoursApprox  etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opApprox  etatF) ) [] (opApprox  etatF)


--------------------------------------------------------------------------
--------------------------------------------------------------------------
--                                                                      --
--      	  Les methodes de l'Algorithme A* multi-criteres            --
--                                                                      --
--------------------------------------------------------------------------
--------------------------------------------------------------------------

-------------------------------------------------------------------------
-- Creation d'un type vecteurEtat etant simplement une liste d'entier  --
-------------------------------------------------------------------------

type VecteurEtat = [Int]

--Vect prend un etat et un etatHisto et retourne une liste des valeurs des criteres appliques a ces etats
--Un utilise le negate pourquoi????
--Lorsque l'on prend la distance euclidienne (ou distance manhattan) de deux etat, on se trouve, 
--en appliquant les differentes methodes avec le chemin plus long E1 > E2 = True
--Le negate mettant leur valeur en negatif on obtient E1 > E2 = False soit le chemin le plus direct ces
--ce que l'on veut.

vect :: Etat -> EtatHisto -> VecteurEtat
vect ef etat = [(distFApprox ef etat) ,  negate (distFEuclid ef etat) , negate (distFManhat ef etat)]

-----------------
--Methode Somme--
-----------------

opSomme :: Etat -> EtatHisto -> EtatHisto -> Bool
opSomme ef e1 e2 = sum (vect ef e1) > sum (vect ef e2)

parcours1 :: Etat -> Etat -> [Position]
parcours1 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opSomme etatF) ) [] (opSomme etatF)	

---------------------------	
--Methode MoyennePonderee--
---------------------------

opMoyenne :: Etat -> EtatHisto -> EtatHisto -> Bool
opMoyenne ef e1 e2 = div (sum (zipWith (*) [30,40,30] (vect ef e1))) 100 > div (sum (zipWith (*) [30,40,30] (vect ef e2))) 100

parcours2 :: Etat -> Etat -> [Position]
parcours2 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opMoyenne etatF) ) [] (opMoyenne etatF)

------------------
--Methode MINMAX--
------------------

opMinMax :: Etat -> EtatHisto -> EtatHisto -> Bool
opMinMax ef e1 e2 = maximum (vect ef e1) < maximum (vect ef e2)

parcours3 :: Etat -> Etat -> [Position]
parcours3 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opMinMax etatF) ) [] (opMinMax etatF)

---------------------------
--Methode Lexicographique--
---------------------------

-- Cette Methode est beaucoup plus longue a l'execution, non pas parcequ'elle est mal programme mais par le fait 
-- qu'elle realise un appelle recursif sur la liste des vecteurs (distEuclid , distManhatt , Approx)
-- Dans notre cas, a chaque etat on peut avoir trois appels recursifs successif
-- Dans le pire cas, on peut imaginer que pour tout les etats on fais trois appels a chaque fois ce qui multiplie le temps 
-- d'execution

opLexi :: Etat -> EtatHisto -> EtatHisto -> Bool
opLexi ef e1 e2 = opLexiBis (vect ef e1) (vect ef e2)
			where
				opLexiBis :: VecteurEtat -> VecteurEtat -> Bool
				opLexiBis v1 v2 
						| v1 == [] || v2 == [] = True
						| max1 < max2 = True
						| max1 > max2 = False
						| otherwise = opLexiBis l1 l2
					where
						max1 = maximum v1
						max2 = maximum v2
						l1 = filter (< max1) v1
						l2 = filter (< max2) v2
		
parcours4 :: Etat -> Etat -> [Position]
parcours4 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opLexi etatF) ) [] (opLexi etatF)

--------------------
--Methode MINECART--
--------------------

opMineCart :: Etat -> EtatHisto -> EtatHisto -> Bool
opMineCart ef e1 e2 = maximum (vect ef e1) - minimum (vect ef e1) < maximum (vect ef e2) - minimum (vect ef e2)

parcours5 :: Etat -> Etat -> [Position]
parcours5 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opMineCart etatF) ) [] (opMineCart etatF)

--------------------------
--Methode SommeDesBornes--
--------------------------

opSommeDesBornes :: Etat -> EtatHisto -> EtatHisto -> Bool
opSommeDesBornes ef e1 e2 = minimum (vect ef e1) + maximum (vect ef e1) > minimum (vect ef e2) + maximum (vect ef e2)

parcours6 :: Etat -> Etat -> [Position]
parcours6 etatI etatF =  profondeurA etatF (enqueue (etatI,[]) emptyQueue (opSommeDesBornes etatF) ) [] (opSommeDesBornes etatF)


------------------------------------------------------------------------
------------------------------------------------------------------------
-- 			Application de l'Algorithme A* multi-criteres 	          --
------------------------------------------------------------------------
------------------------------------------------------------------------
--   LES TROIS CRITERES DEMANDES : MANHATAN, EUCLIDIENNE ET OBSTACLES   --
------------------------------------------------------------------------
------------------------------------------------------------------------

----------------------------------
--Critere : Division Euclidienne--
----------------------------------

--Heuristique H = estimation du co�t de l'etat(n) a l'etat(n')
--On realise le calcul de la distance euclidienne entre deux position
--Pour eviter d'utiliser autre chose que des Int, on prefere garder le carre de la distance euclidienne
--Mais pour ne pas fausser les resultat on passera le critere manhattan au carrees

distHEuclid :: Position -> Position -> Int
distHEuclid (x1,y1) (x2,y2) = (x1 - x2)^2 + (y1 - y2)^2

--g(n) = le co�t g(n) + le co�t pour aller de l'etat Initial a etat(n)
--On considerera le cout comme les etats qui ont ete visite precedemment

distGEuclid :: EtatHisto -> Int
distGEuclid etat = length (getHisto etat)

--f(n�) = g(n�) + H ; c'est l'heuristique 

distFEuclid :: Etat -> EtatHisto -> Int
distFEuclid etatF etat = (distGEuclid etat) + distHEuclid (getRobot (getEtat etat)) (getRobot etatF)

-------------------------------
--Critere : Distance Manhatan--
-------------------------------

--Heuristique H = estimation du co�t de l'etat(n) a l'etat Final
--On realise le calcul de la distance manhattan entre deux position
--Pour eviter d'utiliser autre chose que des Int, on prefere garder le carre de la distance euclidienne
--Mais pour ne pas fausser les resultat le passe au carrees comme explique ci-dessus

distHManhat :: Position -> Position -> Int
distHManhat (x1,y1) (x2,y2) = (abs (x1 - x2) + abs (y1 - y2))^2
 
--g(n) = le co�t g(n) + le co�t pour aller de etat Initial a etat(n)
--On considerera le cout comme les etats qui ont ete visite precedemment

distGManhat :: EtatHisto -> Int
distGManhat etat = length (getHisto etat)

--f(n�) = g(n�) + H ; c'est l'heuristique 

distFManhat :: Etat -> EtatHisto -> Int
distFManhat etatF etat = (distGManhat etat) + distHManhat(getRobot (getEtat etat)) (getRobot etatF)

-------------------------------------------------
--Critere : Approximite du chemin des obstacles--
-------------------------------------------------

--Heuristique H = estimation du co�t de l'etat(n) a l'etat Final
--On calcul la distance entre la position et tout les obstacles
--On garde le minimum (la position la plus proche d'un mur)

distHApprox :: Position -> [Position] -> Int
distHApprox (x1,y1) l = (minimum (map (distHEuclid (x1,y1)) l))
 
--g(n) = le co�t pour aller de l'etat Initial a etat(n)
--On considerera le cout comme les etats qui ont ete visite precedemment

distGApprox :: EtatHisto -> Int
distGApprox etat = length (getHisto etat)

--f(n�) = g(n�) + H ; c'est l'heuristique 

distFApprox :: Etat -> EtatHisto -> Int
distFApprox etatF etat = (distGApprox etat) + distHApprox(getRobot (getEtat etat)) (listeMur(getLab etatF))

--------------------------------------------------------------------
--------------------------------------------------------------------
--                LES DEFINITIONS DES TYPES UTILISES              --
--          TD du Jeudi 7 decembre -> realisation d'un labyrinthe --
--------------------------------------------------------------------
--------------------------------------------------------------------
--  		     		STRUCTURE UTILISe        			      --
--------------------------------------------------------------------
--------------------------------------------------------------------

notIN :: (Eq a) => [a] -> a -> Bool
notIN []  _ = True
notIN (x:xs) e
	| x == e 	= False
	| otherwise	= notIN xs e

----------------------
--  TYPE POSITION   --
----------------------

type Position = (Int,Int)

getX :: Position -> Int
getX = fst

getY :: Position -> Int
getY = snd

------------------------------------
--     	   TYPE ROBOT    	      --
-- 		(Tuple d'entier)		  --
------------------------------------

type Robot = Position

-----------------------------------------
--     			TYPE LABY  	           --
-- (largeur,hauteur,liste d'obstacles) --
-----------------------------------------

type  Laby = (Int,Int,[Position])

largeur :: Laby -> Int
largeur (l,h,m) = l

hauteur :: Laby -> Int
hauteur (l,h,m)  =  h

listeMur :: Laby -> [Position]
listeMur (l,h,m)  =  m

isMur :: Position -> Laby -> Bool
isMur pos (l,h,m) = elem pos m

---------------------------------------
--            TYPE ETAT              --
--           (Robot,Laby)            --
---------------------------------------

type Etat = (Robot,Laby)

getRobot :: Etat -> Robot
getRobot = fst

getLab :: Etat -> Laby
getLab = snd

-----------------------------------
--         TYPE ETATHISTO        --
--     (Etat,liste de Position)  --
-----------------------------------

type EtatHisto = (Etat,[Position])

getEtat :: EtatHisto -> Etat
getEtat = fst

getHisto :: EtatHisto -> [Position]
getHisto = snd

---------------------------------------
--          TYPE FILE A PRIORITE     --
---------------------------------------

type File a = [a]

emptyQueue :: File a
emptyQueue = []

isEmptyF :: File a -> Bool
isEmptyF [] = True
isEmptyF _  = False

enqueue :: a -> File a -> (a->a->Bool) -> File a
enqueue a [] op = a:[]
enqueue a (x:xs) op
	| op a x	= 	a : (x:xs)
	| otherwise	= 	x : (enqueue a xs op)

enqueueL :: [a] -> File a -> (a->a->Bool) -> File a
enqueueL [] f op     = f
enqueueL (x:xs) f op = enqueueL xs (enqueue x f op) op

frontF :: File a -> a
frontF = head

dequeue :: File a -> File a
dequeue = tail