--
Rend du Projet Haskell
de
Yemouna Chikbouni
et
Pacôme Perrotin

--
Compilation :
Afin de compiler le programme, faites :
- make

--
Utilisation :
L'exécutable a pour nom xmlSearch et s'utilise comme suit :
- ./xmlSearch (requête | -f (fichier requête)) (fichier arbre xml)
Avec (requête) une chaîne de caractère correspondant à une requête XPath
et (fichier requête) et (fichier arbre xml) des chemins de fichiers

--
Formats :
Les fichiers xml lus doivent être corrects syntaxiquement.
Sont authorisés les bornes <a> </b>, les noeud textuels, et les arguments.

Les requêtes XPath doivent également suivre une syntaxe précise.
Les seules conditions authorisées sont :
- @attr=value
- str=value
- sibling

--
Organisation du code :
Le code source est divisé entre Parser.hs et xmltree.hs.
- Parser.hs fournit un ensemble d'outils de base pour parser du texte.
- xmltree.hs est organisé en quatre grandes parties :
	- Le main et l'ouveture des fichiers (fonctions IO)
	- Le parseur d'arbre Xml
	- Le parseur de requêtes XPath suivit des fonctions qui effectuent une requête
	- Des objets et des fonctions de tests

--
Tests :
Afin de tester la fiabilité du programme, nous avons mis en oeuvre un
ensemble de fonctions de tests et d'exemples.
En bas du fichier xmltree.hs vous trouverez une fonction testXmlTreeParsing
dont le but est de lire un arbre et de l'afficher plusieurs fois pour vérifier
que la sortie et l'entrée concordent.
Quelques objets ont été créés pour manipuler facilement des exemples.
- l'arbre exemple_1 peut subir les requêtes request_11, request_12 ... request_16
- l'arbre exemple_2 peut subir les requêtes request_21, request_22 ... request_26
Ces exemples sont facilements utilisables sous ghci avec :
- query request_ab exemple_a

--
Problèmes rencontrés et tentatives de résolution :
Le programme rendu exécute très des requêtes simples ou complexes sur des
arbres de tailles variables, et ce en un temps réduit.
Cependant, nous sommes arrivés à un grave problème de performances en ce qui
concerne le parsing de l'arbre xml, lorsque celui-çi devenait trop gros.
De prime abord nous avons crus à une boucle infinie. Mais des tests simples ont
démontrés que le processus prenait un temps de plus en plus grand à parser
l'arbre xml lorsque celui ci dépassait la vingtaine de noeuds.
Ce phénomène peut être simplement observé, car lorsque le parsing de l'arbre
"tree.xml" est instantanné, celui de "text.xml" prend un temps léger, et
celui de "exemple_catalogue.xml" un temps très long.
Dans la motivation de trouver un remède à ce problème, nous avons tenté de
profiler avec l'option de compilation -prof le programme lors de son exécution.
Le résultat (disponible sous xmlSearch.prof) semble exprimé que le principal
du temps est utilisé par des fonctions comme parserSpace ; fonction utilisée
pour parser un nombre indéfini d'espaces, de retours à la ligne, et de tabulations.

Nous n'avons donc pas trouvé de solutions à ce problème. Il s'agit peut-être du fait
que la trop grande succession de parseurs provoque une utilisation mémoire trop importante.

