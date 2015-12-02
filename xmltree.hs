import Parser

type XmlText = String
type XmlNom = String
type XmlAttribut = (String, String)

data XmlElement = Tree XmlTree | Text XmlText
			
type XmlTree = (XmlNom, [XmlAttribut], [XmlElement])

tree :: XmlTree
tree = ("books",[],[Tree ("book",[("language"," French")],[Tree ("title",[],[Text "bla"]),Tree ("author",[],[Text "bla"])]),Tree ("book",[],[Tree ("title",[],[Text "bla"]),Tree ("author",[],[Text "Flaubert "]),Tree ("author",[],[Text "Gautier "])])])

text1, text2, text3 :: XmlText
text1 = "blablabla"
text2 = "blobloblo"
text3 = "bliblibli"

nom1, nom2, nom3 :: XmlNom
nom1 = "noeud 1"
nom2 = "noeud 2"
nom3 = "noeud 3"

a1, a2, a3 :: XmlAttribut
a1 = ("force", "3")
a2 = ("valeur", "patate")
a3 = ("spiritualité", "vague")

e1, e2, e3, e4, e5 :: XmlElement
e1 = (Text text1)
e2 = (Text text2)
e3 = (Text text3)
e4 = (Tree (nom1, [], [e1, e2]) )
e5 = (Tree (nom2, [], [e3, e4]) )

showXmlText :: XmlText -> String
showXmlText text = text

showXmlNom :: XmlNom -> String
showXmlNom nom = nom

showXmlAttribut :: XmlAttribut -> String
showXmlAttribut (space, value) = space ++ "=\"" ++ value ++ "\""

showXmlAttributs :: [XmlAttribut] -> String
showXmlAttributs [] = ""
showXmlAttributs (x:[]) = showXmlAttribut x
showXmlAttributs (x:xs) = showXmlAttribut x ++ ", " ++ showXmlAttributs xs

showXmlElement :: XmlElement -> String
showXmlElement (Tree (name, attributs, nodes) ) =
			"<" ++ showXmlNom name ++ " " ++ showXmlAttributs attributs ++ ">"
			++ showXmlElements nodes
			++ "</" ++ showXmlNom name ++ ">"
showXmlElement (Text text) = text

instance Show XmlElement where
		show = showXmlElement

showXmlElements :: [XmlElement] -> String
showXmlElements [] = ""
showXmlElements (x:xs) = showXmlElement x ++ "\n" ++ showXmlElements xs

showXmlTree :: XmlTree -> String
showXmlTree tree = showXmlElement (Tree tree) ++ "\n"

parserChars :: String -> Parser Char
parserChars [] = MkParser (\_ -> Right "Impossible de filtrer avec un filtre vide")
parserChars xs = MkParser( \s -> case s of
							"" 		-> Right ("Tentative de filter " ++ xs ++ " sur une chaine vide")
							(c:cs)  -> if elem c xs
									   then Left (c, cs)
									   else Right ("\"" ++
												  [c] ++
												  "\" trouve en l'attente d'un parmis \"" ++
												  xs ++ "\"") )

parserNotChars :: String -> Parser Char
parserNotChars "" = MkParser ( \s -> case s of
	"" -> Right "Chaîne vide rencontree"
	(c:cs) -> Left (c, cs) )

parserNotChars xs = MkParser( \s -> case s of
							"" 		-> Right ("Tentative de filter non " ++ xs ++ " sur une chaine vide")
							(c:cs)  -> if elem c xs
									  then Right ("\"" ++
												  [c] ++
												  "\" trouve en l'attente d'autre chose que \"" ++
												  xs ++ "\"")
									  else Left (c, cs) )


parserWord :: String -> Parser [Char]
parserWord [] = MkParser ( \cs -> Left ([], cs) )
parserWord (c:cs) = parserChars [c] >>= \r -> parserWord cs >>= \rs -> return (r:rs)

parserSpace :: Parser [Char]
parserSpace = zeroOuPlus ( parserChars " \n\t" )

parserTextUntil :: String -> Parser [Char]
parserTextUntil [] = MkParser( \cs -> Left (cs, []) )
parserTextUntil filtre = MkParser(
					\str -> case str of
						[] -> Left ([], [])
						(c:cs) -> case parse (parserNotChars filtre) (c:cs) of
							Right error -> Left ([], (c:cs))
							Left (x, xs) -> ( case parse (parserTextUntil filtre) xs of
								Right error -> Right error -- ce cas n'arrive jamais dans la pratique
								Left(y, ys) -> Left(x:y, ys) )
									 )
parserNoeudElement :: Parser XmlElement
parserNoeudElement = parserNoeud >>= \noeud -> return (Tree noeud)

parserTree :: Parser XmlTree
parserTree = parserNoeud

parserNoeud :: Parser XmlTree
parserNoeud = do
				parserSpace;
				(nom, attributs) <- parserBorneOuvrante;
			  	parserSpace;
			  	elements <- zeroOuPlus parserElement;
				parserSpace;
			  	parserBorneFermante nom;
			  	return (nom, attributs, elements);

parserElement :: Parser XmlElement
parserElement = parserNoeudElement ||| parserTextElement

parserBorneOuvrante :: Parser (XmlNom, [XmlAttribut])
parserBorneOuvrante = parserChars "<"
					  >> parserNom
					  >>= \nom -> parserSpace
					  >> parserListeAttributs 
					  >>= \listeAttribut -> parserChars ">"
					  >> return (nom, listeAttribut)

parserListeAttributs :: Parser [XmlAttribut]
parserListeAttributs = zeroOuPlus
						(parserSpace >> parserAttribut)

parserAttribut :: Parser XmlAttribut
parserAttribut = parserNom >>= \domaine -> parserSpace
				 >> parserWord "=\"" 
				 >> parserTextUntil "\""
				 >>= \valeur -> parserChars "\""
				 >> return (domaine, valeur)

parserTextElement :: Parser XmlElement
parserTextElement = parserText >>= \text -> return (Text text)

parserText :: Parser [Char]
parserText = parserSpace
			 >> parserTextUntil "<"
			 >>= \text -> case text of
				"" -> fail ""
				cs -> return text

parserNom :: Parser [Char]
parserNom = parserTextUntil "=<> \"\t\n"

parserBorneFermante :: String -> Parser XmlNom
parserBorneFermante nom = parserSpace
						  >> parserWord "</"
						  >> parserWord nom
						  >> parserSpace
						  >> parserChars ">"
						  >> return nom

data XRequest = Rien | Path XPath
type XPath = (XElement, XRequest)
type XElement = (String, [XCondition])
type XCondition = (String, String)

instance Show XRequest where
		show = showXRequest

showXRequest :: XRequest -> String
showXRequest Rien = "/"
showXRequest (Path p) = "/" ++ (showXPath p)

showXPath :: XPath -> String
showXPath (elem, request) = (showXElement elem) ++ ( case request of
					Rien -> ""
					Path _ -> (showXRequest request)
				)

showXElement :: XElement -> String
showXElement (cs, conditions) = cs ++ (showXConditions conditions)

showXConditions :: [XCondition] -> String
showXConditions [] = ""
showXConditions cs = "[" ++ (showNXConditions cs) ++ "]"

showNXConditions :: [XCondition] -> String
showNXConditions [] = ""
showNXConditions (c:[]) = showXCondition c
showNXConditions (c:cs) = (showXCondition c) ++ ", " ++ (showNXConditions cs)

showXCondition :: XCondition -> String
showXCondition (a, b) = "@" ++ a ++ "='" ++ b ++ "'"

parserXRequest :: Parser XRequest
parserXRequest = ( do
				path <- parserPath;
				return (Path path); ) |||
				( do
				parserWord "/";
				return Rien; )

parserPath :: Parser XPath
parserPath = ( do
			 parserWord "/";
			 elem <- parserXElement;
			 next <- parserXRequest;
			 return (elem, next); ) |||
			 ( do
			 parserWord "/"
			 elem <- parserXElement;
			 return (elem, Rien); )

parserXElement :: Parser XElement
parserXElement = do
					parserSpace;
					elem <- parserTextUntil " \t\n@/";
					conditions <- parserXConditions;
					return (elem, conditions);

parserXConditions :: Parser [XCondition]
parserXConditions = ( do
					parserSpace;
					parserWord "[";
					condition1 <- parserXCondition;
					conditions <- zeroOuPlus ( do
									parserSpace;
									parserWord ",";
									condition <- parserXCondition;
									return condition; )
					parserSpace;
					parserWord "]";
					return (condition1:conditions); ) |||
					( do return []; )

parserXCondition :: Parser XCondition
parserXCondition = do	
					parserSpace;
					parserWord "@";
					parserSpace;
					nom <- parserTextUntil " \n\t=";
					parserSpace;
					parserWord "=";
					parserSpace;
					parserWord "'";
					val <- parserTextUntil "'";
					parserWord "'";
					return (nom, val);

checkXCondition :: XCondition -> [XmlAttribut] -> Bool
checkXCondition c [] = False
checkXCondition (cname, cvalue) ((name, value):as) = case cname == name of
							True -> case cvalue == value of
								True -> True
								False -> checkXCondition (cname, cvalue) as
							False -> checkXCondition (cname, cvalue) as

checkXConditions :: [XCondition] -> [XmlAttribut] -> Bool
checkXConditions [] as = True
checkXConditions cs [] = False
checkXConditions (c:cs) as = case checkXCondition c as of
								True -> checkXConditions cs as
								False -> False

--Anciennes fonctions fausses et mauvaises
--does_path_match_tree :: XElement -> XmlTree -> Bool
--does_path_match_tree (req_name, conditions) (name, attrs, elems) =
--				case ( req_name == name ) of
--					False -> False
--					True -> checkXConditions conditions attrs

--query_sub_trees :: XRequest -> [XmlElement] -> [XmlTree]
--query_sub_trees Rien trees = []
--query_sub_trees request [] = []
--query_sub_trees request ( Text text: ts ) = query_sub_trees request ts
--query_sub_trees (Path (elem, sub_request) ) ( Tree (nom, attrs, sub_trees) :ts) =
--				( case does_path_match_tree elem (nom, attrs, sub_trees) of -- Tentative d'ajout du noeud
--					True -> [(nom, attrs, sub_trees)]
--					False -> [] ) ++ query_sub_trees (Path (elem, sub_request)) ts

-- Nouvelle fonction propre

tryQuery :: XRequest -> XmlElement -> [XmlTree]
tryQuery Rien tree = []
tryQuery request (Text text) = []
tryQuery (Path ((path_name, conditions), request)) (Tree (node_name, attrs, sub_trees)) =
					case path_name == node_name of
						False -> []
						True -> case checkXConditions conditions attrs of
							False -> []
							True -> queryNode request (node_name, attrs, sub_trees)

tryQueries :: XRequest -> [XmlElement] -> [XmlTree]
tryQueries request [] = []
tryQueries request (t:ts) = (tryQuery request t) ++ (tryQueries request ts)

queryNode :: XRequest -> XmlTree -> [XmlTree]
queryNode Rien tree = [tree]
queryNode request (node_name, attrs, sub_trees) = tryQueries request sub_trees

query :: XRequest -> XmlTree -> [XmlTree]
query Rien tree = [tree]
query (Path ((path_name, conditions), request)) (node_name, attrs, sub_trees) =
					case path_name == node_name of
						False -> []
						True -> case checkXConditions conditions attrs of
							False -> []
							True -> queryNode request (node_name, attrs, sub_trees)


-- tests du projet

language_1, language_2 :: XmlAttribut
language_1 = ("language", "French")
language_2 = ("language", "English")

propriete_1 :: XmlAttribut
propriete_1 = ("test_invalide", "valeur_fausse")

exemple_1 :: XmlTree
exemple_1 = ("books", [], [
				Tree ("book", [language_1], [
					Tree ("title", [], [Text "Désert"]),
					Tree ("author", [], [Text "Le Clézio"])
				]),
				Tree ("book", [], [
					Tree ("title", [], [Text "Contes du XIXème siècle"]),
					Tree ("author", [], [Text "Flaubert"]),
					Tree ("author", [], [Text "Gautier"])
				])
			])

request_1, request_2, request_3 :: XRequest
request_1 = Path ( ("books", []) , Path ( ("book", []), Path ( ("author", []), Rien) ) )
request_2 = Path ( ("books", []) , Path ( ("book", [("language","French")]), Rien ) )
request_3 = Rien

condition_1, condition_2 :: XCondition
condition_1 = ("language", "French")
condition_2 = ("language", "English")
