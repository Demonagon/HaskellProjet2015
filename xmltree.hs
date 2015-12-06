import Parser
import System.IO
import System.Environment

-- || MAIN

main :: IO()
main = do
			args <- getArgs
			if (length args) < 2
				then putStrLn "Arguments : [query | -f file_query] file_tree"
				else do
				if ( ( args !! 0 ) == "-f" )
					then do
						if (length args) < 3
							then putStrLn "Arguments : [query | -f file_query] file_tree"
							else do
								request <- getAllFile (args !! 1)
								tree <- getAllFile (args !! 2)
								results <- executeRequest request tree
								readResults results
					else do
								tree <- getAllFile (args !! 1)
								results <- executeRequest (args !! 0) tree
								readResults results
							
			return ()

getAllFile :: String -> IO String
getAllFile path = do
				--putStrLn ("Opening " ++ path ++ "...")
				handle <- openFile path ReadMode
				content <- hGetContents handle
				return content;

executeRequest :: String -> String -> IO [XmlTree]
executeRequest request_str tree_str =
					do
						mb_request <- executeParseXRequest request_str
						case mb_request of
							Right error -> do putStrLn error; return [];
							Left request -> do
								mb_tree <- executeParseXmlTree tree_str
								case mb_tree of
									Right error -> do putStrLn error; return [];
									Left tree -> return (query request tree)

readResults :: [XmlTree] -> IO ()
readResults [] = putStrLn "No results !"
readResults ts = putStrLn (showXQueryResult ts)


type ParserResult a = Either a String

executeParseXRequest :: String -> IO (ParserResult XRequest)
executeParseXRequest str = return (parseJustXRequest str)

parseJustXRequest :: String -> ParserResult XRequest
parseJustXRequest str =
				case result of
					Left (request, "") -> Left request
					Left (request, cs) -> Right ("Parsing query error : \"" ++ cs ++ "\" couldn't be parsed")
					Right error -> Right error
					where result = parse (
									do
										r <- parserXRequest
										parserSpace
										return r
										) str

executeParseXmlTree :: String -> IO (ParserResult XmlTree)
executeParseXmlTree str = return (parseJustXmlTree str)

parseJustXmlTree :: String -> ParserResult XmlTree
parseJustXmlTree str =
				case result of
					Left (request, "") -> Left request
					Left (request, cs) -> Right ("Parsing tree error : \"" ++ cs ++ "\" couldn't be parsed")
					Right error -> Right error
					where result = parse (
									do
										r <- parserTree
										parserSpace
										return r
										) str
					

-- || XMLTREE

type XmlText = String
type XmlNom = String
type XmlAttribut = (String, String)

data XmlElement = Tree XmlTree | Text XmlText
			
type XmlTree = (XmlNom, [XmlAttribut], [XmlElement])

showXmlText :: XmlText -> String
showXmlText text = text

showXmlNom :: XmlNom -> String
showXmlNom nom = nom

showXmlAttribut :: XmlAttribut -> String
showXmlAttribut (space, value) = space ++ "=\"" ++ value ++ "\""

showXmlAttributs :: [XmlAttribut] -> String
showXmlAttributs [] = ""
showXmlAttributs (x:[]) = " " ++ showXmlAttribut x
showXmlAttributs (x:xs) = " " ++ showXmlAttribut x ++ "," ++ showXmlAttributs xs

showXmlElement :: XmlElement -> String
showXmlElement (Tree (name, attributs, nodes) ) =
			"<" ++ showXmlNom name ++ showXmlAttributs attributs ++ ">\n"
			++ showXmlElements nodes
			++ "</" ++ showXmlNom name ++ ">\n"
showXmlElement (Text text) = text

instance Show XmlElement where
		show = showXmlElement

showXmlElements :: [XmlElement] -> String
showXmlElements [] = ""
showXmlElements (x:xs) = showXmlElement x ++ showXmlElements xs

showXmlTree :: XmlTree -> String
showXmlTree tree = showXmlElement (Tree tree) ++ "\n"

parserChars :: String -> Parser Char
parserChars [] = MkParser (\_ -> Right "Impossible de filtrer avec un filtre vide")
parserChars xs = MkParser( \s -> case s of
							"" 		-> Right ("Tentative de filtrer " ++ xs ++ " sur une chaine vide")
							(c:cs)  -> if elem c xs
									   then Left (c, cs)
									   else Right ("\"" ++
												  [c] ++
												  "\" trouve en l'attente d'un parmis \"" ++
												  xs ++ "\"") )

parserCharsIn :: String -> Parser [Char]
parserCharsIn str = zeroOuPlus ( parserChars str )

parserNotChars :: String -> Parser Char
parserNotChars "" = MkParser ( \s -> case s of
	"" -> Right "Chaîne vide rencontree"
	(c:cs) -> Left (c, cs) )

parserNotChars xs = MkParser( \s -> case s of
							"" 		-> Right ("Tentative de filtrer non " ++ xs ++ " sur une chaine vide")
							(c:cs)  -> if elem c xs
									  then Right ("\"" ++
												  [c] ++
												  "\" trouve en l'attente d'autre chose que \"" ++
												  xs ++ "\"")
									  else Left (c, cs) )


parserWord :: String -> Parser [Char]
parserWord [] = MkParser ( \cs -> Left ([], cs) )
parserWord (c:cs) = do
						r <- parserChars [c];
						rs <- parserWord cs;
						return (r:rs);
					

parserSpace :: Parser [Char]
parserSpace = zeroOuPlus ( parserChars " \n\r\t" )

parserSpacedWord :: String -> Parser [Char]
parserSpacedWord str = do
					parserSpace;
					str <- parserWord str;
					return str;

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
parserNoeudElement = do
						noeud <- parserNoeud;
						return (Tree noeud);

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
parserBorneOuvrante = do
						parserChars "<";
						nom <- parserNom;
					  	parserSpace;
						listeAttribut <- parserListeAttributs;
						parserChars ">";
						return (nom, listeAttribut);

parserListeAttributs :: Parser [XmlAttribut]
parserListeAttributs = zeroOuPlus
						(parserSpace >> parserAttribut)

parserAttribut :: Parser XmlAttribut
parserAttribut = do
					domaine <- parserNom;
					parserSpace;
					parserWord "=\"";
					valeur <- parserTextUntil "\"";
					parserChars "\"";
					return (domaine, valeur);

parserTextElement :: Parser XmlElement
parserTextElement = do
						text <- parserText;
						return (Text (strRemoveSpace text));

strRemoveSpace :: String -> String
strRemoveSpace "" = ""
strRemoveSpace str = case elem (last str) " \n\r\t" of
						False -> str
						True -> strRemoveSpace (init str)

parserText :: Parser [Char]
parserText = do
				parserSpace;
				text <- parserTextUntil "<";
				case text of
					"" -> fail ""
					cs -> return text;

parserNom :: Parser [Char]
parserNom = parserTextUntil "=<> \"\t\n"

parserBorneFermante :: String -> Parser XmlNom
parserBorneFermante nom = do
							parserSpace;
							parserWord "</";
							parserWord nom;
							parserSpace;
							parserChars ">";
							return nom;

-- || XPATH

data XRequest = Rien | Path XPath
type XPath = (XElement, XRequest)
type XElement = (String, [XCondition])
data XCondition = XAttr (String, String) | XStr (String, String) | XSibling Integer

instance Show XRequest where
		show = showXRequest

instance Show XCondition where
		show = showXCondition

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
showXCondition (XAttr (a, b)) = "@" ++ a ++ "='" ++ b ++ "'"
showXCondition (XStr (a, b)) = a ++ "='" ++ b ++ "'"
showXCondition (XSibling n) = show n

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
					elem <- parserTextUntil " \t\n[@/";
					conditions <- parserXConditions;
					return (elem, conditions);

parserXConditions :: Parser [XCondition]
parserXConditions = ( do
					parserSpacedWord "[";
					parserSpace;
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
					parserSpacedWord "@";
					parserSpace;
					nom <- parserTextUntil " \n\t=";
					case nom of
						"" -> fail "Erreur : nom d'attribut vide"
						cs -> parserSpacedWord "=";
					parserSpacedWord "'";
					val <- parserTextUntil "'";
					parserWord "'";
					return (XAttr (nom, val));
                   |||
                   do
					parserSpace;
					nom <- parserTextUntil " \n\t=";
					case nom of
						"" -> fail "Erreur : nom de condition vide"
						cs -> parserSpacedWord "=";
					parserSpacedWord "'";
					val <- parserTextUntil "'";
					parserWord "'";
					return (XStr (nom, val));
                   |||
                   do
					parserSpace;
					nbStr <- parserCharsIn "1234567890"
					return (XSibling (read nbStr));
					

checkXAttrCondition :: (String, String) -> [XmlAttribut] -> Bool
checkXAttrCondition c [] = False
checkXAttrCondition (cname, cvalue) ((name, value):as) = case cname == name of
							True -> case cvalue == value of
								True -> True
								False -> checkXAttrCondition (cname, cvalue) as
							False -> checkXAttrCondition (cname, cvalue) as

getElementStrValue :: XmlElement -> String
getElementStrValue (Text text) = text
getElementStrValue (Tree (_, _, es)) = getStrValue es

getStrValue :: [XmlElement] -> String
getStrValue [] = ""
getStrValue (e:es) = (getElementStrValue e) ++ (getStrValue es)

checkForStrValue :: (String, String) -> [XmlElement] -> Bool
checkForStrValue _ [] = False
checkForStrValue strcondition ((Text text):es) = checkForStrValue strcondition es
checkForStrValue (name, value) ((Tree (node_name, attrs, sub_tree):es)) =
								case node_name == name of
									False -> checkForStrValue (name, value) es
									True -> case getStrValue sub_tree == value of
										True -> True
										False -> checkForStrValue (name, value) es

checkXStrCondition :: (String, String) -> XmlTree -> Bool
checkXStrCondition (name, value) (node_name, attrs, sub_tree) =
				checkForStrValue (name, value) sub_tree
-- case cname == name of
--							True -> case cvalue == value of
--								True -> True
--								False -> checkXCondition (XAttr (cname, cvalue)) as
--							False -> checkXCondition (XAttr (cname, cvalue)) as

-- Arguments : numéro du noeud recherché, noeud à l'essais, numéro absolu de ce noeud, arbre père
-- Processus : on sait que l'on recherche un noeud dont le nom est le même que t.
-- t est le n-ième noeud de father.
-- La condition est valide si t est également le n-ième noeud de father à porter ce nom.
checkXSiblingCondition :: Integer -> XmlTree -> Integer -> XmlTree -> Bool
checkXSiblingCondition _ _ _ (_, _, []) = False
checkXSiblingCondition sib (t_name, t_attrs, t_sub) n (f_name, f_attrs, ((Text text):sons)) =
	checkXSiblingCondition sib (t_name, t_attrs, t_sub) (n - 1) (f_name, f_attrs, sons)
checkXSiblingCondition 	sib
						(t_name, t_attrs, t_sub)
						n
						(f_name, f_attrs, ( (Tree (s_name, s_attrs, s_sub)):sons) ) =
	case s_name == t_name of
		True -> case sib == 1 of
					True -> n == 1
					False -> checkXSiblingCondition (sib - 1) (t_name, t_attrs, t_sub) (n - 1) (f_name, f_attrs, sons)
		False -> checkXSiblingCondition sib (t_name, t_attrs, t_sub) (n - 1) (f_name, f_attrs, sons)

checkXCondition :: XCondition -> XmlElement -> Integer -> XmlTree -> Bool
checkXCondition _ (Text text) _ _ = False
checkXCondition (XAttr (name, value)) (Tree (_, attrs, _)) _ _ =
				checkXAttrCondition (name, value) attrs
checkXCondition (XStr (name, value)) (Tree tree) _ _ =
				checkXStrCondition (name, value) tree
checkXCondition (XSibling i) (Tree tree) num father =
				checkXSiblingCondition i tree num father
					
						

-- Arguments : conditions à tester, élément sur lequel tester, numéro de l'élément, noeud père, résultat
checkXConditions :: [XCondition] -> XmlElement -> Integer -> XmlTree -> Bool
checkXConditions [] e n f = True
--checkXConditions cs [] = False
checkXConditions (c:cs) e n father = case checkXCondition c e n father of
								True -> checkXConditions cs e n father
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

-- Arguments : requête, élément à tester, numéro de l'élément, arbre parent, résultat
tryQuery :: XRequest -> XmlElement -> Integer -> XmlTree -> [XmlTree]
tryQuery Rien tree n father = []
tryQuery request (Text text) n father = []
tryQuery (Path ((path_name, conditions), request)) (Tree (node_name, attrs, sub_trees)) n father =
					case path_name == node_name of
						False -> []
						True -> case checkXConditions conditions (Tree (node_name, attrs, sub_trees)) n father of
							False -> []
							True -> queryNode request (node_name, attrs, sub_trees)

tryQueries :: XRequest -> [XmlElement] -> Integer -> XmlTree -> [XmlTree]
tryQueries request [] n father = []
tryQueries request (t:ts) n father = (tryQuery request t n father) ++ (tryQueries request ts (n + 1) father)

queryNode :: XRequest -> XmlTree -> [XmlTree]
queryNode Rien tree = [tree]
queryNode request (node_name, attrs, sub_trees) = tryQueries request sub_trees 1 (node_name, attrs, sub_trees)

query :: XRequest -> XmlTree -> [XmlTree]
query Rien tree = [tree]
query (Path ((path_name, conditions), request)) (node_name, attrs, sub_trees) =
					case path_name == node_name of
						False -> []
						True -> case checkXConditions conditions (Tree (node_name, attrs, sub_trees)) 1 ("", [], []) of
							False -> []
							True -> queryNode request (node_name, attrs, sub_trees)

showXQueryResult :: [XmlTree] -> String
showXQueryResult [] = ""
showXQueryResult (t:ts) = "-> \n" ++ (showXmlTree t) ++ "\n" ++ (showXQueryResult ts)

-- || TESTS

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

request_11, request_12, request_13, request_14, request_15, request_16 :: XRequest
request_11 = Path ( ("books", []) , Path ( ("book", []), Path ( ("author", []), Rien) ) )
request_12 = Path ( ("books", []) , Path ( ("book", [XAttr ("language","French")]), Rien ) )
request_13 = Rien
request_14 = Path ( ("books", []) , Path ( ("book", []), Path ( ("author", [XSibling 1]), Rien) ) )
request_15 = Path ( ("books", []) , Path ( ("book", []), Path ( ("author", [XSibling 2]), Rien) ) )
request_16 = Path ( ("books", []) , Path ( ("book", [XStr ("title", "Désert")]), Rien ) )

exemple_2 :: XmlTree
exemple_2 = ("fruits", [], [
				Tree ("fruit", [("nom", "Banane")], [
					Tree ("vitamine", [], [Text "A"]),
					Tree ("vitamine", [], [Text "B"])
				]),
				Tree ("fruit", [("nom", "Pomme")], [
					Tree ("vitamine", [], [Text "A"]),
					Tree ("vitamine", [], [Text "Z"]),
					Tree ("vitamine", [], [Text "B"]),
					Tree ("vitamine", [], [Text "D"])
				]),
				Tree ("fruit", [("nom", "Orange")], [
					Tree ("vitamine", [], [Text "C"]),
					Tree ("vitamine", [], [Text "F"]),
					Tree ("vitamine", [], [Text "Z"])
				])
			])

request_21, request_22, request_23, request_24, request_25, request_26 :: XRequest
request_21 = Path ( ("fruits", []) , Path ( ("fruit", []), Rien ) )
request_22 = Path ( ("fruits", []) , Path ( ("fruit", [XSibling 2]), Rien ) )
request_23 = Path ( ("fruits", []) , Path ( ("fruit", []), Path ( ("vitamine", [XSibling 2]), Rien ) ) )
request_24 = Path ( ("fruits", []) , Path ( ("fruit", [XAttr ("nom", "Pomme")]), Rien ) )
request_25 = Path ( ("fruits", []) , Path ( ("fruit", []), Path ( ("vitamine", []), Rien ) ) )
request_26 = Path ( ("fruits", []) , Path ( ("fruit", [XStr ("vitamine", "A"), XStr ("vitamine", "Z")]), Rien ) )

condition_1, condition_2 :: XCondition
condition_1 = XAttr ("language", "French")
condition_2 = XAttr ("language", "English")

sub_tree_1 :: XmlTree
sub_tree_1 = ("book", [], [
					Tree ("title", [], [Text "Contes du XIXème siècle"]),
					Tree ("author", [], [Text "Flaubert"]),
					Tree ("author", [], [Text "Gautier"])
				])

sub_sub_tree_1 :: XmlTree
sub_sub_tree_1 = ("author", [], [Text "Flaubert"])
