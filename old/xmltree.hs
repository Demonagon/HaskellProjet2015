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
parserChars [] = MkParser (\_ -> Nothing)
parserChars xs = MkParser( \s -> case s of
							"" 		-> Nothing
							(c:cs)  -> if elem c xs then Just (c, cs) else Nothing )

parserNotChars :: String -> Parser Char
parserNotChars "" = MkParser ( \s -> case s of
							"" -> Nothing
							(c:cs) -> Just(c, cs) )
parserNotChars xs = MkParser( \s -> case s of
							"" 		-> Nothing
							(c:cs)  -> if elem c xs then Nothing else Just (c, cs) )


parserWord :: String -> Parser [Char]
parserWord [] = MkParser ( \cs -> Just([], cs) )
--parserWord (c:cs) = MkParser ( \ (x:xs) -> if 
parserWord (c:cs) = parserChars [c] >>= \r -> parserWord cs >>= \rs -> return (r:rs)

parserSpace :: Parser [Char]
parserSpace = zeroOuPlus ( parserChars " \n\t" )

parserTextUntil :: String -> Parser [Char]
parserTextUntil [] = MkParser( \cs -> Just(cs, []) )
parserTextUntil filtre = MkParser(
					\str -> case str of
						[] -> Just([], [])
						(c:cs) -> case parse (parserNotChars filtre) (c:cs) of
							Nothing -> Just([], (c:cs))
							Just(x, xs) -> ( case parse (parserTextUntil filtre) xs of
								Nothing -> Nothing -- ce cas n'arrive jamais dans la pratique
								Just(y, ys) -> Just(x:y, ys) )
									 )
parserNoeudElement :: Parser XmlElement
parserNoeudElement = parserNoeud >>= \noeud -> return (Tree noeud)

parserNoeud :: Parser XmlTree
parserNoeud = parserBorneOuvrante
			  >>= \(nom, attributs) -> parserSpace
			  >> zeroOuPlus parserElement
			  >>= \elements -> (parserBorneFermante nom)
			  >> return (nom, attributs, elements)

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
