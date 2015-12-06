module Parser(
              Parser(MkParser),
              caractere,
              parse,
			  zeroOuPlus,
			  unOuPlus,
			  showParsingError,
              (|||),
			  (!!!)
             )  
    where

    type Resultat a = Either (a, String) String
    newtype Parser a = MkParser (String -> Resultat a ) 

    echoue :: String -> Parser a 
    echoue str = MkParser (\_ -> Right str)

    retourne :: a -> Parser a
    retourne v = MkParser (\s -> Left (v, s))

    caractere :: Parser Char
    caractere = MkParser (\s -> case s of
                              ""     -> Right "Fin de la chaîne rencontree"
                              (c:cs) -> Left (c, cs))

    parse :: Parser a -> String -> Resultat a
    parse (MkParser p) = p

    (|||) :: Parser a -> Parser a -> Parser a
    p ||| p' = MkParser (\s -> case parse p s of
                                 Right error -> parse p' s
                                 r       -> r)

    (>>>) :: Parser a -> (a -> Parser b) -> Parser b
    p >>> pf = MkParser (\s -> case parse p s of
                                 Right error -> Right error
                                 Left (a, s') -> parse (pf a) s')

    (!!!) :: Parser a -> (String -> Parser a) -> Parser a
    p !!! pf = MkParser (\s -> case parse p s of
                                 Right error -> parse (pf error) s
                                 r           -> r)
		

    zeroOuPlus :: Parser a -> Parser [a]
    zeroOuPlus p = unOuPlus p ||| retourne []

    unOuPlus :: Parser a -> Parser [a]
    unOuPlus p = p				>>> \x ->
				 zeroOuPlus p 	>>> \xs ->
				 retourne (x:xs)

    showParsingError :: Resultat a -> String
    showParsingError (Left (a, s)) = "Pas d'erreur\n"
    showParsingError (Right error) = error ++ "\n"

    instance Monad Parser where
              (>>=) = (>>>)
              return = retourne
              fail str = echoue str

