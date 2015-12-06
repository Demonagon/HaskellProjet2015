module Parser(
              Parser(MkParser),
              caractere,
              parse,
			  zeroOuPlus,
			  unOuPlus,
              (|||)
             )  
    where

    type Resultat a = Maybe (a, String)
    newtype Parser a = MkParser (String -> Resultat a) 

    echoue :: Parser a 
    echoue = MkParser (\_ -> Nothing)

    retourne :: a -> Parser a
    retourne v = MkParser (\s -> Just (v, s))

    caractere :: Parser Char
    caractere = MkParser (\s -> case s of
                              ""     -> Nothing
                              (c:cs) -> Just (c, cs))

    parse :: Parser a -> String -> Resultat a
    parse (MkParser p) = p

    (|||) :: Parser a -> Parser a -> Parser a
    p ||| p' = MkParser (\s -> case parse p s of
                                 Nothing -> parse p' s
                                 r       -> r)

    (>>>) :: Parser a -> (a -> Parser b) -> Parser b
    p >>> pf = MkParser (\s -> case parse p s of
                                 Nothing      -> Nothing
                                 Just (a, s') -> parse (pf a) s')

    zeroOuPlus :: Parser a -> Parser [a]
    zeroOuPlus p = unOuPlus p ||| retourne []

    unOuPlus :: Parser a -> Parser [a]
    unOuPlus p = p				>>> \x ->
				 zeroOuPlus p 	>>> \xs ->
				 retourne (x:xs)

    instance Monad Parser where
              (>>=) = (>>>)
              return = retourne
              fail _ = echoue 

