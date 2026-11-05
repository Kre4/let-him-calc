{-# LANGUAGE InstanceSigs #-}

module Parser where

-- newtype ParseChain a = ParseChain (a, String)


newtype Parser a = Parser {parse :: String -> Either String (a, String)}

fmapChain :: (a -> b) -> ((a, String) -> (b, String))
fmapChain f (ini, str) = (f ini, str)   

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  -- вариант в лоб
  --   fmap f (Parser parseFunc) = Parser $ \s -> execResult (parseFunc s) f
  --     where
  --       execResult func mapping = case func of
  --         Left s -> Left s
  --         Right (val, str) -> Right (mapping val, str)

  -- вариант покроче
  --   fmap f (Parser parseFunc) = Parser $ \s ->
  --     case parseFunc s of
  --         Left s -> Left s
  --         Right (val, str) -> Right (f val, str)

  -- через functor either
  fmap f (Parser parseFunc) = Parser $ \input ->
    fmap (\(val, rest) -> (f val, rest)) (parseFunc input)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b

-- в лоб
--   (<*>) (Parser first) (Parser second) = Parser $ \input -> runSnd (first input)
--     where
--         runSnd (Left err) = Left err
--         runSnd (Right (mapping, rest)) = fmap (fmapChain mapping) (second rest)

-- через монаду 
  (Parser first) <*> (Parser second) = Parser $ \input -> do
    (mapping, rest)  <- first input
    (value, rest')   <- second rest
    pure (mapping value, rest')

{- или
first input >>= \(mapping, rest) ->
  second rest >>= \(value, rest') ->
    pure (mapping value, rest')
-}


instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b

  (Parser ini) >>= mapper = Parser $ \input -> do
    (val, rest) <- ini input
    parse (mapper val) rest


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input -> satisfyChar input
    where satisfyChar (x:xs) = if (f x) then Right (x, xs) else Left "unexpected character" 
          satisfyChar _ = Left "unexpected character" 

char :: Char -> Parser Char
char c = satisfy (== c) 

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser p1) (Parser p2) = Parser $ \input ->
    case p1 input of 
        Right ok -> Right ok
        Left _ -> p2 input


many :: Parser a -> Parser [a]
many parser = (parser >>= \parseResult -> 
        many parser >>= \parseRest ->
            pure (parseResult:parseRest)
    ) `orElse` pure [] 


string :: String -> Parser String
string [] = pure []
string (x:xs) = do
    c <- char x
    cs <- string xs
    pure (c: cs)