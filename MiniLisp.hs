module MiniLisp where

import Parser

data LispAtom = Number Int | Symbol String deriving (Eq, Show)
data LispValue = List [LispValue] | Atom LispAtom deriving (Eq, Show)

ws :: Parser String
ws = many (orElse (char '\n') (char ' '))

between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p =  pThen pHd (pMap fst (andThen p pTl))

ident :: Parser String
ident = pMap f (andThen  alpha  (many  (orElse (orElse  alpha  digit)   (char '?')))) where f (a, l) = a:l

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = some (orElse (pThen sep p) p)

lisp :: Parser LispValue
lisp = Parser inner where
    inner "" = error "End of input"
    inner input = 
        case runParser lispAtom input of
            Success s rest -> Success (Atom s) rest
            Error err -> 
                case runParser lispList input of
                    Success s rest -> Success (List s) rest
                    Error err -> Error err


lispAtom :: Parser LispAtom
lispAtom = Parser inner where
    inner "" = error  "End of input"
    inner input = 
        case runParser number input of
            Success d rest-> Success (Number d) rest
            Error err -> 
                let
                f (a,l) = a:l
                in
                case runParser (pMap f (andThen  alpha  (many   (orElse  alpha  digit)))) input of
                    Success s rest -> Success (Symbol s) rest
                    Error err -> Error err
               



lispList :: Parser [LispValue] 
lispList = Parser inner where
    inner "" = error "End of input"
    inner input = 
        case runParser lisp input of
            _ -> 
                let 
                            convertList l =
                                case  l of
                                    (x:xs) -> 
                                        case runParser lispAtom x of 
                                             Success (Symbol s) r ->  (Atom (Symbol s)) : convertList xs 
                                             Success (Number n) r ->  (Atom (Number n)) : convertList xs 
                                             Error err -> []
                                    [] -> []
                in
                case runParser (between (char '(') (char ')') (sepBy ws ident)) input of
                    Success list rest -> 
                        
                            Success (convertList list) rest
                    Error  err -> if err == "Unexpected character '('" then Error err else
                        case runParser (between (char '(') (char ')') (sepBy ws number)) input of
                            Success list1 rest -> Success (convertList (map show list1)) rest
                            Error  err -> Error err
           
