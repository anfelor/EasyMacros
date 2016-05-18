{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module RegExp where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Set (Set)
import Data.Semigroup
import qualified Data.Set as Set
import Text.Parsec as P hiding (Empty)

data RegExp
  = Char (Set Char)    -- [a], [abc], [a-z]; matches a single character from the specified class
  | Alt RegExp RegExp  -- r1 | r2 (alternation); matches either r1 or r2
  | Seq RegExp RegExp  -- r1 r2 (concatenation); matches r1 followed by r2
  | Star RegExp        -- r* (Kleene star); matches r zero or more times
  | Empty              -- matches only the empty string
  | Void               -- matches nothing (always fails)
  | Var String         -- a variable holding another regexp (explained later)
  deriving Show
 
match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

nullable :: RegExp -> Bool
nullable (Char _)    = False
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Star _)    = True
nullable Empty       = True
nullable Void        = False
nullable (Var _)     = False
 
deriv :: RegExp -> Char -> RegExp
deriv (Char cs) c
  | c `Set.member` cs = Empty
  | otherwise         = Void
deriv (Alt r1 r2) c   = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2) c
  | nullable r1       = Alt (Seq (deriv r1 c) r2) (deriv r2 c)
  | otherwise         = Seq (deriv r1 c) r2
deriv (Star r) c      = deriv (Alt Empty (Seq r (Star r))) c
deriv Empty _         = Void
deriv Void _          = Void
deriv (Var _) _       = Void

regex :: QuasiQuoter
regex = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things ++ " are not handled by the regex quasiquoter."
 
compile :: String -> Q Exp
compile s =
  case P.parse regexParser "" s of
    Left  err    -> fail (show err)
    Right regexp -> [e| regexp |]
    
regexParser :: Parsec String () RegExp
regexParser = alts <* eof where
  atom       = try var <|> char
  var        = Var <$> (string "${" *> many1 (noneOf "}") <* P.char '}')
  char       = charclass <|> singlechar
  singlechar = (Char . Set.singleton) <$> noneOf specials
  charclass  = fmap (Char . Set.fromList) $
                 P.char '[' *> content <* P.char ']'
  content    = try (concat <$> many1 range)
                 <|> many1 (noneOf specials)
  range      = enumFromTo
                 <$> (noneOf specials <* P.char '-')
                 <*> noneOf specials
  alts       = try (Alt <$> seqs <*> (P.char '|' *> alts)) <|> seqs
  seqs       = try (Seq <$> star <*> seqs) <|> star
  star       = try (Star <$> (atom <* P.char '*'))
                 <|> try (Star <$> (P.char '(' *> alts <* string ")*"))
                 <|> atom
  specials   = "[]()*|"
 
instance Lift a => Lift (Set a) where
    lift set = appE (varE 'Set.fromList) (lift (Set.toList set))

instance Lift RegExp where
    -- lift :: RegExp -> Q Exp
    lift (Char cs)     = apply 'Char  [lift cs]
    lift (Alt r1 r2)   = apply 'Alt   (map lift [r1, r2])
    lift (Seq r1 r2)   = apply 'Seq   (map lift [r1, r2])
    lift (Star r1)     = apply 'Star  (map lift [r1])
    lift Empty         = apply 'Empty []
    lift Void          = apply 'Void  []
    lift (Var vars)    = foldl1 appE $ map (varE . mkName) (words vars)

instance Semigroup RegExp where
    r1 <> r2 = Seq r1 r2

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)
      
