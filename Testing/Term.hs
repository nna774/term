module Testing.Term where

import Control.Applicative
import Data.Maybe

data Term = O | S Term | Plus Term Term | Mul Term Term | Val Char deriving (Show, Eq)

data Form = Equal Term Term
          | Not Form
          | Or  Form Form
          | And Form Form
          | Imp Form Form
          | ForAll Char Form
          | Exist  Char Form
          | ForAllBounded Term Char Form
          | ExistBounded  Term Char Form
          | Cont deriving (Show, Eq)

normalize :: Term -> Term
normalize O = O
normalize (S t) = S $ normalize t
normalize p@(Plus t0 t1) = if not $ hasFreeVal p
                           then intToTerm (fromJust (toInt t0) + fromJust (toInt t1))
                           else Plus (normalize t0) (normalize t1)
normalize m@(Mul  t0 t1) = if not $ hasFreeVal m
                           then intToTerm (fromJust (toInt t0) * fromJust (toInt t1))
                           else Mul  (normalize t0) (normalize t1)
normalize v@(Val _) = v

toInt :: Term -> Maybe Int
toInt O = Just 0
toInt (S t) = (+1) <$> toInt t
toInt (Plus t0 t1) = (+) <$> toInt t0 <*> toInt t1
toInt (Mul t0 t1) = (*) <$> toInt t0 <*> toInt t1
toInt (Val _) = Nothing

toInt' :: Term -> Int
toInt' = fromJust . toInt

intToTerm :: Int -> Term
intToTerm 0 = O
intToTerm n | n > 0 = S $ intToTerm (n - 1)
            | otherwise =  error "intToTerm: Negative arg"

prove :: Form -> Bool
prove (Equal t0 t1) = normalize t0 == normalize t1 -- maybe danger
prove (Not f) = not $ prove f
prove (Or  f0 f1) = prove f0 || prove f1
prove (And f0 f1) = prove f0 && prove f1
prove (Imp f0 f1) = not (prove f0) || prove f1
prove (ForAll c f) = and $ map undefined $ iterate S O
prove (Exist  c f) = undefined
prove (ForAllBounded b c f) = if isVal b
                              then error "prove(ForAllBounded): Free Val"
                              else and $ map (prove.flip (applyToBindF c) f. intToTerm) [0..toInt' b]
prove (ExistBounded b c f) = if isVal b
                              then error "prove(ExistBounded): Free Val"
                              else or $ map (prove.flip (applyToBindF c) f. intToTerm) [0..toInt' b]
prove Cont = False

isVal :: Term -> Bool
isVal (Val _) = True
isVal _ = False

applyToBindT :: Char -> Term -> Term -> Term
applyToBindT _ _ O = O
applyToBindT c t0 (S t1) = S $ applyToBindT c t0 t1
applyToBindT c t0 (Plus t1 t2) = Plus (applyToBindT c t0 t1) (applyToBindT c t0 t2)
applyToBindT c t0 (Mul t1 t2) = Mul (applyToBindT c t0 t1) (applyToBindT c t0 t2)
applyToBindT c t0 v@(Val vc) = if c == vc then t0 else v

applyToBindF :: Char -> Term -> Form -> Form
applyToBindF c t (Equal t0 t1) = Equal (applyToBindT c t t0) (applyToBindT c t t1) 
applyToBindF c t (Not f) = Not $ applyToBindF c t f
applyToBindF c t (Or  f0 f1) = Or  (applyToBindF c t f0) (applyToBindF c t f1) 
applyToBindF c t (And f0 f1) = And (applyToBindF c t f0) (applyToBindF c t f1) 
applyToBindF c t (Imp f0 f1) = Imp (applyToBindF c t f0) (applyToBindF c t f1)
applyToBindF c t (ForAllBounded t0 c0 f0) = ForAllBounded t0 c0 (applyToBindF c t f0)
applyToBindF c t (ExistBounded t0 c0 f0) = ExistBounded t0 c0 (applyToBindF c t f0)
applyToBindF _ _ Cont = Cont

getForm :: Form -> Form
getForm (ForAll _ f) = f
getForm (ForAllBounded _ _ f) = f
getForm (Exist _ f) = f
getForm (ExistBounded _ _ f) = f

hasFreeVal :: Term -> Bool
hasFreeVal O = False
hasFreeVal (S t) = hasFreeVal t
hasFreeVal (Plus t0 t1) = hasFreeVal t0 || hasFreeVal t1
hasFreeVal (Mul  t0 t1) = hasFreeVal t0 || hasFreeVal t1
hasFreeVal (Val _) = True

containFreeValsT :: Term -> [Char]
containFreeValsT O = []
containFreeValsT (S t) = containFreeValsT t
containFreeValsT (Plus t0 t1) = containFreeValsT t0 ++ containFreeValsT t1
containFreeValsT (Mul  t0 t1) = containFreeValsT t0 ++ containFreeValsT t1
containFreeValsT (Val c) = [c]

newFreeValT :: Term -> Char
newFreeValT O = 'a'
newFreeValT (S t) = newFreeValT t
newFreeValT (Plus t0 t1) = max (newFreeValT t0) (newFreeValT t1)
newFreeValT (Mul  t0 t1) = max (newFreeValT t0) (newFreeValT t1)
newFreeValT (Val c) = succ c

newFreeValF :: Form -> Char
newFreeValF = undefined

(<=.) :: Term -> Term -> Form 
x <=. y = ExistBounded y c (Equal y (Plus x (Val c))) -- x \leq y := \exist z. x + z = y
    where c = newFreeValT (Plus x y) -- x にもy にも含まれない変数
