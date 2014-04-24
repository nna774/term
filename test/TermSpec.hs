module TermSpec where

import Testing.Term

import Data.Maybe
import Control.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

instance Arbitrary Term where
    arbitrary = do
      xs <- elements $ O : map Val ['0'..'9']
      ss <- elements $ take 10 $ iterate S xs
      ss' <- elements $ take 5 $ iterate S xs
      pm <- elements $ [Plus, Mul]
      pm <$> return ss <*> return ss'
--      return O

instance Arbitrary Form where
    arbitrary = do
      elements $ [Cont
                 , Not Cont
                 , Or  Cont Cont
                 , And Cont Cont
                 , Imp Cont Cont
                 -- , ForAll 'a' Cont
                 -- , Exist  'a' Cont
                 ]
--      return Cont

spec :: Spec
spec = do
     describe "toInt" $ do
         it "toInt SSSO" $
            fromJust (toInt (S (S (S O)))) `shouldBe` 3
         it "toInt (Val 'a')" $
            toInt (Val 'a') `shouldBe` Nothing
         prop "id x == fromJust . toInt . intToTerm x in 0 <= x <= 10000" $
              \x -> (x < 0) || (x > 10000) || fromJust (toInt (intToTerm x)) == x
         prop "result of newFreeValT not in containFreeValsT" $
              \t -> not $ newFreeValT t `elem` containFreeValsT t

         prop "prove (Not f) == not (prove f)" $
              \f -> prove (Not f) == not (prove f)
         prop "applyToBindT (newFreeValT x) t == t" $
              \(x, t) -> applyToBindT (newFreeValT t) x t == t
         it "applyToBindT 'a' (Plus O O) (Val 'a') == (Plus O O)" $
            applyToBindT 'a' (Plus O O) (Val 'a') `shouldBe` (Plus O O)
         it "applyToBindT 'a' (Plus O O) (Val 'b') == (Val 'b')" $
            applyToBindT 'a' (Plus O O) (Val 'b') `shouldBe` (Val 'b')

         it "not $ hasFreeVal (Plus O (S O))" $
            not $ hasFreeVal (Plus O (S O))
         it "prove Equal (S O) (Plus O (S O))" $
            prove $ Equal (S O) (Plus O (S O))
         it "prove (O <=. (S O)) == True" $
            prove (O <=. (S O)) == True
         it "prove ((S O) <=. (S O)) == True" $
            prove ((S O) <=. (S O)) == True
         it "prove ((S (S O)) <=. (S O)) /= True" $
            prove ((S (S O)) <=. (S O)) /= True







