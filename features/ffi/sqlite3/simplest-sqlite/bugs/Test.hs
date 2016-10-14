{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (Word)

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.Text (unpack, pack, split,Text)

import qualified Data.ByteString.Char8 as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.List
import Data.Char

import Foreign.C.String

import Database.SmplstSQLite3
import Database.SmplstSQLite3.Exception.Internal
import Database.SmplstSQLite3.Constants

instance Arbitrary T.ByteString where
    arbitrary   = fmap T.pack (listOf1 arbitrary)

newtype DoubleS = DoubleS Double
  deriving (Show,Eq)

instance Arbitrary DoubleS where
  arbitrary = do
    l <-  arbitrary
    return (DoubleS l)

newtype IntS = IntS Int
  deriving (Show,Eq)

instance Arbitrary IntS where
  arbitrary = do
    l <- arbitrary
    return (IntS l)

letterGen :: Gen Char
letterGen = suchThat arbitrary isLetter

newtype Word = Word String
  deriving (Show,Eq)

instance Arbitrary Word where
  arbitrary = do
    l <- listOf1 letterGen
    return (Word l)


main = quickCheckWithResult stdArgs {maxSuccess = 200000} prop_bind_test

prop_bind_test :: Word -> IntS -> DoubleS -> T.ByteString ->  Property
prop_bind_test (Word str) (IntS num) (DoubleS dou) bytStr = monadicIO $  do
    ans <- run $ withSQLite ":memory:" (\conn -> do
        withPrepared conn "create table a (num integer,str text,dou real,bytStr blob);" (\stmt -> step stmt)

        ( _ ,insError) <- withPrepared conn "insert into a (num,str,dou,bytStr) values (?1,?2,?3,?4);" (\stmt -> do
                              bind stmt "?1" (num :: Int)
                              bind stmt "?2" ((pack str) :: Text)
                              bind stmt "?3" (dou :: Double)
                              bind stmt "?4" (bytStr :: T.ByteString)
                              step stmt)

        (answer, _ ) <- withPrepared conn "select * from a;" (\stmt -> do
                           step stmt
                           value0 <- column stmt (0 :: Int)
                           value1 <- column stmt (1 :: Int)
                           value2 <- column stmt (2 :: Int)
                           value3 <- column stmt (3 :: Int)
                           return [(value0,value1,value2,value3)]) :: IO ([(Int,Text,Double,T.ByteString)], String)

        withPrepared conn "drop table a;" (\stmt -> step stmt)
        
        return answer)
    let [(a, b, c, d)] = ans
     
    monitor (
      whenFail'
      (putStrLn $ "--------- Database: " ++ " str: " ++ unpack b ++ " expected: " ++ str ++ " num: " ++ show a ++ " expected: " ++ show num ++ " dou: " ++ show c  ++ " expected: " ++ show dou  ++ " bytStr: " ++ T.unpack d ++ " expected: " ++ T.unpack bytStr))
    
    assert(a == num && b == (pack str) &&  c == dou)
    --assert(a == num && b == (pack str) &&  c == dou && d == bytStr)
    --assert(a == num &&  c == dou && d == bytStr)



main3 :: IO ()
main3 = do 
    let byteStr1 = (B.pack [89,191,210,187,17,34,0,87,101,68]) :: T.ByteString
    let byteStr2 = (B.pack [184,60,60,23,29,234,132,51,54,63,16,232,122,0,51]) :: T.ByteString 
    let byteStr3 = (B.pack [56,43,93,25,48,56,245,51,84]) :: T.ByteString

    ([a,b,c],_) <- withSQLite "test.db" (\db -> do
            withPrepared db ("create table abc (A Blob,B Blob,C Blob);") (\cc -> step cc)
            withPrepared db ("insert into abc (A,B,C) values (?1,?2,?3)") (\stmt -> do 
                                                                                    bind stmt "?1" byteStr1
                                                                                    bind stmt "?2" byteStr2 
                                                                                    bind stmt "?3" byteStr3 
                                                                                    step stmt)
            stmt2 <- withPrepared db ("select * from abc") (\dd -> do 
                                                        step dd
                                                        byt1 <- column dd (0 :: Int) :: IO T.ByteString
                                                        byt2 <- column dd (1 :: Int) :: IO T.ByteString
                                                        byt3 <- column dd (2 :: Int) :: IO T.ByteString
                                                        return [byt1,byt2,byt3]) :: IO ([T.ByteString], String)
            return stmt2)
    
    print $ a
    print $ byteStr1
    print $ b
    print $ byteStr2
    print $ c
    print $ byteStr3

    print  $ byteStr1 == a
    print  $ byteStr2 == b
    print  $ byteStr3 == c
    return ()




