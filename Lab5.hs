module Lab5 (main, third_last, every_other, is_cyclops, domino_cycle, domino_cycle_helper, tukeys_ninther, tukeys_ninther_helper) where
import Data.List

main :: IO ()
main = do
    putStrLn "Hello, world!"

{--
    Add your functions for lab 3 below. Function and type signatures are provided below, along with dummy return values. Add your code below each signature, but do not modify the types. Test your code by running 'cabal test' from the lab3 directory.
--}
    
third_last :: [a] -> a
third_last list | length(list) == 3 = head list| otherwise = third_last(tail list)

every_other :: [a] -> [a]
every_other [] = [] 
every_other list = let (hd, tl) = splitAt 1 list in hd ++ every_other (drop 2 list)

is_cyclops :: Int -> Bool
is_cyclops 0 = True
is_cyclops n = do
    let num = show n
    if (length num) == 1 then False
    else if rem (length num) 2 /= 1 then False
    else if last num == '0' then False
    else if head num == '0' then False
    else is_cyclops (read (init(tail num)) :: Int)

domino_cycle :: [(Int, Int)] -> Bool
domino_cycle tiles = False

tukeys_ninther :: (Ord a, Num a) => [a] -> a
tukeys_ninther items = head items
