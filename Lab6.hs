module Lab6 (main, DigitalTime (..)) where

main :: IO ()
main = do
    putStrLn "Hello, world!"

{--
    Add your functions for lab 6 below. Function and type signatures are 
    provided below, along with dummy return values.
    Add your code below each signature, but to not modify the types.
       
    Test your code by running 'cabal test' from the lab3 directory.
--}
    
data DigitalTime = DigitalTime (Int, Int, Int)

instance Show DigitalTime where
  show (DigitalTime (h, m, s)) =
    let show1 timeDigit = if timeDigit < 10 then "0" ++ show timeDigit else show timeDigit
     in "<" ++ show1 h ++ ":" ++ show1 m ++ ":" ++ show1 s ++ ">"

instance Eq DigitalTime where
  (==) (DigitalTime (hx, mx, sx)) (DigitalTime (hy, my, sy)) = and[hx == hy, mx == my, sx == sy]

instance Ord DigitalTime where
  compare (DigitalTime (hx, mx, sx)) (DigitalTime (hy, my, sy))
    | or [hx < hy, mx < my, sx < sy] = LT 
    | or [hx > hy, mx > my, sx > sy]  = GT
    | otherwise = EQ
    
instance Num DigitalTime where
  (DigitalTime (hx, mx, sx)) + (DigitalTime (hy, my, sy)) =
    let s = (sx + sy) `rem` 60
        m = ((mx + my) + ((sx + sy) `div` 60)) `rem` 60
        h = ((hx + hy) + ((mx + my) + ((sx + sy) `div` 60)) `div` 60) `rem` 12
    in DigitalTime (if h == 0 then 12 else h, m, s)

  (DigitalTime (hx, mx, sx)) - (DigitalTime (hy, my, sy)) =
    let
        s = (sx - sy) `mod` 60
        m = ((mx - my) + ((sx - sy) `div` 60)) `rem` 60
        h = ((hx - hy) + (mx - my + ((sx - sy) `div` 60)) `div` 60) `rem` 12
    in DigitalTime (if h == 0 then 12 else h, m, s)
  
  fromInteger sec =
    let s = fromInteger sec `rem` 60
        m = (fromInteger sec `div` 60) `rem` 60
        h = (fromInteger sec `div` 3600) `rem` 12
    in DigitalTime (if h == 0 then 12 else h, m, s)