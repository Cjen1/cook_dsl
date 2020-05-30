{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

import Text.Printf
import Data.List


type Quant = Float
data Step 
class Sym repr where
  quant :: Float -> repr Quant
  ing :: String -> repr Quant -> repr Step
  mix :: [repr Step] -> repr Step
  proc :: String -> [repr Step] -> repr Step

newtype PP a = PP{unPP :: String}
instance Sym PP where
  quant f   = PP $ printf "%.0fg" f
  ing s q   = PP $ printf "%s of %s" (unPP q) s
  mix ss    = PP $ printf "mix %s" (intercalate ", " (map unPP ss))
  proc s ss = PP $ printf "%s\n%s" (intercalate "\n" (map unPP ss)) s
pp e = unPP e

newtype C a = C{unC :: Int}
instance Sym C where
  quant f = C 0
  ing s q = C 1
  mix ss = C $ sum $ map unC ss
  proc s rs = C $ sum $ map unC rs
concurrent_lines x = unC x

newtype W repr a = W{unW :: Float -> repr a}
instance Sym r => Sym (W r)where
  quant f   = W $ \w -> quant $ f * w
  ing s q   = W $ \w -> ing s $ unW q w
  mix ss    = W $ \w -> mix $ map (\x -> unW x w) ss
  proc s rs = W $ \w -> proc s $ map (\x -> unW x w) rs 
reweigh x = unW x

newtype D r r' a = D{unD :: (r a, r' a)}
instance (Sym r, Sym r')=> Sym (D r r')where
  quant f   = D $ (quant f, quant f)
  ing s q   = D $ (ing s (unD q), ing s (unD q))
  mix ss    = D $ (mix (unD ss), mix (unD ss))
  proc s rs = D $ (proc s (unD rs), proc s (unD rs))
duplicate x = unD x

newtype T a = T{unT :: Float}
instance Sym T where
  quant f   = T $ f
  ing s q   = T $ unT q
  mix ss    = T $ sum $ map unT ss
  proc s rs = T $ sum $ map unT rs
total_weight e = unT e

make_up_to t w = 
 let total = total_weight t in
 let coef  = total/w in
 reweigh t coef

(>|=) x g = g [x]


newtype H a = H{unH :: (string * int * int)}
instance Sym H where
  quant f = H $ printf "%.0fg" f, 1, 1
  ing s q = H $ printf "<td>%s %s<td>" s (unH) q
  mix ss  = H $ printf 

compile_to_html t = 
  let html, width = unH t in
  printf "<table>%s</table>" html

t1 = 
  mix [
  	(ing "yeast" $ quant 11),
	(ing "salt"  $ quant 22),
  	(ing "flour" $ quant 1000),
  	(ing "water" $ quant 666)
  ] 
  >|= proc "leave to rise"
  >|= proc "cook in the oven at 180C until internal temperature 87C"

t2 = make_up_to t1 1500

main = do 
  putStrLn $ printf "%s" s1
  putStrLn $ printf "total weight t1: %f" s2
  putStrLn $ printf "concurrent lines: %d" s3
  putStrLn $ printf "total weight t2: %f" s4
  where 
    s1 = pp t1
    s2 = total_weight t1
    s3 = concurrent_lines t1
    s4 = total_weight t2

