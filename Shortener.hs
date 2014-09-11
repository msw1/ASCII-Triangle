module Shortener (repeatTimes, doEitherIf) where

repeatTimes :: String -> Int -> String
repeatTimes s n = concat $ replicate n s

doEitherIf :: (a -> a) -> (a -> a) -> Bool -> a -> a
doEitherIf f1 f2 b n = if b then f1 n else f2 n
