module Triangle where
import Shortener

triangle :: Int -> String
triangle height = 
  unlines . map row $ [1 .. height]
  where
    width row = (row * 2) - 1
    padding row = ((width height) - (width row)) `div` 2

    rowStars x = repeatTimes (width x) "*"
    rowPad x = repeatTimes (padding x) " "

    row x = concat [rowPad x, rowStars x]
