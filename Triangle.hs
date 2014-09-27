module Triangle (triangle) where
import Shortener

-- This function returns an ASCII triangle.
makeTriangle :: Int -> Int -> Int -> Int -> String
makeTriangle height counter indent width =
    if counter <= height
        then "\n" 
          ++ " " `repeatTimes` newIndent 
          ++ "`" `repeatTimes` width 
          ++ makeTriangle height incCounter incIndent incWidth
        else ""
      where
        newIndent  = doEitherIf (id) (subtract a) (height > 0) indent
        incIndent  = doEitherIf (+1) (subtract 1) (height < 0) indent
        incWidth   = doEitherIf (+2) (subtract 2) (height > 0) width
        incCounter = counter + 1
        -- Subtract for every time we're going to add to the indent.
        a          = abs(height) - 1

-- This function creates the triangle's variables.
triangle :: Int -> Int -> String
triangle height termWidth
    -- If the width would overflow, use the max that won't.
    | abs height > halfTermWidth = triangle newHeight termWidth
    | otherwise = makeTriangle height 1 halfTermWidth width
      where
        halfTermWidth = termWidth `div` 2
        width         = if heightInput > 0 then 1 else (-height) * 2 - 1
        newHeight     = doEitherIf (id) (negate) (height > 0) halfTermWidth
