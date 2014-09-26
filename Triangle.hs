module Triangle (triangle) where
import Shortener

-- This function returns an ASCII triangle.
printTriangle :: Int -> Int -> Int -> Int -> String
printTriangle heightInput heightCounter amountToIndent triangleWidth =
    if heightCounter <= heightInput
        then "\n" 
          ++ " " `repeatTimes` newIndent 
          ++ "`" `repeatTimes` triangleWidth 
          ++ printTriangle heightInput incCounter incIndent incWidth
        else ""
      where
        newIndent  = doEitherIf (id) (subtract final) (heightInput > 0) amountToIndent
        incIndent  = doEitherIf (+1) (subtract 1)     (heightInput < 0) amountToIndent
        incWidth   = doEitherIf (+2) (subtract 2)     (heightInput > 0) triangleWidth
        incCounter = heightCounter + 1
        -- Subtract for every time we're going to add to the indent.
        final      = abs(heightInput) - 1

-- This function creates the triangle's variables.
triangle :: Int -> Int -> String
triangle heightInput termWidth
    -- If the width would overflow, use the max that won't.
    | abs heightInput > halfTermWidth = triangle newHeight termWidth
    | otherwise = printTriangle heightInput 1 halfTermWidth triangleWidth
      where
        halfTermWidth = termWidth `div` 2
        triangleWidth = if heightInput > 0 then 1 else (-heightInput) * 2 - 1
        newHeight     = doEitherIf (id) (negate) (heightInput > 0) halfTermWidth
