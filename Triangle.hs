module Triangle (triangle) where

import Control.Monad
import Shortener

-- This function prints an ASCII triangle.
printTriangle :: Int -> Int -> Int -> Int -> IO ()
printTriangle heightInput heightCounter amountToIndent triangleWidth =
    when (heightCounter <= abs heightInput) $ do
        putStr $ "\n" ++ " " `repeatTimes` newIndent ++ "`" `repeatTimes` triangleWidth
        printTriangle heightInput incCounter incIndent incWidth
      where
        newIndent  = doEitherIf (+0) (subtract final) (heightInput > 0) amountToIndent
        incIndent  = doEitherIf (+1) (subtract 1)     (heightInput < 0) amountToIndent
        incWidth   = doEitherIf (+2) (subtract 2)     (heightInput > 0) triangleWidth
        incCounter = heightCounter + 1
        -- Subtract for every time we're going to add to the indent.
        final      = abs(heightInput) - 1

-- This function creates the triangle's variables.
triangle :: Int -> Int -> IO ()
triangle heightInput termWidth
    -- If the width would overflow, use the max that won't.
    | abs heightInput > halfTermWidth = triangle newHeight termWidth
    | otherwise = printTriangle heightInput 1 halfTermWidth triangleWidth
      where
        halfTermWidth = termWidth `div` 2
        triangleWidth = if heightInput > 0 then 1 else (-heightInput) * 2 - 1
        newHeight     = doEitherIf (+0) (negate) (heightInput > 0) halfTermWidth
