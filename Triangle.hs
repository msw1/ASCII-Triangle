module Triangle (triangle) where
import Shortener

makeTriangle :: Int -> Int -> Int -> Int -> String
makeTriangle height counter indent width =
    if counter <= height
        then "\n" 
          ++ " " `repeatTimes` newIndent 
          ++ "`" `repeatTimes` width 
          ++ makeTriangle height incCounter incIndent incWidth
        else ""
      where
        newIndent  = if height > 0 then indent     else indent - a
        incIndent  = if height < 0 then indent + 1 else indent - 1 
        incWidth   = if height > 0 then width  + 2 else width  - 2
        incCounter = counter + 1
        -- Subtract for every time we're going to add to the indent.
        a          = abs(height) - 1

triangle :: Int -> Int -> String
triangle height tWidth
    -- If the width would overflow, use the max that won't.
    | abs height > hTWidth = triangle newHeight tWidth
    | otherwise = makeTriangle height counter hTWidth width
      where
        hTWidth   = tWidth `div` 2
        width     = if height > 0 then 1 else (-height) * 2 - 1
        counter   = if height > 0 then 1 else (height * 2)
        newHeight = if height > 0 then hTWidth else -hTWidth
