import System.Environment
import Data.Char
import TermSize
import Triangle

main :: IO ()
main = do
    termSize <- TermSize.getTermSize
    triangleSize <- getArgs
    let heightInput = filter(\x -> isNumber x || x == '-') . concat $ triangleSize in
        if not $ null heightInput && heightInput /= "-"
            then do 
                triangle (read heightInput) (snd termSize)
                putStr "\n"
            else
                putStrLn "Please enter an integer value."
