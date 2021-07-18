module Main where

main :: IO ()
main = interact id

isBigGang :: Int -> (Bool, String)
isBigGang x = (x >= 9, "Compare gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (v, newLog) = f x in (v, log ++ newLog)

test = ("some text", "create some text.") `applyLog` (\x -> (length x, " get length of " ++ x))