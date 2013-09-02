module World.Utils where

import World.World


logWorld logFile (w, anns) currentMove = do
    appendFile logFile ("\n=========" ++ show currentMove ++ "=========")
    appendFile logFile ("\n  Items count: " ++ show (itemsCount w) ++ "\n")
    appendFile logFile (unlines . map annotationMessage $ anns)
