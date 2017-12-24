{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.PrettyPrint.HughesPJ
import System.Random
import Control.Applicative
import Control.Monad

showpt (x,y) = double x <+> double y

moveto p = showpt p <+> text "moveto"
lineto p = showpt p <+> text "lineto"

gsave = text "gsave"
grestore = text "grestore"
gsaving x = gsave <+> x <+> grestore

setrgbcolor r g b = hsep (map double [r,g,b]) <+> text "setrgbcolor"

polygon (p:ps) = text "newpath" $+$ moveto p $+$ vcat (map lineto ps) $+$ text "closepath"

circle (x,y) r n phi0 = [ (x + r * cos phi, y + r * sin phi) | phi <- map angle [0..n-1]]
    where angle i = phi0 + 2 * pi / fromIntegral n * fromIntegral i

star c r1 r2 phi1 color = polygon pts $+$ gsaving(color <+> text "fill") <+> text "0 setgray stroke"
    where
        pts = concat $ zipWith (\x y -> [x,y]) (circle c r1 5 phi1) (circle c r2 5 phi2)
        phi2 = phi1 + 2*pi/10

poissonize positions = poissonize1 emptyWorld positions
    where
        poissonize world [] = []
        poissonize1 world (p:ps) | fits p world = p : poissonize1 (add p world) ps
                                 | otherwise = poissonize1 world ps
        
        emptyWorld = []
        add p world = p : world
        fits p world = not $ any (intersects p) world
         
        intersects ((x1,y1),r1) ((x2,y2),r2) = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) <= (r1+r2)*(r1+r2) 

starcolor = do
    return $ setrgbcolor 1 0.8 0.2
    {- r <- randomRIO (0.9, 1)
    g <- randomRIO (r - 0.17, r - 0.1)
    b <- randomRIO (0.2, g/2)
    return $ setrgbcolor r g b -}

makeImage = do
    positions :: [((Double, Double), Double)] <- (take 200 . poissonize) <$> liftA3 (\g1 g2 g3 -> 
                let xs = randomRs (0,595) g1
                    ys = randomRs (0,842) g2
                    rs = randomRs (10,70) g3
                in zip (zip xs ys) rs
            ) newStdGen newStdGen newStdGen
    
    stars <- flip mapM positions $ \((x,y),r') -> do
        let r = r' - 5
        rratio <- randomRIO (0.4,0.6)
        phi <- randomRIO (0, 2*pi)
        color <- starcolor
        return $ star (x,y) (r*rratio) r phi color
    
    return $ vcat stars


main = do
    putStrLn "%!PS-Adobe-2.0"
    putStrLn "%%Creator: Me"
    putStrLn "%%Pages: 10"
    putStrLn "%%PageOrder: Ascend"
    putStrLn "%%BoundingBox: 0 0 595 842"
    putStrLn "%%DocumentPaperSizes: A4"
    putStrLn "%%EndComments"
    flip mapM_ [1..1] $ \i -> do
        putStrLn $ "%%Page: " ++ show i
        img <- makeImage

        putStrLn $ render $ img $+$ text "showpage"
