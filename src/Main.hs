{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.PrettyPrint.HughesPJ
import System.Random
import Control.Applicative
import Control.Monad
import Control.Monad.State

showpt (x,y) = double x <+> double y

moveto p = showpt p <+> text "moveto"
lineto p = showpt p <+> text "lineto"
rmoveto p = showpt p <+> text "rmoveto"
rlineto p = showpt p <+> text "rlineto"

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
    -- return $ setrgbcolor 1 0.8 0.2
    r <- randomRIO (0.9, 1)
    g <- randomRIO (r - 0.17, r - 0.1)
    b <- randomRIO (0.2, g/2)
    return $ setrgbcolor r g b

data Object = Star | Snowflake deriving(Enum)


type Turtle = StateT (Bool, (Double, Double), Double) IO

rt, lt, fwd :: Double -> Turtle ()
rt x = modify (\(ps, p, phi) -> (ps, p, phi - (x * pi / 180)))
lt x = rt (-x)

fwd d = do
    (ps, p@(x,y), phi) <- get
    let p' = (x + d * cos phi, y + d * sin phi)
    put (ps, p', phi)
    when ps $ lift $ print $
        text "newpath" <+> moveto p <+> lineto p' <+> text "closepath" <+> text "stroke"
  
{-
flaketree 0 d = return ()
flaketree n d = do
    turtle <- get
    fwd d
    let alpha = 36
        k = 0.65
    lt alpha
    flaketree (n-1) (d * k)
    rt (2*alpha)
    flaketree (n-1) (d * k)
    put turtle
-}

saveturtle a = do
    t <- get
    x <- a
    put t
    return x


flaketree d = do
    {-let alpha1 = 60
        alpha2 = 36
        k1 = 0.3
        k2 = 0.65-}
    alpha1 <- lift $ randomRIO (50,65)
    alpha2 <- lift $ randomRIO (30,40)
    k1 <- lift $ randomRIO (0.2,0.32)
    k2 <- lift $ randomRIO (0.6,0.7)

    let linesize s = lift $ print $ double s <+> text "setlinewidth"
        go 0 d = return ()
        go n d = saveturtle $ do
            
            let ls = d/20 -- * (fromIntegral n) ** 0.5

            linesize ls
            fwd (d/2)

            saveturtle $ do
                lt alpha1
                go (n-1) (d * k1)
            saveturtle $ do
                rt alpha1
                go (n-1) (d * k1)
                
            linesize ls
            fwd (d/2)

            saveturtle $ do
                lt alpha2
                go (n-1) (d * k2)
            saveturtle $ do
                rt alpha2
                go (n-1) (d * k2)

    go 4 d
            

runTurtle :: (Double, Double) -> Double -> Turtle a -> IO a
runTurtle p phi action = evalStateT action (True, p, phi * pi / 180)

makeImage = do
    positions :: [((Double, Double), Double)] <- (take 200 . poissonize) <$> liftA3 (\g1 g2 g3 -> 
                let xs = randomRs (0,595) g1
                    ys = randomRs (0,842) g2
                    rs = randomRs (10,70) g3
                in zip (zip xs ys) rs
            ) newStdGen newStdGen newStdGen
    
    flip mapM_ positions $ \((x,y),r') -> do
        choice <- randomRIO (0.0:: Double, 1.0)
        phi <- randomRIO (0, 2*pi)
        case choice of
            _ | (choice < 0.9 || r' > 60) -> do
                let r = r' - 5
                rratio <- randomRIO (0.4,0.6)
                color <- starcolor
                putStrLn $ render $ star (x,y) (r*rratio) r phi color
            _ | otherwise -> do
                let r = r' - 5
                print gsave
                putStrLn "1 setlinecap"
                print $ setrgbcolor 0.5 0.7 1
                n <- randomRIO (5,7)
                runTurtle (x,y) phi $ do
                    replicateM_ n $ do
                        flaketree (r'/2.3)
                        rt (360/fromIntegral n)

                print grestore



main = do
    putStrLn "%!PS-Adobe-2.0"
    putStrLn "%%Creator: Me"
    putStrLn "%%Pages: 10"
    putStrLn "%%PageOrder: Ascend"
    putStrLn "%%BoundingBox: 0 0 595 842"
    putStrLn "%%DocumentPaperSizes: A4"
    putStrLn "%%EndComments"
    flip mapM_ [1..10] $ \i -> do
        putStrLn $ "%%Page: " ++ show i
        makeImage

        putStrLn "showpage"
