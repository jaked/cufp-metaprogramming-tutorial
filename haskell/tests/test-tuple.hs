{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Tuple.TH as T

-- map a 0-length tuple
t0 = $(T.map 0) undefined ()

-- since there is no 1-length tuples this boils down
-- to the application function: $(T.map 1) = ($)
t1 = $(T.map 1) not True

-- map a pair
t2 = $(T.map 2) not (True,False)

-- map a triple
t3 = $(T.map 3) not (True,False,False)

-- let's collect all them as a tuple
allt = (t0,t1,t2,t3)

tlla = $(T.reverse 4) allt

tllallt = $(4 T.++ 3) tlla ($(T.tail 4) allt)

-- let's say we want to `show' them, since they are of
-- different types using `$(T.map 4) show' wont type-check.
-- Instead we use an alternative version which takes the
-- name of the function to apply. Any call site can then
-- be resolved differently.
allStr = $(T.map' 4 'show) allt
allIO = $(T.mapM' 4 'print) allt
allIO' = $(T.mapM 4) putStrLn allStr
oneIO = $(T.mapM_' 4 'print) allt
oneIO' = $(T.mapM_ 4) putStrLn allStr

zip2alltallt = $(T.zip 2 4) allt allt

main :: IO ()
main = do oneIO
          ((),(),(),()) <- allIO
          oneIO'
          print tllallt
          print zip2alltallt
