{-# OPTIONS_GHC -XOverlappingInstances -XTypeSynonymInstances #-}

--- redefine Maybe as we can't override instance Functor for Maybe
data MaybeN a = NothingN | JustN a
               deriving (Ord, Show, Eq)

instance Functor MaybeN where
   fmap g NothingN = NothingN
   fmap g (JustN a) = JustN (g a)

main = do
  putStrLn $ show $ fmap (>10) (JustN 20)
  putStrLn $ show $ fmap (>10) (JustN 5)
  putStrLn $ show $ fmap (>10) Nothing
