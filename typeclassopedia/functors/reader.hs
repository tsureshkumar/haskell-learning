import Test.HUnit

--- annotation types: a container which holds a value 'a' with annotation of type 'e'. (e, a)
type Reader e = ((->) e)

instance Functor (Reader e) where
  fmap g h = g.h
  
find :: Eq a => [(a,String)] -> a -> String
find xs x =  f (lookup x xs) 
  where
    f (Just y) = y
    f Nothing = "none"
  
tests = TestList [ "test1" ~: "1" ~: 
                   "ts-sk" ~=? (prepend 1),
                   "test2" ~: "2" ~: 
                   "ts-ku" ~=? (prepend 2)
                   ]
        where env = [ (1, "sk"),
                      (2, "ku")
                      ]
              prepend = (fmap ((++)"ts-") (find env))

main = runTestTT tests