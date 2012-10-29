import Test.HUnit

--- annotation types: a container which holds a value 'a' with annotation of type 'e'. (e, a)
type Ann e = ((,) e)

instance Functor (Ann e) where
  fmap g (x,y) = (x, (g y))
  
test1 = TestCase $ assertEqual "(1,20) > 10" True (snd (fmap (>10) (1, 20)))
tests = TestList [ "test1" ~: "(1,20) > 10)" ~: 
                   (1,True) ~=? (fmap (>10) (1,20)),
                   "test2" ~: "(1,20) < 10)" ~: 
                   (1,False) ~=? (fmap (<10) (1,20)),
                   "test3" ~: "'sk' ++ (1,'test') = (1, 'sk-test')" ~: 
                   (1,"sk-test") ~=? (fmap ((++) "sk-") (1,"test"))
                   ]

main = runTestTT tests