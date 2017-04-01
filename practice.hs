dblSmallNum :: Int -> Int
dblSmallNum x = if x > 100 then x else x*3

-- | patterns vs inferior ways to write functions 

-- | lucky: the good
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- | lucky: the bad
lucky' :: (Integral a) => a -> String
lucky' x = if x==7 then "LUCKY NUMBER SEVEN!" else "Sorry, you're out of luck, pal!"

-- | sayMe: the good
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- | sayMe: the bad
sayMe'' :: (Integral a) => a -> String
sayMe'' x = if null (numWordArr' x) then "Not Between 1 and 5" else head $ numWordArr' x 
	where numWordArr' x = [numMatch' x n | n <- [(1, "One!"),(2, "Two!"),(3, "Three!"),(4, "Four!"),(5, "Five!")], numMatch' x n /= "blah"] 
		where numMatch' x n = if x == fst n then snd n else "blah"

-- | sayMe: the ugly
sayMe' :: (Integral a) => a -> String
sayMe' x = if null (numWordArr x) then "Not Between 1 and 5" else head $ numWordArr x 
numMatch :: (Integral a) => a -> (a, String) -> String
numMatch x n = if x == fst n then snd n else "blah"
numWordArr :: (Integral a) => a -> [String] 
numWordArr x = [numMatch x n | n <- [(1, "One!"),(2, "Two!"),(3, "Three!"),(4, "Four!"),(5, "Five!")], numMatch x n /= "blah"] 

-- | factorial: the good
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | factorial: the bad
factorial' :: Int -> Int
factorial' n = if n == 0 then n else n * factorial (n - 1)  


