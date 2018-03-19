-- $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
-- Discrete Mathematics and Functional Programming
-- Haskell Coursework: Film Database (Weiver - revieW)
-- Student ID: 778500
-- $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

-- $$$$$$$$$$$$
-- // Imports :
-- $$$$$$$$$$$$

import Data.List
import Data.Char
import Text.Printf

-- $$$$$$$$$$$
--  // Types :
-- $$$$$$$$$$$
type Fan = String
type RateScore = Int

type Title = String
type Director = String
type Year = Int
type Rate = (Fan, RateScore)
type Rating = [Rate]

type Film = (Title, Director, Year, Rating)
type FilmScore = (Float, Title, Director, Year, Rating)
type Database = [Film]

-- $$$$$$$$$$$$$$$$$$$
--  // Functionality :
-- $$$$$$$$$$$$$$$$$$$

-- Add a new film to the database (i.)
addFilm :: Title -> Director -> Year -> Database -> Database
addFilm ti dir yr db = db++[(ti, dir, yr, [])]

-- Return testDatabase which includes all Films (ii.)
getAllFilms :: Database -> IO ()
getAllFilms database = putStrLn (filmsAsString database)

-- Return all films by a given Director (iii.)
getFilmsByDirector :: Database -> Director -> Database
getFilmsByDirector database dirQ = [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, dir == dirQ]

getFilmsWithDirector :: Database -> Director -> IO ()
getFilmsWithDirector database dirQ = putStrLn (filmsAsString [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, dir == dirQ])

-- Return testDatabase with films only after certain date
getFilmsWithRatingSeventyFivePlus :: Database -> IO ()
getFilmsWithRatingSeventyFivePlus database = putStrLn (filmsAsString [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, getAvgRating score >= 75])

-- $$$$$$$$$$$$$$$$$$$
-- // Help-Functions :
-- $$$$$$$$$$$$$$$$$$$

-- This helper function will change input list into string.
listAsString :: [String] -> String
listAsString [] = ""
listAsString (x:[]) = x ++ listAsString []
listAsString (x:xs) = x ++ ", " ++ listAsString xs

-- This helper function formats all of the films into a neat list.
filmsAsString :: Database -> String
filmsAsString [] = ""
filmsAsString ((ti,dir,yr,score):rest) = "\n Title: " ++ ti ++ "\n Director: " ++ dir ++ "\n Year: " ++ show yr ++ "\n Avg Rating: " ++ show (getAvgRating score) ++ "\n" ++ filmsAsString rest

-- Helper for directorAsString
-- directorAsString :: Database -> Director -> String
-- directorAsString [] = ""
-- directorAsString (())

-- This helper functions is used to calculate the average of the rating list returned by getRating
getAvgRating :: Rating -> Float
getAvgRating [] = 0
getAvgRating rating = fromIntegral (foldr (+) 0 (getRating rating)) / fromIntegral (length rating)

-- This helper function is used to return average rating.
getRating :: Rating -> [Int]
getRating rating = [score | (fan, score) <- rating]

-- $$$$$$$$$$$$$$$$$$$$
-- // Demo Functions :
-- $$$$$$$$$$$$$$$$$$$$

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes" 
--          directed by by "John Stevenson" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
--demo 4  = putStrLn all films with website rating >= 75%
--demo 5  = putStrLn average website rating for "Ridley Scott"
--demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
--demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
--demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
--demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
--demo 8  = films between 2000 and 2006 inclusive sorted by website rating

-- $$$$$$$$$$$$$$$$$$$
-- // User Interface :
-- $$$$$$$$$$$$$$$$$$$

-- $$$$$$$$$$$$$$$$$$$$$$
-- Demonstration Database
-- $$$$$$$$$$$$$$$$$$$$$$

testDatabase :: Database
testDatabase = [("Blade Runner", "Ridley Scott", 1982, [("Zoe", 75), ("Heidi", 70), ("Jo", 94), ("Kate", 71), ("Emma", 89), ("Liz", 74), ("Dave", 79), ("Sam", 75), ("Olga", 73), ("Tim", 78)]),
    ("The Fly", "David Cronenberg", 1986, [("Garry", 56), ("Dave", 75), ("Zoe", 73), ("Kevin", 71), ("Emma", 69), ("Heidi", 66), ("Jo", 72), ("Kate", 77)]),
    ("Body Of Lies", "Ridley Scott", 2008, [("Garry", 64), ("Dave", 68), ("Bill", 63), ("Olga", 68), ("Tim", 60), ("Zoe", 62), ("Paula", 74)]),
    ("Avatar", "James Cameron", 2009, [("Dave", 74), ("Amy", 75), ("Liz", 73), ("Olga", 77), ("Tim", 79), ("Zoe", 70), ("Paula", 65)]),
    ("Titanic", "James Cameron", 1997, [("Zoe", 40), ("Emma", 47), ("Paula", 49), ("Liz", 65), ("Olga", 75), ("Dave", 77), ("Sam", 79), ("Wally", 62), ("Kate", 63)]),
    ("The Departed", "Martin Scorsese", 2006, [("Wally", 63), ("Liz", 61), ("Kevin", 68), ("Tim", 65), ("Emma", 69), ("Olga", 74), ("Dave", 72), ("Kate", 76), ("Zoe", 79)]),
    ("Aliens", "Ridley Scott", 1986, [("Dave", 79), ("Garry", 73), ("Liz", 77), ("Sam", 71), ("Wally", 70), ("Kate", 65), ("Zoe", 64), ("Tim", 68), ("Emma", 63), ("Jo", 62), ("Olga", 61)]),
    ("Kingdom Of Heaven", "Ridley Scott", 2005, [("Jo", 46), ("Wally", 49), ("Emma", 56), ("Tim", 58), ("Garry", 53), ("Ian", 73), ("Neal", 79)]),
    ("Alien: Covenant", "Ridley Scott", 2017, [("Kevin", 57), ("Tim", 46), ("Emma", 54), ("Jo", 52), ("Liz", 51)]),
    ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, [("Dave", 51), ("Amy", 58), ("Garry", 58), ("Ian", 54), ("Neal", 54), ("Jenny", 59), ("Kate", 59), ("Emma", 69), ("Olga", 69)]),
    ("Bridge of Spies", "Steven Spielberg", 2015, [("Wally", 69), ("Sam", 69), ("Dave", 73), ("Neal", 73), ("Bill", 78), ("Garry", 73), ("Ian", 73), ("Kate", 72)]),
    ("Jaws", "Steven Spielberg", 1975, [("Jenny", 72), ("Emma", 79), ("Bill", 79), ("Neal", 74), ("Sam", 63), ("Ian", 63), ("Kate", 67)]),
    ("The Martian", "Ridley Scott", 2015, [("Wally", 67), ("Sam", 62), ("Dave", 62), ("Jo", 69), ("Jenny", 74), ("Kate", 47), ("Emma", 47), ("Olga", 53), ("Ian", 57), ("Neal", 51), ("Tim", 64), ("Liz", 58)]),
    ("The BFG", "Steven Spielberg", 2016, [("Sam", 64), ("Wally", 63), ("Dave", 68), ("Jo", 64), ("Kate", 69), ("Neal", 73)]),
    ("The Shawshank Redemption", "Frank Darabont", 1994, [("Dave", 73), ("Amy", 71), ("Bill", 78), ("Garry", 67), ("Ian", 63), ("Neal", 69), ("Kate", 56), ("Jenny", 52), ("Zoe", 59), ("Heidi", 63), ("Jo", 61)]),
    ("Gladiator", "Ridley Scott", 2000, [("Olga", 61), ("Neal", 61), ("Kate", 69), ("Garry", 74), ("Heidi", 78), ("Bill", 56), ("Sam", 52), ("Zoe", 59)]),
    ("The Green Mile", "Frank Darabont", 1999, [("Kevin", 59), ("Tim", 67), ("Emma", 63), ("Heidi", 66), ("Kate", 69), ("Jenny", 73), ("Zoe", 72)]),
    ("True Lies", "James Cameron", 1994, [("Sam", 71), ("Dave", 76), ("Emma",58), ("Olga", 73), ("Jenny", 65), ("Zoe", 68)]),
    ("Super 8", "J J Abrams", 2011, [("Kevin", 68), ("Tim", 64), ("Emma", 65), ("Olga", 66), ("Heidi", 74), ("Wally", 73), ("Dave", 78), ("Jenny", 71), ("Zoe", 75)]),
    ("Minority Report", "Steven Spielberg", 2002, [("Kevin", 75), ("Kate", 65), ("Tim", 68), ("Emma", 66), ("Jenny", 62), ("Zoe", 71), ("Olga", 72), ("Heidi", 53)]),
    ("War Horse", "Steven Spielberg", 2011, [("Garry", 36), ("Bill", 67), ("Olga", 45), ("Jo", 74), ("Wally", 95), ("Emma", 21), ("Tim", 75), ("Kate", 79), ("Zoe", 54), ("Heidi", 74), ("Jenny", 63), ("Sam", 64)]),
    ("Silence", "Martin Scorsese", 2016, [("Wally", 64), ("Emma", 64), ("Tim", 64), ("Heidi", 64), ("Bill", 74), ("Jo", 73), ("Dave", 45), ("Olga", 47)]),
    ("The Terminal", "Steven Spielberg", 2004, [("Kate", 47), ("Dave", 43), ("Jo", 54), ("Wally", 53), ("Emma", 58), ("Heidi", 55)]),
    ("Star Wars: The Force Awakens", "J J Abrams", 2015, [("Emma", 55), ("Wally", 51), ("Zoe", 59), ("Kate", 64), ("Bill", 68), ("Dave", 63), ("Liz", 60), ("Olga", 62), ("Jo", 72), ("Neal", 47)]),
    ("Hugo", "Martin Scorsese", 2011, [("Wally", 47), ("Sam", 63), ("Kate", 53), ("Bill", 74), ("Dave", 68)])]
