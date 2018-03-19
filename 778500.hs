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
getFilmsWithDirector :: Database -> Director -> Database
getFilmsWithDirector database dirQ = [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, dir == dirQ]

-- Return testDatabase with films that have rating score of 75% or above (iv.)
getFilmsWithRatingSeventyFivePlus :: Database -> Database
getFilmsWithRatingSeventyFivePlus database = [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, getAvgRating score >= 75]

-- Return the average website rating for the films of a give director (v.)
getAvgDirRating :: Database -> Director -> Float
getAvgDirRating database dir = (foldr (+) 0 [getAvgRating score | (_, _, _, score) <- (getDirFilms database dir)]) / (fromIntegral (length (getDirFilms database dir)))

-- Return Title of movies rated by a particular user, also if they liked it or not (vi).
getScoreFrom :: Database -> Fan -> Rating
getScoreFrom database fan = [(ti, getFanScore rating fan) | (ti, _, _, rating) <- database, getFanScore rating fan >= 0]

-- Return all films between given years (viii.)
getFilmsYear :: Database -> Year -> Year -> Database
getFilmsYear database yr1 yr2 = [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, (yr >= yr1 && yr <= yr2)]

{- 
$$$$$$$$$$$$$$$$$$$
// Help-Functions :
$$$$$$$$$$$$$$$$$$$
-}
-- This helper function formats all of the films into a neat list.
filmsAsString :: Database -> String
filmsAsString [] = ""
filmsAsString ((ti,dir,yr,score):rest) = "\n Title: " ++ ti ++ "\n Director: " ++ dir ++ "\n Year: " ++ show yr ++ "\n Avg Rating: " ++ show (getAvgRating score) ++ "\n" ++ filmsAsString rest

-- This helper functions is used to calculate the average of the rating list returned by getRating
getAvgRating :: Rating -> Float
getAvgRating [] = 0
getAvgRating rating = fromIntegral (foldr (+) 0 (getRating rating)) / fromIntegral (length rating)

-- This helper function is used to return average rating.
getRating :: Rating -> [Int]
getRating rating = [score | (fan, score) <- rating]

-- This helper function is used for Avg Director Rating.
getDirFilms :: Database -> Director -> Database
getDirFilms database dirQ = [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, dir == dirQ]

-- This helper function is used for getScoreFrom
getFanScore :: Rating -> Fan -> RateScore
getFanScore [] fan = -1
getFanScore ((name, score):rest) fan = if fan==name then score else getFanScore rest fan

-- This helper function is used to format list of ratings
listOfRatingInString :: Rating -> String
listOfRatingInString [] = ""
listOfRatingInString ((title, score):rest) =  "\n Title: " ++ title ++ "\n Rating: " ++ (show score) ++ "%" ++ (if score >= 50 then " Likes" else " Dislikes") ++ "\n" ++ listOfRatingInString rest

-- This helper function checks if a movie has a rating from the selected fan.
getRatingFrom :: Database -> Fan -> Rating
getRatingFrom database fan = [(title, getFanScore rating fan) | (title, _, _, rating) <- database, getFanScore rating fan >= 0]

--
applyFilmRating :: Film -> Title -> Fan -> RateScore -> Film
applyFilmRating (fTitle, director, year , rating) title fan score
    | fTitle /= title                                                = (fTitle, director, year , rating)
    | [(fTitle, director, year , rating)] `getRatingFrom` fan /= []  = (fTitle, director, year , updateRating rating fan score) 
    | otherwise                                                      = (fTitle, director, year , rating ++ [(fan, score)])

--
updateRating :: Rating -> Fan -> RateScore -> Rating
updateRating [] fan score = []
updateRating ((rName , rScore): rest) fan score = if rName==fan then [(rName, score)] ++ updateRating rest fan score else [(rName, rScore)] ++ updateRating rest fan score

--


{- 
    This helper function checks
    if the film year is bigger
    than the given one
-}
-- This helper function is a rate function which is used to rate / override rating
rateThisFilm :: Database -> Title -> Fan -> RateScore -> Database
rateThisFilm database title fan score = [applyFilmRating film title fan score | film <- database]

-- moviesByYearAsString :: Database -> Year -> Year -> String
-- moviesByYearAsString database year1 year2 = 

{- 
    This helper function checks if the film year is bigger
    than the given one, if it 
-}
-- checkYear :: Database -> Year -> String
-- checkYear database year = (filmsAsString [(ti,dir,yr,score) | (ti,dir,yr,score) <- database, yr >= year])

-- $$$$$$$$$$$$$$$$$$$$
-- // Demo Functions :
-- $$$$$$$$$$$$$$$$$$$$

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
-- putStrLn all films after adding 2018 film "Sherlock Gnomes" directed by by "John Stevenson" to testDatabase
demo 1  = putStrLn (filmsAsString (addFilm "Sherlock Gnomes" "John Stevenson" 2018 testDatabase))
-- Get all films in the database
demo 2  = getAllFilms testDatabase
-- Get all films in the database directed by Ridley Scott.
demo 3  = putStrLn (filmsAsString(getFilmsWithDirector testDatabase "Ridley Scott"))
-- Get all films with rating >= 75%.
demo 4  = putStrLn (filmsAsString(getFilmsWithRatingSeventyFivePlus testDatabase))
-- Get average rating for Ridley Scott movies.
demo 5  = putStrLn ("Ridley Scott Average Rating: " ++ printf "%.4s"(show (getAvgDirRating testDatabase "Ridley Scott")))
-- Get titles of films rated by Emma with Likes and Dislikes.
demo 6  = putStrLn (listOfRatingInString (testDatabase `getRatingFrom` "Emma"))
-- Get all films after Emma says she likes Avatar.
demo 7  = putStrLn (filmsAsString (rateThisFilm testDatabase "Avatar" "Emma" 100))
-- Get all films after Emma says she likes Titanic.
demo 71 = putStrLn (filmsAsString (rateThisFilm testDatabase "Titanic" "Emma" 100))
-- Get all films after Emma says she dislikes Jaws.
demo 72 = putStrLn (filmsAsString (rateThisFilm testDatabase "Jaws" "Emma" 0))
-- Get Films between year 2000 and 2006 (Not sorted ATM).
demo 8 = putStrLn (filmsAsString (getFilmsYear  testDatabase 2000 2006))

-- $$$$$$$$$$$$$$$$$$$
-- // User Interface :
-- $$$$$$$$$$$$$$$$$$$

main :: IO ()
main = do
    currentUser <- getCurrentUser
    database <- getDbFile
    userInterface database currentUser
    return()

getCurrentUser :: IO Fan
getCurrentUser = do
    putStrLn "Enter your name please: "
    response <- getLine
    return response

getDbFile :: IO Database
getDbFile = do
    file <- readFile "films.txt"
    return (read file)

writeDbFile :: Database -> IO ()
writeDbFile database = do
    writeFile "films.txt" (show database)


userInterface :: Database -> Fan -> IO ()
userInterface database currentUser = do
    putStrLn ("Hey there " ++ currentUser ++ " ! ! ! Please enter the following numbers for corresponding function.")
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn "1. Show All films in the Database."
    putStrLn "2. Show movies between two Years (for example : 2000 & 2006)"
    putStrLn "3. Show movies with rating 75% +"
    putStrLn "4. Search movies by Director"
    putStrLn "5. Show Average rating for a Director"
    putStrLn "6. Show user rating"
    putStrLn "7. Rate selected movie"
    putStrLn "8. Add new movie to the database"
    putStrLn "9. Quit \n"
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStr   "Select your number: "
    response <- getLine

    case response of
        "1" -> do
            -- Shows all films in the database
            putStrLn (filmsAsString database)
            putStrLn "\n"
            putStrLn "Do you wish to go back to main menu?"
            putStr   "enter y or n: "
            response2 <- getLine

            case response2 of
                "y" -> do
                    -- Go back to main menu
                    userInterface database currentUser
                "n" -> do
                    -- Quit and save
                    writeDbFile database
                    return ()
                _ -> userInterface database currentUser
        "2" -> showMenu2 database currentUser
        
        "3" -> do
            -- Show all movies with rating 75%
            putStrLn (filmsAsString (getFilmsWithRatingSeventyFivePlus database))
            putStrLn "\n"
            putStrLn "Do you wish to go back to main menu?"
            putStr   "enter y or n: "
            response2 <- getLine

            case response2 of
                "y" -> do
                    -- Go back to main menu
                    userInterface database currentUser
                "n" -> do
                    -- Quit and save
                    writeDbFile database
                    return ()
                _ -> userInterface database currentUser

        "4" -> showMenu4 database currentUser

        
        "5" -> showMenu5 database currentUser

        "6" -> do
            putStrLn "Not implemented in UI, Only Demo 6"
        "7" -> do
            putStrLn "Not implemented in UI, Only Demo 7 , Demo 71, Demo 72"
        "8" -> do
            putStrLn "Not implemented in UI, Only Demo 1"

        "9" -> do
                -- Quit and save
                writeDbFile database
                return ()
        
        _ -> userInterface database currentUser

showMenu2 :: Database -> Fan -> IO ()
showMenu2 database currentUser = do
    putStrLn "Enter First Year: "
    y1 <- getLine
    let  yr1 = read y1 :: Integer
    if (yr1 >= 1900) && (yr1 <= 2018) then do
        let year1 = read y1
        putStrLn "Enter Second Year: "
        y2 <- getLine
        let  yr2 = read y2 :: Integer
        if (yr2 >= 1900) && (yr2 <= 2018) then do
            let year2 = read y2
            putStrLn (filmsAsString (getFilmsYear database year1 year2))
        else do
            putStrLn ("Incorrect Year - Enter first and second year between 1900 & 2018")
            showMenu2 database currentUser
    else do
        putStrLn ("Incorrect Year - Enter first and second year between 1900 & 2018")
        showMenu2 database currentUser
    
    putStrLn "\n"
    putStrLn "Do you wish to go back to main menu?"
    putStr   "enter y or n: "
    response2 <- getLine

    case response2 of
        "y" -> do
            -- Go back to main menu
            userInterface database currentUser
        "n" -> do
            -- Quit and save
            writeDbFile database
            return ()
        _ -> userInterface database currentUser

showMenu4 :: Database -> Fan -> IO ()
showMenu4 database currentUser = do
    putStrLn "1. Ridley Scott"
    putStrLn "2. David Cronenberg"
    putStrLn "3. James Cameron"
    putStrLn "4. Martin Scorsese"
    putStrLn "5. Steven Spielberg"
    putStrLn "6. Frank Darabont"
    putStrLn "7. J J Abrams"
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStr   "Pick Director: "
    response <- getLine

    case response of
        "1" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "Ridley Scott"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "2" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "David Cronenberg"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "3" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "James Cameron"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "4" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "Martin Scorsese"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "5" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "Steven Spielberg"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "6" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "Frank Darabont"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "7" -> do
            putStrLn (filmsAsString(getFilmsWithDirector testDatabase "J J Abrams"))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser
        
        _ -> userInterface database currentUser


showMenu5 :: Database -> Fan -> IO ()
showMenu5 database currentUser = do
    putStrLn "1. Ridley Scott"
    putStrLn "2. David Cronenberg"
    putStrLn "3. James Cameron"
    putStrLn "4. Martin Scorsese"
    putStrLn "5. Steven Spielberg"
    putStrLn "6. Frank Darabont"
    putStrLn "7. J J Abrams"
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStr   "Pick Director for Average: "
    response <- getLine

    case response of
        "1" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "Ridley Scott")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "2" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "David Cronenberg")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "3" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "James Cameron")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "4" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "Martin Scorsese")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "5" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "Steven Spielberg")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "6" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "Frank Darabont")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser

        "7" -> do
            putStrLn (printf "%.4s" (show (getAvgDirRating testDatabase "J J Abrams")))
            putStrLn "\n"
            putStrLn "Enter any key to go back to main menu"
            putStr   "enter y or n: "
            response2 <- getLine
            -- Back to main menu
            userInterface database currentUser
        
        _ -> userInterface database currentUser


-- $$$$$$$$$$$$$$$$$$$$$$
-- // Demonstration Database
-- $$$$$$$$$$$$$$$$$$$$$$

testDatabase :: Database
testDatabase = [("Blade Runner", "Ridley Scott", 1982, [("Zoe", 90), ("Heidi", 70), ("Jo", 94), ("Kate", 100), ("Emma", 89), ("Liz", 74), ("Dave", 79), ("Sam", 75), ("Olga", 73), ("Tim", 78)]),
    ("The Fly", "David Cronenberg", 1986, [("Garry", 100), ("Dave", 75), ("Zoe", 73), ("Kevin", 71), ("Emma", 69), ("Heidi", 100), ("Jo", 72), ("Kate", 77)]),
    ("Body Of Lies", "Ridley Scott", 2008, [("Garry", 64), ("Dave", 68), ("Bill", 63), ("Olga", 68), ("Tim", 60), ("Zoe", 62), ("Paula", 74)]),
    ("Avatar", "James Cameron", 2009, [("Dave", 74), ("Amy", 75), ("Liz", 73), ("Olga", 77), ("Tim", 79), ("Zoe", 70), ("Paula", 65)]),
    ("Titanic", "James Cameron", 1997, [("Zoe", 40), ("Emma", 47), ("Paula", 49), ("Liz", 65), ("Olga", 75), ("Dave", 77), ("Sam", 79), ("Wally", 62), ("Kate", 63)]),
    ("The Departed", "Martin Scorsese", 2006, [("Wally", 63), ("Liz", 61), ("Kevin", 68), ("Tim", 65), ("Emma", 69), ("Olga", 74), ("Dave", 72), ("Kate", 76), ("Zoe", 79)]),
    ("Aliens", "Ridley Scott", 1986, [("Dave", 79), ("Garry", 100), ("Liz", 77), ("Sam", 100), ("Wally", 70), ("Kate", 65), ("Zoe", 100), ("Tim", 68), ("Emma", 63), ("Jo", 62), ("Olga", 61)]),
    ("Kingdom Of Heaven", "Ridley Scott", 2005, [("Jo", 46), ("Wally", 49), ("Emma", 56), ("Tim", 58), ("Garry", 53), ("Ian", 73), ("Neal", 79)]),
    ("Alien: Covenant", "Ridley Scott", 2017, [("Kevin", 57), ("Tim", 46), ("Emma", 54), ("Jo", 52), ("Liz", 51)]),
    ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, [("Dave", 51), ("Amy", 58), ("Garry", 58), ("Ian", 54), ("Neal", 54), ("Jenny", 59), ("Kate", 59), ("Emma", 69), ("Olga", 69)]),
    ("Bridge of Spies", "Steven Spielberg", 2015, [("Wally", 69), ("Sam", 100), ("Dave", 100), ("Neal", 73), ("Bill", 78), ("Garry", 100), ("Ian", 73), ("Kate", 72)]),
    ("Jaws", "Steven Spielberg", 1975, [("Jenny", 72), ("Emma", 79), ("Bill", 79), ("Neal", 74), ("Sam", 63), ("Ian", 63), ("Kate", 67)]),
    ("The Martian", "Ridley Scott", 2015, [("Wally", 67), ("Sam", 62), ("Dave", 62), ("Jo", 69), ("Jenny", 74), ("Kate", 47), ("Emma", 47), ("Olga", 53), ("Ian", 57), ("Neal", 51), ("Tim", 64), ("Liz", 58)]),
    ("The BFG", "Steven Spielberg", 2016, [("Sam", 64), ("Wally", 100), ("Dave", 68), ("Jo", 64), ("Kate", 100), ("Neal", 73)]),
    ("The Shawshank Redemption", "Frank Darabont", 1994, [("Dave", 73), ("Amy", 71), ("Bill", 78), ("Garry", 67), ("Ian", 63), ("Neal", 69), ("Kate", 56), ("Jenny", 52), ("Zoe", 59), ("Heidi", 63), ("Jo", 61)]),
    ("Gladiator", "Ridley Scott", 2000, [("Olga", 65), ("Neal", 100), ("Kate", 100), ("Garry", 74), ("Heidi", 78), ("Bill", 100), ("Sam", 52), ("Zoe", 59)]),
    ("The Green Mile", "Frank Darabont", 1999, [("Kevin", 59), ("Tim", 67), ("Emma", 63), ("Heidi", 66), ("Kate", 69), ("Jenny", 73), ("Zoe", 72)]),
    ("True Lies", "James Cameron", 1994, [("Sam", 71), ("Dave", 76), ("Emma",58), ("Olga", 73), ("Jenny", 65), ("Zoe", 68)]),
    ("Super 8", "J J Abrams", 2011, [("Kevin", 68), ("Tim", 64), ("Emma", 65), ("Olga", 66), ("Heidi", 74), ("Wally", 73), ("Dave", 78), ("Jenny", 71), ("Zoe", 75)]),
    ("Minority Report", "Steven Spielberg", 2002, [("Kevin", 75), ("Kate", 65), ("Tim", 68), ("Emma", 66), ("Jenny", 62), ("Zoe", 71), ("Olga", 72), ("Heidi", 53)]),
    ("War Horse", "Steven Spielberg", 2011, [("Garry", 36), ("Bill", 67), ("Olga", 45), ("Jo", 100), ("Wally", 100), ("Emma", 21), ("Tim", 75), ("Kate", 79), ("Zoe", 54), ("Heidi", 74), ("Jenny", 63), ("Sam", 64)]),
    ("Silence", "Martin Scorsese", 2016, [("Wally", 64), ("Emma", 64), ("Tim", 64), ("Heidi", 64), ("Bill", 74), ("Jo", 73), ("Dave", 45), ("Olga", 47)]),
    ("The Terminal", "Steven Spielberg", 2004, [("Kate", 47), ("Dave", 43), ("Jo", 54), ("Wally", 53), ("Emma", 58), ("Heidi", 55)]),
    ("Star Wars: The Force Awakens", "J J Abrams", 2015, [("Emma", 100), ("Wally", 100), ("Zoe", 59), ("Kate", 64), ("Bill", 100), ("Dave", 63), ("Liz", 60), ("Olga", 62), ("Jo", 72), ("Neal", 47)]),
    ("Hugo", "Martin Scorsese", 2011, [("Wally", 47), ("Sam", 63), ("Kate", 53), ("Bill", 74), ("Dave", 68)])]
