module Robot ( readLDM
             , readLCR
             , run
             ) where

import Control.Monad.State
import Parsing 
import Prelude hiding (lines)

type Fuel = Int
type Point = (Int,Int)
type Material = Int

data Robot = Robot {
                energy    :: Fuel,
                position  :: Point,
                collected :: Material
             } deriving (Eq, Ord)

sampleRobot :: Robot
sampleRobot = Robot {
                 energy = 100,
                 position = (1,1),
                 collected = 0
              }

-- QUESTION 01
instance Show Robot where
    show robot = "Energy:" ++ show (energy robot) ++ "\nPosition:" ++ show (position robot) ++ "\nCollected:" ++ show (collected robot)

data Element = Empty         -- espaço vazio
             | Entry         -- entrada da mina
             | Wall          -- parede
             | Earth         -- terra
             | Rock          -- rocha
             | Material Int  -- material, Int indica quantidade.
             deriving (Eq,Ord)

-- QUESTION 03
-- runParser (pElement) (show Entry)
pElement :: Parser Char Element
pElement = f <$> (symbol ' ' <|> symbol 'E' <|> symbol '%' 
    <|> symbol '.' <|> symbol '*' <|> symbol '?' 
    <|> symbol ':' <|> symbol ';' <|> symbol '$')
      where f ' ' = Empty
            f 'E' = Entry
            f '%' = Wall
            f '.' = Earth
            f '*' = Rock
            f '?' = Material 50
            f ':' = Material 100
            f ';' = Material 150
            f '$' = Material 200
            
-- QUESTION 02
instance Show Element where
    show Empty = " "
    show Entry = "E"
    show Wall = "%"
    show Earth = "."
    show Rock = "*"
    show (Material 50) = "?"
    show (Material 100) = ":"
    show (Material 150) = ";"
    show (Material x) = "$"

type Line = [Element]

data Mine = Mine {
              lines  :: Int,
              columns  :: Int,
              elements :: [Line]
            } deriving (Eq, Ord)

-- QUESTION 07
instance Show Mine where
  show mine = unlines (map show (elements mine))

-- QUESTION 04
-- CHeck if lines and Columns must have same length as in the mine and a mine needs at least one entry point in the boarder.
validMine :: Mine -> Bool
validMine mine = 
    (lines mine) == length (elements mine) 
    && all (\line -> (columns mine) == length line) (elements mine)
    && any (\line -> any (\element -> element == Entry) line) (elements mine)

-- QUESTION 05
exampleMine :: Mine
exampleMine = Mine {
    lines = 15,
    columns = 15,
    elements = [[Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
                [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Wall],
                [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall],
                [Wall, Rock, Rock, Rock, Earth, Earth, Earth, Empty, Earth, Earth, Rock, Rock, Rock, Earth, Wall],
                [Wall, Earth, Material 50, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Rock, Earth, Earth, Wall],
                [Wall, Earth, Earth, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
                [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
                [Wall, Earth, Material 100, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Earth, Wall],
                [Wall, Earth, Earth, Empty, Earth, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Earth, Earth, Wall],
                [Wall, Earth, Earth, Rock, Earth, Empty, Earth, Earth, Empty, Earth, Earth, Earth, Earth, Earth, Wall],
                [Wall, Earth, Earth, Earth, Earth, Empty, Earth, Earth, Empty, Earth, Material 150, Material 150, Earth, Earth, Wall],
                [Wall, Earth, Rock, Earth, Earth, Empty, Earth, Earth, Earth, Material 150, Material 150, Earth, Earth, Rock, Wall],
                [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Material 200, Wall],
                [Wall, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Earth, Empty, Empty, Empty, Earth, Wall],
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Entry, Wall]]
}

-- QUESTION 06
-- runParser (pMine) (show exampleMine)
pLine :: Parser Char Line
pLine = many pElement

pMine :: Parser Char Mine
pMine = f <$> endBy pLine (symbol '\n')
    where f x = Mine {
        lines = length x,
        columns = length (head x),
        elements = x
    }

data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

-- QUESTION 08
pInstr :: Parser Char Instr
pInstr = f <$> (symbol 'L' <|> symbol 'R' <|> symbol 'U' <|> symbol 'D' <|> symbol 'C' <|> symbol 'S')
    where f 'L' = L
          f 'R' = R
          f 'U' = U
          f 'D' = D
          f 'C' = C
          f 'S' = S

-- QUESTION 09
pProgram :: Parser Char [Instr]
pProgram = many pInstr

type Conf = (Robot, Mine)
type ConfM a = State Conf a

-- QUESTION 10
current :: ConfM Point
current = do
    (robot, _) <- get
    return (position robot)

mine :: ConfM Mine
mine = do
    (_, mine) <- get
    return mine

enoughEnergy :: Int -> ConfM Bool
enoughEnergy n = do
    (robot, _) <- get
    return (energy robot >= n)
    
incEnergy :: ConfM ()
incEnergy = do
    (robot, mine) <- get
    put (robot {energy = energy robot + 1}, mine)

-- QUESTION 11 - Determine if a instruction is valid or not based on the previous rules.
valid :: Instr -> ConfM Bool
valid instr = do
    (robot, mine) <- get
    case instr of
        L -> do
            (x, y) <- current
            return (x > 0 && (elements mine !! y) !! (x - 1) /= Wall)
        R -> do
            (x, y) <- current
            return (x < (columns mine - 1) && (elements mine !! y) !! (x + 1) /= Wall)
        U -> do
            (x, y) <- current
            return (y > 0 && (elements mine !! (y - 1)) !! x /= Wall)
        D -> do
            (x, y) <- current
            return (y < (lines mine - 1) && (elements mine !! (y + 1)) !! x /= Wall)
        C -> do
            (x, y) <- current
            return ((elements mine !! y) !! x == Material 50 || (elements mine !! y) !! x == Material 100 || (elements mine !! y) !! x == Material 150 || (elements mine !! y) !! x == Material 200)
        S -> do
            return (energy robot < 100)

-- QUESTION 12 - From an instruction, updates the mine configuration, if it is valid.
updateMine :: Instr -> ConfM ()
updateMine instr = do
    (robot, mine) <- get
    case instr of
        L -> do
            (x, y) <- current
            put (robot {position = (x - 1, y)}, mine)
        R -> do
            (x, y) <- current
            put (robot {position = (x + 1, y)}, mine)
        U -> do
            (x, y) <- current
            put (robot {position = (x, y - 1)}, mine)
        D -> do
            (x, y) <- current
            put (robot {position = (x, y + 1)}, mine)
        C -> do
            (x, y) <- current
            case (elements mine !! y) !! x of
                Material 50 -> do
                    put (robot {position = (x, y), energy = energy robot + 50}, mine {elements = (take y (elements mine)) ++ [(take x ((elements mine) !! y)) ++ [Empty] ++ (drop (x + 1) ((elements mine) !! y))] ++ (drop (y + 1) (elements mine))})
                Material 100 -> do
                    put (robot {position = (x, y), energy = energy robot + 100}, mine {elements = (take y (elements mine)) ++ [(take x ((elements mine) !! y)) ++ [Empty] ++ (drop (x + 1) ((elements mine) !! y))] ++ (drop (y + 1) (elements mine))})
                Material 150 -> do
                    put (robot {position = (x, y), energy = energy robot + 150}, mine {elements = (take y (elements mine)) ++ [(take x ((elements mine) !! y)) ++ [Empty] ++ (drop (x + 1) ((elements mine) !! y))] ++ (drop (y + 1) (elements mine))})
                Material 200 -> do
                    put (robot {position = (x, y), energy = energy robot + 200}, mine {elements = (take y (elements mine)) ++ [(take x ((elements mine) !! y)) ++ [Empty] ++ (drop (x + 1) ((elements mine) !! y))] ++ (drop (y + 1) (elements mine))})
        S -> do
            put (robot {energy = 100}, mine)

-- QUESTION 13
exec :: Instr -> ConfM ()
exec = undefined

-- QUESTION 14
initRobot :: Mine -> Robot
initRobot mine = initRobot' 0 0 mine where
    initRobot' x y mine
        | (elements mine !! y) !! x == Entry = Robot 100 (x, y) 0
        | x < (columns mine) - 1 = initRobot' (x + 1) y mine
        | otherwise = initRobot' 0 (y + 1) mine

-- QUESTION 15
run :: [Instr] -> Mine -> Mine
run = undefined

-- QUESTION 16 - Implement a function that reads ".ldm" files containing mine descriptions and returns a value of type Mine or an error message indicating that the file could not be read.
readLDM :: String -> IO (Either String Mine)
readLDM = undefined
    
-- QUESTION 17 - Implement a function that reads ".lcr" files containing robot descriptions and returns a value of type [Instr] or an error message indicating that the file could not be read.
readLCR :: String -> IO (Either String [Instr])
readLCR = undefined


