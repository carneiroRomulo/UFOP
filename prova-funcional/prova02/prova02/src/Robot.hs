module Robot ( readLDM
             , readLCR
             , run
             )where

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

instance Show Mine where
  show mine = unlines (map show (elements mine))

-- QUESTION 04
validMine :: Mine -> Bool
validMine mine = (lines mine) == length (elements mine) && all (\line -> (columns mine) == length line) (elements mine)

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
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]]
}

-- QUESTION 06
-- runParser (pMine) (show exampleMine)
pLine :: Parser Char Line
pLine = undefined

pMine :: Parser Char Mine
pMine = undefined
-- pMine = do
--     n <- natural
--     symbol ' '
--     m <- natural
--     symbol '\n'
--     lines <- pLine
--     return (Mine n m lines)


data Instr = L -- move para esquerda
           | R -- move para direita
           | U -- move para cima
           | D -- move para baixo
           | C -- coleta material
           | S -- para para recarga.
           deriving (Eq,Ord,Show,Enum)

pInstr :: Parser Char Instr
pInstr = undefined

pProgram :: Parser Char [Instr]
pProgram = undefined

type Conf = (Robot, Mine)

type ConfM a = State Conf a


current :: ConfM Point
current = undefined

mine :: ConfM Mine
mine = undefined

enoughEnergy :: Int -> ConfM Bool
enoughEnergy = undefined

incEnergy :: ConfM ()
incEnergy = undefined

valid :: Instr -> ConfM Bool
valid = undefined


updateMine :: Instr -> ConfM ()
updateMine = undefined

exec :: Instr -> ConfM ()
exec = undefined

initRobot :: Mine -> Robot
initRobot = undefined

run :: [Instr] -> Mine -> Mine
run = undefined

readLDM :: String -> IO (Either String Mine)
readLDM = undefined

readLCR :: String -> IO (Either String [Instr])
readLCR = undefined
