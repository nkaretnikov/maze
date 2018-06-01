{-# language ScopedTypeVariables #-}

module Maze where

import           Data.Set      (Set)
import qualified Data.Set      as Set
import qualified Data.Maybe    as Maybe
import qualified System.Random as Random

import           Canvas.Types
import           Cell
import           Cell.Types
import qualified Stack         as Stack
import           Stack.Types


-- | If a value is not in a visited list, then it's not been visited.
notVisitedNeighbors
  :: CanvasHeight -> CanvasWidth -> Cell -> Set Cell -> [Cell]
notVisitedNeighbors canvasH canvasW (Coord x y) visited =
  Maybe.catMaybes $ f <$> nbrs
    where
      nbrs =
        -- Each cell is surrounded by (implicit) borders.  Jump over a border to
        -- get to the actual cell.
        [ (-2, 0)  -- left
        , ( 2, 0)  -- right

        , ( 0, 2)  -- down
        , ( 0,-2)  -- up
        ]

        -- Disallow diagonal movement.
        {-
        , (-2, 2)  -- lower left
        , ( 2, 2)  -- lower right

        , (-2,-2)  -- upper left
        , ( 2,-2)  -- upper right
        ]
        -}
      f (x', y') =
        let coord = Coord (x + x') (y + y')
        in if Set.member coord visited
           then Nothing
           else if isValidCell canvasH canvasW coord
                then Just coord
                else Nothing

removeWall :: Cell -> Cell -> Cell
removeWall (Coord x1 y1) (Coord x2 y2)
  = Coord (nbrCoord x1 x2) (nbrCoord y1 y2)
  where
    nbrCoord n m =
      if n > m
      then n - 1
      else if m > n
           then m - 1
           else n  -- n == m

-- | https://en.wikipedia.org/wiki/Maze_generation_algorithm#Recursive_backtracker
maze :: CanvasHeight -> CanvasWidth -> IO (Set Cell)
maze canvasH canvasW =
  -- Make the initial cell the current cell and mark it as visited.
  let currentCell = Coord 0 0
      cellStack = Stack []
      visitedCells = Set.empty

      -- While there are unvisited cells.
      loop :: Stack Cell -> Set Cell -> Cell -> IO (Set Cell)
      loop stack cells cur = do
        -- If the current cell has any neighbors which have not
        -- been visited.
        let nbrs = notVisitedNeighbors canvasH canvasW cur cells

        if length nbrs > 0
        then do
          -- Chose randomly one of the unvisited neighbors.
          -- XXX: The second 'randomR' argument must not be less than the
          -- first one: relies on the length check above.
          ix :: Int <- Random.getStdRandom $ Random.randomR (0, length nbrs - 1)
          -- To make it deterministic (should generate a snake-like shape):
          -- let ix = 0 :: Int

          -- XXX: May fail at runtime if the index is not correct.
          let nbr = nbrs !! ix

          -- Push the current cell to the stack.
          -- Remove the wall between the current cell
          -- and the chosen cell.
          let stack'  = Stack.push stack cur
              gate    = removeWall cur nbr
              cells'  = Set.insert gate cells
              cells'' = Set.insert cur cells'
              -- Make the chosen cell the current cell and mark it as visited.
              -- Cells are marked as visited by inserting them into the
              -- 'cells' map.
              cells''' = Set.insert nbr cells''
              cur' = nbr

          loop stack' cells''' cur'
        else
          -- If stack is not empty.
          if not (Stack.isEmpty stack)
          then do
            -- Pop a cell from the stack.
            -- Make it the current cell.
            let (cur', stack') = Stack.pop stack

            loop stack' cells cur'
          else return cells

  in loop cellStack visitedCells currentCell
