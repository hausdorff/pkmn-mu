{-# LANGUAGE GADTs #-}

module Pkmn (
  -- pkmn-mu map, map-related data structures, parsing routines.
    Square(..), Point(..), Map(..)
  , parseMap
  -- Edge querying.
  , EdgePattern(..)
  , matchPath, coords, (<->>), (<->)
  -- Finite state machine, FSM-related data structures, transformation routines.
  , EdgeSet(..), AdjList(..), EdgeIndex(..), Edge(..), Transition(..), TransExp(..)
  , mkEdgeIndex, getEdges, getEdge, (<|>), (<&>)
) where

import Prelude hiding (Left, Right)
import qualified Control.Monad.State as St
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

--------------------------------------------------
-- pkmn-mu data structures and parsing routines. -
--------------------------------------------------

--
-- Square.
--

data Square = Entrance | Exit | Wall | Grass | Ground
instance Read Square where
  readsPrec _ "E" = [(Entrance, "")]
  readsPrec _ "X" = [(Exit, "")]
  readsPrec _ "W" = [(Wall, "")]
  readsPrec _ "G" = [(Grass, "")]
  readsPrec _ " " = [(Ground, "")]
instance Show Square where
  show Entrance = "E"
  show Exit     = "X"
  show Wall     = "W"
  show Grass    = "G"
  show Ground   = " "
instance Eq Square where
  (==) Entrance Entrance = True
  (==) Exit Exit         = True
  (==) Wall Wall         = True
  (==) Grass Grass       = True
  (==) Ground Ground     = True
  (==) _ _               = False

--
-- Point.
--

data Point = Point {
    x      :: Int
  , y      :: Int
  , square :: Square
}
instance Show Point where
  show (Point x y sq) = show sq ++ " (" ++ show x ++ "," ++ show y ++ ")"
instance Eq Point where
  (==) (Point x1 y1 sq1) (Point x2 y2 sq2) = (x1, y1, sq1) == (x2, y2, sq2)
instance Ord Point where
  compare (Point x1 y1 _) (Point x2 y2 _) = compare (x1, y1) (x2, y2)

type Row  = V.Vector Point
type Rows = V.Vector Row
type Col  = V.Vector Point
type Cols = V.Vector Col

--
-- Map.
--

data Map = Map {
    rows     :: Rows
  , cols     :: Cols
  , entrance :: Point
  , exit     :: Point
  , index    :: EdgeIndex
}
instance Show Map where
  show m = List.intercalate "\n" $ rowStrings
    where rowStrings :: [String]
          rowStrings = V.toList $
            (rows m) >>= \row ->
            return $ V.toList $ row >>= V.fromList . show . square

parseMap :: [String] -> Maybe Map
parseMap rows = do
  (ent, ex, rows') <- parseRows Nothing Nothing (indexed paddedRows) []
  let cols =
        let rows'' = map V.toList (V.toList rows') in
        V.fromList $ map V.fromList (List.transpose rows'')
  return Map{
      rows     = rows'
    , cols     = cols
    , entrance = ent
    , exit     = ex
    , index    = mkEdgeIndex rows' cols
    }
  where paddedRows = map padRow ([paddingRow] ++ rows ++ [paddingRow])
          where maxLen     = List.foldl (\maxLen row -> max maxLen (length row)) 0 rows
                paddingRow = List.replicate (maxLen) 'W'
                padRow row = let rowLen = length row in case rowLen < maxLen of
                  False -> "W" ++ row ++ "W"
                  True  -> "W" ++ row ++ (List.replicate (maxLen - rowLen) 'W') ++ "W"

        indexed = zip [-1..]

        -- Parse the map, failing if there are too many entrances or exits.
        parseRows ent ex [] acc         = do
          ent' <- ent
          ex'  <- ex
          return (ent', ex', V.fromList $ List.reverse acc)
        parseRows ent ex ((y, row):rows) acc =
          case (parseRow y ent ex (indexed row) []) of
            Nothing                -> Nothing
            Just (ent', ex', row') -> parseRows ent' ex' rows (row':acc)
          where parseRow y ent      ex       []            acc = Just (ent, ex, V.fromList $ List.reverse acc)
                parseRow _ (Just _) _        ((_, 'E'):_)  _   = Nothing
                parseRow _ _        (Just _) ((_, 'X'):_)  _   = Nothing
                parseRow y ent      ex       ((x, sq):sqs) acc =
                  let sq' = read [sq] in
                  let p = Point x y sq' in case sq' of
                    Entrance -> parseRow y (Just p) ex sqs (p:acc)
                    Exit     -> parseRow y ent (Just p) sqs (p:acc)
                    _        -> parseRow y ent ex sqs (p:acc)

collapse' :: Map -> Maybe Map
collapse' m = do
  Nothing
  -- (inEdges, outEdges) <- getEdges (index m) (x ex) (y ex)
  -- case collapsed inEdges outEdges of
  --   Nothing    -> Nothing
  --   Just True  -> Nothing -- TODO: Replace with `Just`.
  --   Just False -> Nothing -- TODO: Recurse

  -- * If the only edges are E->E or E->X, return. If not, select an edge from E
  --   to some random node b at random (not including those edges).
  -- * Partition edge sets into a few groups:
  --   * Edges that go from X->b->Y
  --   * Edges that go from X->b->X
  --   * Edges that go from b->b
  -- * For each of these groups, apply the following transformation:
  --   * Remove the edge X->b->Y, and replace it with a direct edge
  --     X->(b->b)*->Y
  --   * Remove the edge X->b->X, and replace it with a direct edge
  --     X->(b->b)*->Y
  --   * Remove b->b
  -- * Recurse.
  where ent = entrance m
        ex  = exit m

        -- TODO: Need to check that no edges are coming into the exit node in
        -- `inEdges`.
        collapsed inEdges outEdges =
          let len = length inEdges in
          let pattern = coords ent <->> coords ex in
          Nothing
          -- let hasEntToExit = hasEdge inEdges ent ex in
          -- let hasEntToEnt = hasEdge inEdges ent ent in
          -- case (len, hasEntToExit, hasEntToExit) of
          --   (0, _, _)       -> Nothing
          --   (1, True, _)    -> Just True
          --   (2, True, True) -> Just True
          --   _               -> Just False

        -- Find all edges X->p, p->Y such that X->p->Y.
        throughEdges inEdges outEdges p = S.empty

        -- Find all edges X->p, p->X such that X->p->X.
        loopEdges inEdges outEdges p = S.empty

coords :: Point -> EdgePattern Terminal
coords (Point x y _) = Coords x y

data Terminal

data EdgePattern a where
  Coords :: Int -> Int -> EdgePattern a
  From   :: EdgePattern a -> EdgePattern b -> EdgePattern c
  FromTo :: EdgePattern a -> EdgePattern b -> EdgePattern c

infixr 2 <->>
(<->>) :: EdgePattern a -> EdgePattern b -> EdgePattern c
(<->>) ep1 ep2 = From ep1 ep2

infixr 2 <->
(<->) :: EdgePattern a -> EdgePattern b -> EdgePattern c
(<->) ep1 ep2 = FromTo ep1 ep2

matchPath :: Map -> EdgePattern a -> Maybe (V.Vector Point)
matchPath m (Coords x y) = do
  pt <- getPoint m x y
  return $ V.singleton pt

matchPath m (From ep1 ep2) = do
  match1 <- matchPath m ep1
  match2 <- matchPath m ep2
  let last = V.last match1
  let first = V.head match2

  (_, outEdges) <- getEdges (index m) (x last) (y last)
  _             <- getEdge outEdges last first
  return $ match1 V.++ match2

matchPath m (FromTo ep1 ep2) = do
  -- This is basically the reverse of the `matchPath` that runs on `From`.
  match2 <- matchPath m ep2
  match1 <- matchPath m ep1
  let last = V.last match1
  let first = V.head match2

  (_, outEdges) <- getEdges (index m) (x last) (y last)
  _             <- getEdge outEdges last first

  matchPath m (From ep1 ep2)

getPoint :: Map -> Int -> Int -> Maybe Point
getPoint m x y = do
  row <- (rows m) V.!? (y+1)
  row V.!? (x+1)


----------------------------------------------
-- FSM data structures and parsing routines. -
----------------------------------------------

type EdgeSet = S.Set Edge

hasEdge es p1 p2 = S.member (Edge p1 p2 $ Lit Left) es

getEdge :: EdgeSet -> Point -> Point -> Maybe Edge
getEdge es p1 p2 = do
  i <- S.lookupIndex (Edge p1 p2 $ Lit Left) es
  return $ S.elemAt i es

getEdge' :: EdgeSet -> (Int, Int) -> (Int, Int) -> Maybe Edge
getEdge' es (x1, y1) (x2, y2) = getEdge es (Point x1 y1 Entrance) (Point x2 y2 Entrance)

--
-- AdjList.
--

-- type AdjList = V.Vector (V.Vector EdgeSet)
type AdjList = M.Map Int (M.Map Int EdgeSet)

emptyAdjList rows cols = V.replicate rows $ V.replicate cols $ S.empty

getEdges' :: AdjList -> Int -> Int -> Maybe EdgeSet
getEdges' rows x y = do
  row <- M.lookup y rows
  M.lookup x row

--
-- EdgeIndex.
--

data EdgeIndex = EdgeIndex{
    inEdges  :: AdjList
  , outEdges :: AdjList
}

mkEdgeIndex :: Rows -> Cols -> EdgeIndex
mkEdgeIndex rows cols =
  let idx = mkEdgeIndex' M.empty M.empty in
  let idx' = V.foldl addRow idx rows in
  V.foldl addCol idx' cols
  where addRow idx row        = V.foldl addHoriz idx (V.zip row $ V.tail row)
        addCol idx col        = V.foldl addVert idx (V.zip col $ V.tail col)
        addHoriz idx (p1, p2) =
          let idx' = add idx p1 p2 $ Lit Right in
          add idx' p2 p1 $ Lit Left
        addVert idx (p1, p2)  =
          let idx' = add idx p1 p2 $ Lit Down in
          add idx' p2 p1 $ Lit Up
        add idx p1 p2 texp    =
          let Point x1 y1 sq1 = p1 in
          let Point x2 y2 sq2 = p2 in case (sq1, sq2) of
            (Wall, _) -> idx
            (_, Wall) -> addEdge idx p1 p1 texp
            (_, _)    -> addEdge idx p1 p2 texp

mkEdgeIndex' outEdges inEdges = EdgeIndex{
    outEdges = outEdges
  , inEdges = inEdges
}

getEdges :: EdgeIndex -> Int -> Int -> Maybe (EdgeSet, EdgeSet)
getEdges idx x y = do
  inEdges <- getEdges' (inEdges idx) x y
  outEdges <- getEdges' (outEdges idx) x y
  return (inEdges, outEdges)

addEdge :: EdgeIndex -> Point -> Point -> TransExp -> EdgeIndex
addEdge idx p1@(Point x1 y1 _) p2@(Point x2 y2 _) texp = idx'
  where idx'         =
          let outEs = add (outEdges idx) x1 y1 in
          let inEs  = add (inEdges idx) x2 y2 in
          mkEdgeIndex' outEs inEs
        add :: AdjList -> Int -> Int -> AdjList
        add rows x y =
          let row   = M.findWithDefault M.empty y rows in
          let edges' = S.insert edge edges
                where edges = M.findWithDefault S.empty x row
                      edge = case getEdge edges p1 p2 of
                        Nothing                 -> Edge p1 p2 texp
                        Just (Edge p1 p2 texp') -> Edge p1 p2 $ texp <|> texp'
          in
          let row' = M.insert x edges' row in
          M.insert y row' rows

--
-- Edge.
--

data Edge = Edge Point Point TransExp
instance Show Edge where
  show (Edge p1 p2 t) = concat ["[", show p1, "]", " --( ", show t, " )--> ", "[", show p2, "]"]
instance Eq Edge where
  (==) (Edge p1 p2 _) (Edge p3 p4 _) = (p1, p2) == (p3, p4)
instance Ord Edge where
  compare (Edge p1 p2 _) (Edge p3 p4 _) = compare (p1, p2) (p3, p4)

--
-- Transition.
--

data Transition = A | B | Left | Right | Up | Down
instance Read Transition where
  readsPrec _ "A" = [(A, "")]
  readsPrec _ "B" = [(B, "")]
  readsPrec _ "◀" = [(Left, "")]
  readsPrec _ "▶" = [(Right, "")]
  readsPrec _ "▲" = [(Up, "")]
  readsPrec _ "▼" = [(Down, "")]
instance Show Transition where
  show A     = "A"
  show B     = "B"
  show Left  = "◀"
  show Right = "▶"
  show Up    = "▲"
  show Down  = "▼"

--
-- TransExp.
--

data TransExp =
    Lit Transition
  | Or TransExp TransExp
  | And TransExp TransExp
  | Star TransExp
instance Show TransExp where
  show (Lit t)     = show t
  show (Or e1 e2)  = concat [show e1, "|", show e2]
  show (And e1 e2) = concat [show' e1, show' e2]
    where show' e = case e of
            Or _ _ -> concat ["(", show e, ")"]
            _      -> show e
  show (Star e)    = case e of
    Lit t  -> concat [show t, ")*"]
    Star _ -> show e
    _      -> concat ["(", show e, ")*"]

(<|>) :: TransExp -> TransExp -> TransExp
(<|>) e1 e2 = Or e1 e2

(<&>) :: TransExp -> TransExp -> TransExp
(<&>) e1 e2 = And e1 e2
