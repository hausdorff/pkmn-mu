import Prelude hiding (Left, Right)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

import Pkmn

main :: IO ()
main = hspec $ do
  describe "Square (==)" $ do
    it "returns true for the identity" $ do
      Entrance `shouldBe` Entrance
      Exit `shouldBe` Exit
      Wall `shouldBe` Wall
      Grass `shouldBe` Grass
      Ground `shouldBe` Ground

    it "returns false for comparisons between different constructors" $ do
      Entrance `shouldNotBe` Exit
      Entrance `shouldNotBe` Wall
      Entrance `shouldNotBe` Grass
      Entrance `shouldNotBe` Ground
      Wall `shouldNotBe` Grass
      Wall `shouldNotBe` Ground
      Grass `shouldNotBe` Ground

  describe "Point (==)" $ do
    it "returns true when all fields are equal" $ do
      (Point 1 1 Entrance) `shouldBe` (Point 1 1 Entrance)

    it "returns true when any field is unequal" $ do
      (Point 1 1 Entrance) `shouldNotBe` (Point 2 1 Entrance)
      (Point 1 1 Entrance) `shouldNotBe` (Point 1 2 Entrance)
      (Point 1 1 Entrance) `shouldNotBe` (Point 1 1 Exit)

  describe "Point compare" $ do
    it "returns LT when point is a lexicographic predecessor (ignoring square)" $ do
      compare (Point 1 1 Entrance) (Point 2 1 Entrance) `shouldBe` LT
      compare (Point 1 1 Entrance) (Point 1 2 Entrance) `shouldBe` LT
      compare (Point 1 1 Entrance) (Point 2 2 Entrance) `shouldBe` LT
      compare (Point 1 1 Entrance) (Point 2 1 Exit) `shouldBe` LT
      compare (Point 1 1 Exit) (Point 1 2 Entrance) `shouldBe` LT
      compare (Point 1 1 Entrance) (Point 2 2 Exit) `shouldBe` LT

    it "returns GT when point is a lexicographic successor (ignoring square)" $ do
      compare (Point 2 1 Entrance) (Point 1 1 Entrance) `shouldBe` GT
      compare (Point 1 2 Entrance) (Point 1 1 Entrance) `shouldBe` GT
      compare (Point 2 2 Entrance) (Point 1 1 Entrance) `shouldBe` GT
      compare (Point 2 1 Entrance) (Point 1 1 Exit) `shouldBe` GT
      compare (Point 1 2 Exit) (Point 1 1 Entrance) `shouldBe` GT
      compare (Point 2 2 Entrance) (Point 1 1 Exit) `shouldBe` GT

    it "returns EQ when points have equal coordinates (ignoring square)" $ do
      compare (Point 1 1 Entrance) (Point 1 1 Entrance) `shouldBe` EQ
      compare (Point 2 2 Entrance) (Point 2 2 Exit) `shouldBe` EQ

  let shouldNotBeEmpty match =
        Maybe.isJust match && V.length (Maybe.fromJust match) > 0 `shouldBe` True

  describe "matchPath" $ do
    it "matches trivial edge paths" $ do
      let Just m@Map{entrance=ent, exit=ex, index=idx} = parseMap ["EX"]
      let pattern1   = coords ent <->> coords ex
      let pattern1'  = coords ex <->> coords ent
      let pattern1'' = coords ent <-> coords ex

      let pattern2  = coords ent <->> coords ent
      let pattern2' = coords ent <-> coords ent

      let pattern3  = coords ex <->> coords ex
      let pattern3' = coords ex <-> coords ex

      shouldNotBeEmpty $ matchPath m pattern1
      shouldNotBeEmpty $ matchPath m pattern1'
      shouldNotBeEmpty $ matchPath m pattern1''

      shouldNotBeEmpty $ matchPath m pattern2
      shouldNotBeEmpty $ matchPath m pattern2'

      shouldNotBeEmpty $ matchPath m pattern3
      shouldNotBeEmpty $ matchPath m pattern3'

    it "fails to match paths between nodes that are not connected" $ do
      let Just m@Map{entrance=ent, exit=ex, index=idx} = parseMap ["EWX"]
      let pattern1   = coords ent <->> coords ex
      let pattern1'  = coords ex <->> coords ent
      let pattern1'' = coords ent <-> coords ex

      let pattern2  = coords ent <->> coords ent

      let pattern3 = coords ex <->> coords ex

      matchPath m pattern1 `shouldBe` Nothing
      matchPath m pattern1' `shouldBe` Nothing
      matchPath m pattern1'' `shouldBe` Nothing
      shouldNotBeEmpty $ matchPath m pattern2
      shouldNotBeEmpty $ matchPath m pattern3

    it "matches trivial multi-node paths" $ do
      let Just m@Map{entrance=ent, exit=ex, index=idx} = parseMap ["E X"]
      let pattern1 = coords ent <->> (Coords 1 0) <->> coords ex
      shouldNotBeEmpty $ matchPath m pattern1

    it "matches intermediate multi-node paths" $ do
      let Just m@Map{entrance=ent, exit=ex, index=idx} = parseMap ["EWX", "   "]
      let fwd = coords ent <->> (Coords 0 1) <->> (Coords 1 1) <->> (Coords 2 1) <->> coords ex
      let pattern1 = fwd <->> (Coords 2 1) <->> (Coords 1 1) <->> (Coords 0 1) <->> coords ent

      let fwd' = coords ent <-> (Coords 0 1) <-> (Coords 1 1) -- <-> (Coords 2 1) <-> coords ex
      -- let pattern1' = fwd' <-> (Coords 2 1) <-> (Coords 1 1) <-> (Coords 0 1) <-> coords ent
      shouldNotBeEmpty $ matchPath m fwd'

  describe "parseMap" $ do
    --
    -- NOTE: `Map` doesn't implement `Eq`, so we can't compare to `Maybe Map`.
    --

    it "trivial roundtrip works" $ do
      show (parseMap ["EX"]) `shouldBe` L.intercalate "\n" ["Just WWWW", "WEXW", "WWWW"]

    it "fails with multiple entrances" $ do
      show (parseMap ["EWEX"]) `shouldBe` "Nothing"

    it "fails with multiple exits" $ do
      show (parseMap ["EXWX"]) `shouldBe` "Nothing"

    it "fails with 0 exits" $ do
      show (parseMap ["E"]) `shouldBe` "Nothing"

    it "fails with 0 entrances" $ do
      show (parseMap ["X"]) `shouldBe` "Nothing"

  describe "Edge (==)" $ do
    it "returns true if all values are equal" $ do
      let e1 = (Edge (Point 1 1 Entrance) (Point 1 2 Exit) $ Lit Right)
      let e2 = (Edge (Point 1 1 Entrance) (Point 1 2 Exit) $ Lit Right)
      e1 `shouldBe` e2

    it "returns true if all values except transition are equal" $ do
      let e1 = (Edge (Point 1 1 Entrance) (Point 1 2 Exit) $ Lit Right)
      let e2 = (Edge (Point 1 1 Entrance) (Point 1 2 Exit) $ Lit Left)
      e1 `shouldBe` e2

  describe "getEdge" $ do
    it "trivially correctly retrieves an edge" $ do
      let (p1, p2) = ((Point 1 1 Entrance), (Point 1 2 Exit))
      let e1 = (Edge p1 p2 $ Lit Left)
      let es = S.singleton e1
      getEdge es p1 p2 `shouldBe` Just e1

  describe "EdgeIndex" $ do
    -- NOTE: `Edge` implements `Eq` as a comparison of the points in the edge,
    -- ignoring the `TransExp`. This is so that it works with data structures
    -- like `Set`, but it means that transitions aren't compared. Hence, we have
    -- `dummyTransExp`.
    let dummyTransExp = Lit A
    it "correctly creates edges for trivial maze" $ do
      let Just m = parseMap ["EX"]
      let idx = index m
      let Just (inEdges, outEdges) = getEdges idx 0 0

      outEdges `shouldBe` S.fromList [
          (Edge (Point 0 0 Entrance) (Point 1 0 Exit) (Lit Right))
        , (Edge (Point 0 0 Entrance) (Point 0 0 Entrance) dummyTransExp)
        ]
      inEdges `shouldBe` S.fromList [
          (Edge (Point 1 0 Exit) (Point 0 0 Entrance) (Lit Left))
        , (Edge (Point 0 0 Entrance) (Point 0 0 Entrance) dummyTransExp)
        ]

    it "correctly creates edges for trivial vertical maze" $ do
      let Just m = parseMap [
                "E"
              , "X"
              ]
      let idx = index m

      let Just (inEdges, outEdges) = getEdges idx 0 0
      outEdges `shouldBe` S.fromList [
          (Edge (Point 0 0 Entrance) (Point 0 1 Exit) (Lit Down))
        , (Edge (Point 0 0 Entrance) (Point 0 0 Entrance) dummyTransExp)
        ]
      inEdges `shouldBe` S.fromList [
          (Edge (Point 0 1 Exit) (Point 0 0 Entrance) (Lit Up))
        , (Edge (Point 0 0 Entrance) (Point 0 0 Entrance) dummyTransExp)
        ]

    it "correctly creates edges around a wall" $ do
      let Just m = parseMap [
                "EWX"
              , "   "
              ]
      let idx = index m
      let entrance = (Point 0 0 Entrance)
      let exit     = (Point 2 0 Exit)
      let ground1  = (Point 0 1 Ground)
      let ground2  = (Point 1 1 Ground)
      let ground3  = (Point 2 1 Ground)

      -- Entrance.
      let Just (inEdges, outEdges) = getEdges idx 0 0
      outEdges `shouldBe` S.fromList [
          (Edge entrance entrance (Lit Right))
        , (Edge entrance ground1 (Lit Down))
        ]
      inEdges `shouldBe` S.fromList [
          (Edge entrance entrance (Lit Right))
        , (Edge ground1 entrance (Lit Up))
        ]

      -- Wall.
      getEdges idx 1 0 `shouldBe` Nothing

      -- Exit.
      let Just (inEdges, outEdges) = getEdges idx 2 0
      outEdges `shouldBe` S.fromList [
          (Edge exit exit (Lit Left))
        , (Edge exit ground3 (Lit Down))
        ]
      inEdges `shouldBe` S.fromList [
          (Edge exit exit (Lit Left))
        , (Edge ground3 exit (Lit Up))
        ]

      -- Ground 1.
      let Just (inEdges, outEdges) = getEdges idx 0 1
      outEdges `shouldBe` S.fromList [
          (Edge ground1 entrance (Lit Up))
        , (Edge ground1 ground2 (Lit Right))
        , (Edge ground1 ground1 dummyTransExp)
        ]
      inEdges `shouldBe` S.fromList [
          (Edge entrance ground1 (Lit Down))
        , (Edge ground2 ground1 (Lit Left))
        , (Edge ground1 ground1 dummyTransExp)
        ]

      -- Ground 2.
      let Just (inEdges, outEdges) = getEdges idx 1 1
      outEdges `shouldBe` S.fromList [
          (Edge ground2 ground2 (Lit Up))
        , (Edge ground2 ground1 (Lit Left))
        , (Edge ground2 ground3 (Lit Right))
        , (Edge ground2 ground2 dummyTransExp)
        ]
      inEdges `shouldBe` S.fromList [
          (Edge ground2 ground2 (Lit Up))
        , (Edge ground1 ground2 (Lit Right))
        , (Edge ground3 ground2 (Lit Left))
        , (Edge ground2 ground2 dummyTransExp)
        ]

      -- Ground 3.
      let Just (inEdges, outEdges) = getEdges idx 2 1
      outEdges `shouldBe` S.fromList [
          (Edge ground3 exit (Lit Up))
        , (Edge ground3 ground2 (Lit Left))
        , (Edge ground3 ground3 dummyTransExp)
        ]
      inEdges `shouldBe` S.fromList [
          (Edge exit ground3 (Lit Down))
        , (Edge ground2 ground3 (Lit Right))
        , (Edge ground3 ground3 dummyTransExp)
        ]

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
