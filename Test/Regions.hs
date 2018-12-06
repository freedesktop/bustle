{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad (when)
import Data.List (sort, group)
import Data.Maybe (isNothing, isJust)
import System.Exit (exitFailure)

import Bustle.Regions

newtype NonOverlappingStripes = NonOverlappingStripes [Stripe]
  deriving
    (Show, Eq, Ord)

paste :: [a] -> [(a, a)]

instance Arbitrary NonOverlappingStripes where
    arbitrary = do
        xs <- listOf1 arbitrary
        ys <- vectorOf (length xs) arbitrary

        nub . sort 


        xs <- nub . sort . map (getNonNegative) <$> listOf1 arbitrary

        heights <- 


        -- Generate dense stripes sometimes
        let g :: Gen Double
            g = frequency [(1, return 1.0), (7, choose (0.0, 1.0))]

        rs <- vectorOf (length tops) (choose (0.0, 1.0))

        let stripes = zipWith3 (\t1 t2 r -> Stripe t1 (t1 + ((t2 - t1) * r)))
                               tops (tail tops) rs
        return $ NonOverlappingStripes stripes

newtype ValidRegions a = ValidRegions (Regions a)
  deriving
    (Show, Eq, Ord)

instance (Eq a, Arbitrary a) => Arbitrary (ValidRegions a) where
    arbitrary = do
        NonOverlappingStripes stripes <- arbitrary
        values <- vector (length stripes) `suchThat` unique
        return $ ValidRegions (zip stripes values)
      where
        unique = all (== 1) . map length . group

instance (Eq a, Arbitrary a) => Arbitrary (RegionSelection a) where
    arbitrary = do
        ValidRegions rs <- arbitrary
        return $ regionSelectionNew rs

prop_NonOverlapping_generator_works (NonOverlappingStripes ss) = all coherent ss && nonOverlapping ss
  where
    coherent s = stripeTop s < stripeBottom s

prop_InitiallyUnselected rs = isNothing $ rsCurrent rs
prop_UpDoesNothing rs = isNothing $ rsCurrent $ regionSelectionUp rs

prop_DownDoesNothing vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
        let final = last regions
            rs'   = regionSelectionLast rs
        in
            rsCurrent (regionSelectionDown rs') == Just final

prop_DownWorks vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
        rsCurrent (regionSelectionDown rs) == Just (head regions)

withRegions :: Testable t
            => ValidRegions a
            -> (RegionSelection a -> t)
            -> t
withRegions (ValidRegions regions) f = f (regionSelectionNew regions)

prop_UpdateToFirst :: (Eq a)
                  => ValidRegions a
                  -> Bool
prop_UpdateToFirst vr@(ValidRegions regions) = withRegions vr $ \rs ->
    Just first == rsCurrent (regionSelectionUpdate y rs) &&
    null (rsBefore rs)
  where
    first@(Stripe top bottom, _) = head regions
    y = (top + bottom) / 2

prop_SelectFirst :: (Eq a)
                 => ValidRegions a
                 -> Bool
prop_SelectFirst vr@(ValidRegions regions) = withRegions vr $ \rs ->
    Just (head regions) == rsCurrent (regionSelectionFirst rs)

prop_SelectLast :: (Eq a)
                => ValidRegions a
                -> Bool
prop_SelectLast vr@(ValidRegions regions) = withRegions vr $ \rs ->
    Just (last regions) == rsCurrent (regionSelectionLast rs)

prop_UpdateToAny :: (Eq a, Show a)
                => ValidRegions a
                -> Property
prop_UpdateToAny vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
    forAll (elements regions) $ \ r@(s, _) ->
        rsCurrent (regionSelectionUpdate (midpoint s) rs) == Just r

shuffled :: [a] -> Gen [a]
shuffled [] = return []
shuffled xs = do
    i <- choose (0, length xs - 1)
    let x    = xs !! i
        pre  = take i xs
        post = drop (i + 1) xs
    xs' <- shuffled (pre ++ post)
    return (x:xs')

prop_UpdateToAll :: (Eq a, Show a)
                => ValidRegions a
                -> Property
prop_UpdateToAll vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
    forAll (shuffled regions) $ \regions' ->
        updateAndForward rs regions'
  where
    updateAndForward rs [] = True
    updateAndForward rs (x:xs) =
        let rs' = regionSelectionUpdate (midpoint (fst x)) rs
        in rsCurrent rs' == Just x && updateAndForward rs' xs

randomMutation :: Gen (RegionSelection a -> RegionSelection a)
randomMutation = do
    y <- arbitrary
    elements [ regionSelectionUp
             , regionSelectionDown
             , regionSelectionFirst
             , regionSelectionLast
             , regionSelectionUpdate y
             ]

randomMutations :: Gen (RegionSelection a -> RegionSelection a)
randomMutations =
    foldr (.) id <$> listOf randomMutation

prop_ClickAlwaysInSelection rs =
    forAll (fmap Blind randomMutations) $ \(Blind f) ->
      let
        rs' = f rs
      in
        isJust (rsCurrent rs') ==>
          let
            Just (Stripe top bottom, _) = rsCurrent rs'
            y = rsLastClick rs'
          in
            top <= y && y <= bottom

prop_SelectWorks :: (Eq a, Show a)
                 => ValidRegions a
                 -> Property
prop_SelectWorks vr@(ValidRegions regions) =
    withRegions vr $ \rs ->
    forAll (elements regions) $ \ r@(s, x) ->
      Just r == rsCurrent (regionSelectionSelect x rs)

prop_Append :: (Eq a, Show a)
            => ValidRegions a
            -> Property
prop_Append vr@(ValidRegions regions) =
    forAll (choose (0, length regions - 1)) $ \i ->
        let as = take i regions
            bs = drop i regions
        in regionSelectionAppend bs (regionSelectionNew as) == regionSelectionNew regions

prop_FlattenThenNewIsIdempotent
    :: (Eq a, Show a)
    => ValidRegions a
    -> Property
prop_FlattenThenNewIsIdempotent vr@(ValidRegions regions) =
    withRegions vr $ \rs -> property $
        regionSelectionNew (regionSelectionFlatten rs) == rs

-- Essential scary hack to make quickCheckAll work O_o
-- https://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck-All.html
return []
runTests = $quickCheckAll

main = do
    success <- runTests
    when (not success) exitFailure
