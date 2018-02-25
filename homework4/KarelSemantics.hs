module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)
-- Name:    Mark Bereza
-- ONID ID: berezam

import KarelSyntax
import KarelState
import KarelExamples

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = (getFacing r) == c
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r


-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      _ _ r = Done r
stmt Move          _ w r = let p = relativePos Front r
                           in if isClear p w
                                 then OK w (setPos p r)
                                 else Error ("Blocked at: " ++ show p)
stmt PickBeeper    _ w r = let p = getPos r
                           in if hasBeeper p w
                                 then OK (decBeeper p w) (incBag r)
                                 else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper     _ w r = let p = getPos r
                           in if isEmpty r
                                 then Error ("No beeper to put.")
                                 else OK (incBeeper p w) (decBag r)
stmt (Turn d)      _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call m)      d w r = case lookup m d of
                             Just s  -> stmt s d w r
                             Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) d w r = if i > 0
                             then case stmt s d w r of
                                    OK w' r' -> stmt (Iterate (i-1) s) d w' r'
                                    res      -> res
                             else OK w r
stmt (If t s1 s2)  d w r = if test t w r
                             then stmt s1 d w r
                             else stmt s2 d w r
stmt (While t s)   d w r = if test t w r
                             then case stmt s d w r of
                                    OK w' r' -> stmt (While t s) d w' r'
                                    res      -> res
                             else OK w r
stmt (Block b)  d w r = case b of
                          []   -> OK w r
                          s:ss -> case stmt s d w r of
                                    OK w' r' -> stmt (Block ss) d w' r'
                                    res      -> res

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
