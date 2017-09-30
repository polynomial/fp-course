{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative where

import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Applicative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
--
-- * The law of left identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of right identity
--   `∀x. x <*> pure id ≅ x`
class Functor f => Applicative f where
  pure ::
    a -> f a
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$$> (ExactlyOne 2)
-- ExactlyOne 3
--
-- >>> (+1) <$$> Nil
-- []
--
-- >>> (+1) <$$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$$>) = (<$>)

-- | Insert into ExactlyOne.
--
-- prop> pure x == ExactlyOne x
--
-- >>> ExactlyOne (+10) <*> ExactlyOne 8
-- ExactlyOne 18
instance Applicative ExactlyOne where
  pure ::
    a
    -> ExactlyOne a
  pure = ExactlyOne
  (<*>) :: 
    ExactlyOne (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<*>) (ExactlyOne f) (ExactlyOne a) = ExactlyOne (f a)
-- OMG I LOVE IT

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where
  pure ::
    a
    -> List a
  pure a = (a :. Nil)
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  (<*>) Nil _ = Nil
  (<*>) (f :. t) l = (map f l) ++ (<*>) t l

-- | Insert into an Optional.
--
-- prop> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure = Full
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Full f) (Full a) = Full (f a)

-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure a _ = a 
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  (<*>) tab ta t = tab t (ta t)
-- i think this can be rewritten as:
-- pure :: a -> t -> a

-- i think this can be rewritten as

--  (<*>) :: t -> (a -> b) -> t -> a -> t -> b
-- or
--  (<*>) :: t -> (a -> b) -> ta -> tb
--  (<*>) :: t -> ab -> ta -> tb
-- ghci
--  (<*>) :: (t -> a -> b) -> (t -> a) -> t -> b
-- >>> ((+) <*> (+5)) 3
-- 11
-- 3 +5 + 
-- t ab t a t -> b
-- +5 + 3
-- ab (t0 t1)
--  (<*>) t ab ta = ab (ta t)
--  (<*>) tab ta = tab <*> ta

-- | Apply a binary function in the environment.
-- what is an environment?
--
-- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
-- ExactlyOne 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  Applicative f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
-- lift2 :: (a -> b -> c) -> f a -> f b -> f c

lift2 abc fa fb = abc <$> fa <*> fb

-- | Apply a ternary function in the environment.
--
-- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
-- ExactlyOne 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Applicative f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 abcd fa fb fc = abcd <$> fa <*> fb <*> fc

-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
-- ExactlyOne 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Applicative f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 abcde fa fb fc fd = abcde <$> fa <*> fb <*> fc <*> fd

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
-- (*>) fa fb = lift2 (\_ -> \b -> b) fa fb
(*>) = lift2 (\_ -> \b -> b)
-- >>> lift2 (+) Empty (Full 8)
--(*>) fa fb = lift2 (\_ -> b -> b) fa fb

--(*>) fa = (<*>) (\_ -> fb)

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Applicative f =>
  f b
  -> f a
  -> f b
(<*) = lift2 (\a -> \_ -> a)

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence = foldRight (lift2 (:.)) (pure Nil)

-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (ExactlyOne "hi")
-- ExactlyOne ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA i fa = sequence (replicate i fa)
--replicateA _ fa = foldRight (lift2 (:.)) (pure Nil) (fa :. Nil)
-- replicateA
--   ExactlyOne:       FAIL
--     expected: ExactlyOne ["hi","hi","hi","hi"]
--      but got: ExactlyOne ["hi"]
--   Optional - Full:  FAIL
--     expected: Full ["hi","hi","hi","hi"]
--      but got: Full ["hi"]
--   Optional - Empty: OK
--   (->):             FAIL
--     expected: [10,10,10,10]
--      but got: [10]
--   List:             FAIL
--     expected: ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
--      but got: ["a","b","c"]


-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering _ Nil = (pure Nil)

-- working through sequence because im not sure i fully grok it:
-- sequence ::
--   Applicative f =>
--   List (f a)
--   -> f (List a)
-- sequence = foldRight (lift2 (:.)) (pure Nil)
--
-- lift2 ::
--   Applicative f =>
--   (a -> b -> c)
--   -> f a
--   -> f b
--   -> f c
-- lift2 abc fa fb = abc <$> fa <*> fb
--
-- foldRight :: (a -> b -> b) -> b -> List a -> b
-- foldRight _ b Nil      = b
-- foldRight f b (h :. t) = f h (foldRight f b t)
--
-- data List t =
--   Nil
--   | t :. List t
--   deriving (Eq, Ord)
-- -- Right-associative
-- infixr 5 :.
--
--
-- foldRight :: (a -> b -> b) -> b -> List a -> b
-- sequence :: Course.Applicative.Applicative f => List (f a) -> f (List a)
-- lift2 :: Course.Applicative.Applicative f => (a -> b -> c) -> f a -> f b -> f c
--
-- sequence            = foldRight (lift2 (:.)) (pure Nil)
-- sequence l u        = foldRight (lift2 (:.)) (pure Nil) l
-- sequency List (f a) = foldRight (lift2 (:.)) (pure Nil) List (f a)
-- foldRight :: (a -> b -> b) -> b -> List a -> b
-- lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--
-- lift2 (:.) h (foldRight f b t)
--
--
--
-- i dont change it in the center
-- i tell it what i want on the input
-- and then i tell it what i want on the output
-- and i find things that match that, how it mutates, i dont care or know!
--



-- List a -> h :. t
-- if (a -> f bool) h is true
-- then f h
-- else 

-- (a -> f b) -> List a -> f (List a)
-- (a -> f b) -> t a -> f (t a)
-- this led me to discover traverse
-- the definition of traverse was helpful:
-- Map each element of a structure to an action, evaluate these actions from left to right, and collect the results. For a version that ignores the results see traverse_.


--filtering afb (h :. t) = (afb <$> h) :. (filtering afb t)
-- the journey of a thousand cuts begins with one sheet of paper

--filtering afb (h :. t) =
--  if afb h
--    then h :. filtering afb t
--    else filtering afb t
--    • Couldn't match expected type ‘Bool’ with actual type ‘f Bool’


-- filtering afb l = map afb l
--      Expected type: f (List a)
--        Actual type: List (f Bool)

-- filtering afb l = foldRight afb (pure Nil) l
--       Expected type: a -> f (List a) -> f (List a)
--        Actual type: a -> f Bool


-- filtering afb l = foldRight (lift2 (:.) (replicateA (length l) afb)) (pure Nil) l
--      Expected type: (a -> List (List (f Bool)))
--                     -> f (List a) -> f (List a)
--        Actual type: (a -> List (List (f Bool)))
--                     -> a -> List (List (f Bool))


-- filtering afb l = pure (filter afb l)
--     • Couldn't match type ‘f Bool’ with ‘Bool’
--      Expected type: a -> Bool
--        Actual type: a -> f Bool

-- filtering afb = foldRight (twiceOptional (:.)) (pure Nil)
--      Expected type: List a -> f (List a)
--        Actual type: List (Optional a) -> Optional (List a)


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist

(>>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
