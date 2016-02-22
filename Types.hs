{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Test.QuickCheck
import Data.DeriveTH

-- 0 - ghci -fwarn-incomplete-patterns / ghci -Wall (more strict)

-- Intro.
-- Let me get some boos. I wanna hear you boo me. I thrive on boos.
-- I'm not going to talk about dynamic vs static today.
-- I'm just going to show you how having a capable type system can improve quality and aid testing.

-- 1 - Type inference - look Ma, no type signature!
-- One of the common complaints for not having static typing is the type signature is noisy and hides the semantics.
-- Meet Hindley-Milner.
calculateTax amount taxRate = amount * taxRate

-- NOTE: Not to be confused with dynamic typing

-- Try calling it with a different type, say String
calculateAfterTax amount tax = amount + tax

-- This would work in some languages, I'm not gonna name names *cough*JavaScript*cough* -> 1 + "1" = "11"
--getInvoiceAmount = calculateAfterTax 1 "1"

-- Whaaaaaaa?
-- Compiler FTW!

-- You can add type signature as documentation
calculateTaxWithTypes :: Num a => a -> a -> a
--calculateTaxWithTypes :: Float -> Float -> Float
calculateTaxWithTypes amount taxRate = amount * taxRate

-- Often the type signature tells you exactly what it does, e.g. lookup function



-- 2 - ADT - Let's do some algebra!

-- Product type
data Customer = Customer {
  firstName  :: String,
  lastName   :: String,
  age        :: Int,
  netWorth   :: Float,
  likesTrump :: Bool
}

-- Sum type
--data Maybe a = Nothing | Just a

-- Look Ma, no NPE!
isKey key dict = case (lookup key dict) of
                      Nothing -> False
                      Just _  -> True

-- Make illegal states unrepresentable - Yaron Minsky
-- Use types to make it hard, if not impossible, to make mistakes

-- Recursive sum type
-- Describes data structures in a succinct way
data List a = EmptyList | Cons a (List a)

data Tree a = EmptyNode | Node a (Tree a) (Tree a)

traverse' EmptyNode           = []
traverse' (Node a left right) = a : (traverse' left) ++ (traverse' right)

-- There are other types, like unit, etc, but we're not gonna talk about them today.

-- Domain modeling - on to something dear to us...
-- Start with the enumeration. You can look at ADTs as enums on steroids.
-- Then add the record types and
-- Add Eq, Ord, typeclasses when doing PBT
data Invoice = IssuedInvoice    { emailed :: Bool }
               | EmailedInvoice { viewed  :: Bool }
               | ViewedInvoice  { paid    :: Bool }
               | PaidInvoice
               deriving (Eq, Ord, Show)

-- Define the state machine.
-- We don't care what trans is at this point, all we know is it transitions an invoice from one state to the next.
-- Dependency injection the functional way!
runMachine trans invoice = case (trans invoice) of
                                PaidInvoice -> PaidInvoice
                                _           -> runMachine trans invoice

transition invoice@(IssuedInvoice emailed) = case emailed of
                                                  True  -> EmailedInvoice { viewed=False }
                                                  False -> invoice
transition invoice@(EmailedInvoice viewed) = case viewed of
                                                  True  -> ViewedInvoice { paid=False }
                                                  False -> invoice
transition invoice@(ViewedInvoice paid)    = case paid of
                                                  True  -> PaidInvoice
                                                  False -> invoice
-- Intentionally leave this one out to show non-exhaustive pattern matching warning
-- transition PaidInvoice = PaidInvoice

-- Whaaaa? How can the compiler be so smart?!

-- PBT example - made possible by types!

-- Property 1 - the result of the transition has to be equal to or bigger than the current state
derive makeArbitrary ''Invoice

-- verboseCheck prop_transition
prop_transition_result_is_equal_or_bigger before = let after = transition before
                                                   in after >= before

-- BAMM! 100% code coverage!
-- We can go further by

-- 3 - GADT

-- Topic for another time as it's very abstract and I didn't have time to come up with a relevant example.
-- Briefly mention the SafeList example.


-- Summary
-- 1 - Types are not your enemy, types are your best friend.
-- 2 - Type inference reduces "noise" and enables rapid prototyping without interrupting the flow
-- 3 - ADT is a great tool for domain modeling and designing programs
-- 4 - Type system doesn't replace tests, it complements them.
-- When used wisely, it makes illegal states unrepresentable (NPE, list out of bounds, etc) and eliminates many unnecessary tests.
-- When combined with PBT, it becomes even more indispensable.
-- 5 - Types systems are getting better as type theory progresses.
-- Dependent types allow behavioral correctness to be built into the type signature.
-- Briefly show Agda and how you can have validation built into the type system, e.g. the tax rate has to be between 0 and 100.