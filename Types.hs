{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Test.QuickCheck
import Data.DeriveTH

-- 0 - ghci -fwarn-incomplete-patterns / ghci -Wall (more strict)

-- 1 - Type inference - look Ma, no type signature!
calculateTax amount taxRate = amount * taxRate

-- NOTE: Not to be confused with dynamic typing

-- Try calling it with a different type, say String
calculateAfterTax amount tax = amount + tax

--getInvoiceAmount = calculateAfterTax 666 "abomination"

-- NOTE: Compiler FTW!

-- You can add type signature as documentation
calculateTaxWithTypes :: Num a => a -> a -> a
--calculateTaxWithTypes :: Float -> Float -> Float
calculateTaxWithTypes amount taxRate = amount * taxRate





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

-- Look Ma, no NPE! Make illegal states unrepresentable - Yaron Minsky
isKey key dict = case (lookup key dict) of
                      Nothing -> False
                      Just _  -> True

-- Recursive sum type
data List a = Empty | Cons a (List a)

-- There are other types, like unit, etc, but we're not gonna talk about them today. Look them up.

-- Domain modeling - on to something dear to us...
-- Start with the enumeration
-- Then add the record types and typeclasses
data Invoice = IssuedInvoice    { emailed :: Bool }
               | EmailedInvoice { viewed  :: Bool }
               | ViewedInvoice  { paid    :: Bool }
               | PaidInvoice
               deriving (Eq, Ord, Show)

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
transition invoice@(ViewedInvoice paid) = case paid of
                                               True  -> PaidInvoice
                                               False -> invoice
-- transition PaidInvoice = PaidInvoice

-- PBT example - made possible by types!

-- Property 1 - the result of the transition has to be equal to or bigger than the current state
derive makeArbitrary ''Invoice

-- verboseCheck prop_transition
prop_transition before = let after = transition before
                         in after >= before

-- TODO: Show how type signature describes behavior AND enforces correctness




-- 3 - GADT - Let's do more math! More. Moooooooree!



-- Summary
-- 1 - Types are not your enemy, types are your best friend.
-- 2 - Type inference reduces "noise" and enables rapid prototyping without interrupting the flow
-- 3 - ADT is a great tool for domain modeling and designing programs
-- 4 - Type system doesn't replace tests, it complements them.
-- When used wisely, it makes illegal states unrepresentable (NPE, list out of bounds, etc) and eliminates many unnecessary tests.
-- When combined with PBT, it becomes even more indispensable.
-- 5 - Types systems are getting better as type theory progresses.
-- Dependent types allow behavioral correctness to be built into the type signature.