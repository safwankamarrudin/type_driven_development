module Types where

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
  firstName :: String,
  lastName  :: String,
  age       :: Int,
  netWorth  :: Float
}

-- Sum type
--data Maybe a = Nothing | Just a

-- Look Ma, no NPE!
isKey key dict = case (lookup key dict) of
                      Nothing -> False
                      Just x  -> True

-- Recursive sum type
data List a = Empty | Cons a (List a)

-- There are other types, like unit, etc, but we're not gonna talk about them today. Look them up.

-- Domain modeling - on to something dear to us...
data Invoice = IssuedInvoice    { emailed :: Bool }
               | EmailedInvoice { viewed :: Bool }
               | ViewedInvoice  { paid :: Bool }
               | PaidInvoice
               deriving (Eq, Ord, Show)

fromIssued invoice = case (emailed invoice) of
                          True  -> EmailedInvoice { viewed=False }
                          False -> invoice

fromEmailed invoice = case (viewed invoice) of
                           True  -> ViewedInvoice { paid=False }
                           False -> invoice

fromViewed invoice = case (paid invoice) of
                          True  -> PaidInvoice
                          False -> invoice

--Found a limitation in GHC!
-- invalidTransition = fromIssued ViewedInvoice { paid=True }

runMachine transition invoice = case (transition invoice) of
                                     PaidInvoice -> PaidInvoice
                                     _           -> runMachine transition invoice

invoiceTransition invoice = case invoice of
                                 IssuedInvoice i  -> fromIssued invoice
                                 EmailedInvoice i -> fromEmailed invoice
                                 ViewedInvoice i  -> fromViewed invoice
                                 --Commented out to show non-exhaustive pattern matching - PaidInvoice      -> PaidInvoice

-- PBT example - made possible by types!

calculateInvoiceTax :: Invoice -> Float
calculateInvoiceTax invoice = 42

-- Conclusion: Type inference allows you to do domain design with very little friction

-- TODO: Show how type signature describes behavior AND enforces correctness




-- 3 - GADT - Let's do more math! More. Moooooooree!