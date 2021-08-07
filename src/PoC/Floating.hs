{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- %%
-- %:set -XFlexibleInstances
-- %:set -XFlexibleContexts
-- %-

module PoC.Floating where

import Prelude

-- %%
data Q = Q
  deriving (Eq, Show)

-- %%
instance Num Q where
  Q + Q = Q
  Q * Q = Q
  abs Q = Q
  signum Q = Q
  negate Q = Q
  fromInteger _ = Q

-- %%
instance Fractional Q where
  Q / Q = Q
  fromRational _ = Q

-- %%
type NonNegativeNumber = ([Q], [Q])

-- %%
instance Num NonNegativeNumber where
  (l, r) * (l', r') = ([x * x' | x <- l, x' <- l'], [y * y' | y <- r, y' <- r'])

-- %{
zero, infty :: NonNegativeNumber
zero = ([], [Q])
infty = ([Q], [])

-- %}

demo1 :: NonNegativeNumber
demo1 =
  -- %%
  zero * infty -- expect: = ([],[])

-- %%
instance Fractional NonNegativeNumber where
  (l, r) / (l', r') = ([x / x' | x <- l, x' <- l'], [y / y' | y <- r, y' <- r'])

-- %-

demo2 :: NonNegativeNumber
demo2 =
  -- %%
  zero / zero
