module Text.Regex.PCRE2.Wrap.Helper
       (
         cSizeToInt
       , intToCSize
       , intToCInt
       , cIntToInt
       , first3
       , handleArrayOfEithers
) where

import Foreign.C.Types (CSize, CInt)

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

first3 :: (a, b, c) -> a
first3 (x, _, _) = x

handleArrayOfEithers :: [Either a b] -> Either a [b]
handleArrayOfEithers [] = Right []
handleArrayOfEithers (x : xs) = case x of
          (Left a) -> Left a
          (Right b) -> case handleArrayOfEithers xs of
                 (Left c) -> Left c
                 (Right d) -> Right $ b : d



