module Text.Regex.PCRE2.Wrap.Helper
       (
         cSizeToInt
       , intToCSize
       , intToCInt
       , cIntToInt
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
