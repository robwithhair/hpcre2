{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Text.Regex.PCRE2.Wrap(
       compileRegex
) where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Word
import Data.Char

foreign import capi "pcre2.h value PCRE2_JIT_COMPLETE"
  c_PCRE2_JIT_COMPLETE :: CUInt

foreign import ccall "pcre2.h pcre2_compile_8"
  c_pcre2_compile :: Ptr CChar -> CSize -> CUInt -> Ptr CInt -> Ptr CSize -> Ptr () -> IO (Ptr Code)

foreign import ccall "pcre2.h pcre2_jit_compile_8"
  c_pcre2_jit_compile :: Ptr Code -> CUInt -> IO (CInt)

foreign import ccall "pcre2.h &pcre2_code_free_8"
  c_pcre2_code_free :: FunPtr (Ptr Code -> IO ())


data MatchData
data MatchContext
data Code
data GeneralContext
data JitStack

type PCRE2ErrorCode = Int
type PCRE2ErrorOffset = Int

data RegexCompilationError = CompilationError PCRE2ErrorCode PCRE2ErrorOffset | JITCompilationError PCRE2ErrorCode deriving Show

-- Convert a string to a vector of numbers of word8
utf8Vector :: String -> V.Vector Word8
utf8Vector = V.fromList . map (fromIntegral . ord)

-- Get csize of vector
cVectorSize :: V.Vector Word8 -> CSize
cVectorSize = fromIntegral . V.length

-- Compile a regex from a vector of 8 bit chars
compileRegexFromVector :: V.Vector Word8 -> IO (Either RegexCompilationError (ForeignPtr Code))
compileRegexFromVector regex = alloca $ \errorCode -> alloca $ \errorOffset -> do
                       compiledRegex <- V.unsafeWith regex $ \regexPointer -> c_pcre2_compile (castPtr regexPointer) regexSize 0 errorCode errorOffset nullPtr
                       if compiledRegex == nullPtr then do
                          errorCodePeek <- peek errorCode
                          errorCodeOffset <- peek errorOffset
                          return $ Left $ CompilationError (fromIntegral errorCodePeek) (fromIntegral errorCodeOffset)
                       else do
                          rc <- c_pcre2_jit_compile compiledRegex c_PCRE2_JIT_COMPLETE
                          if rc /= 0 then
                             return $ Left $ JITCompilationError $ fromIntegral rc
                          else do
                             foreignRegex <- newForeignPtr c_pcre2_code_free compiledRegex
                             return $ Right foreignRegex
                       where
                       regexSize = cVectorSize regex

-- Compile a regex from a String
compileRegex :: String -> IO (Either RegexCompilationError (ForeignPtr Code))
compileRegex = compileRegexFromVector . utf8Vector

