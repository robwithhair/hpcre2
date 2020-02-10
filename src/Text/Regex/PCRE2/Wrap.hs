{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BangPatterns #-}

module Text.Regex.PCRE2.Wrap(compileRegex
                           , compileRegexFromByte8String
                           , match
                           , matchFromByte8String
) where


import Foreign.C.Types
import Foreign.C.String(CString
                      , peekCAString
                      , CStringLen)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Word
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

foreign import capi "pcre2.h value PCRE2_JIT_COMPLETE"
  c_PCRE2_JIT_COMPLETE :: CUInt

foreign import capi "pcre2.h value PCRE2_ZERO_TERMINATED"
  c_PCRE2_ZERO_TERMINATED :: CSize

foreign import capi "pcre2.h value PCRE2_INFO_JITSIZE"
  c_PCRE2_INFO_JITSIZE :: CUInt

foreign import capi "pcre2.h value PCRE2_UTF"
  c_PCRE2_UTF :: CUInt

foreign import ccall "pcre2.h pcre2_compile_8"
  c_pcre2_compile :: CString -> CSize -> CUInt -> Ptr CInt -> Ptr CSize -> Ptr () -> IO (Ptr Code)

foreign import ccall "pcre2.h pcre2_jit_compile_8"
  c_pcre2_jit_compile :: Ptr Code -> CUInt -> IO (CInt)

foreign import ccall "pcre2.h &pcre2_code_free_8"
  c_pcre2_code_free :: FunPtr (Ptr Code -> IO ())

-- This one is marked unsafe for extra performance.
-- See https://wiki.haskell.org/Performance/FFI
foreign import ccall unsafe "pcre2.h pcre2_jit_match_8"
  c_pcre2_jit_match :: Ptr Code -> CString -> CSize -> CSize -> CUInt
    -> Ptr MatchData -> Ptr MatchContext -> IO CInt

foreign import ccall "pcre2.h pcre2_jit_stack_create_8"
  c_pcre2_jit_stack_create :: CSize -> CSize -> Ptr () -> IO (Ptr JitStack)

foreign import ccall "pcre2.h pcre2_jit_stack_assign_8"
  c_pcre2_jit_stack_assign :: Ptr MatchContext -> Ptr () -> Ptr JitStack
    -> IO ()

foreign import ccall "pcre2.h pcre2_match_data_create_8"
  c_pcre2_match_data_create :: CUInt -> Ptr GeneralContext
    -> IO (Ptr MatchData)

foreign import ccall "pcre2.h pcre2_match_data_create_from_pattern_8"
  c_pcre2_match_data_create_from_pattern :: Ptr Code -> Ptr GeneralContext
    -> IO (Ptr MatchData)

foreign import ccall "pcre2.h pcre2_jit_stack_free_8"
  c_pcre2_jit_stack_free :: Ptr JitStack -> IO ()

foreign import ccall "pcre2.h &pcre2_match_data_free_8"
  c_pcre2_match_data_free :: FunPtr (Ptr MatchData -> IO ())

foreign import ccall "pcre2.h pcre2_pattern_info_8"
  c_pcre2_pattern_info :: Ptr Code -> CUInt -> Ptr CSize -> IO (CInt)

foreign import ccall "pcre2.h pcre2_match_context_create_8"
  c_pcre2_match_context_create :: Ptr GeneralContext -> IO (Ptr MatchContext)

data MatchData
data MatchContext
data Code
data GeneralContext
data JitStack

type PCRE2ErrorCode = Int
type PCRE2ErrorOffset = Int
type MatchCount = Int

data PCRE2Error = CompilationError PCRE2ErrorCode PCRE2ErrorOffset
     | JITCompilationError PCRE2ErrorCode
     | MatchDataCreateError
     | JITMatchError PCRE2ErrorCode
     | NoMatch
     | PartialMatch
     | JITMatchVectorOffsetsTooSmall deriving Show

data MatchResult = MatchResult MatchCount (ForeignPtr MatchData) deriving Show

-- Get csize of vector
cVectorSize :: V.Vector Word8 -> CSize
cVectorSize = fromIntegral . V.length

compileRegexFromCTypes :: CStringLen -> Ptr CInt -> Ptr CSize -> IO (Either PCRE2Error (ForeignPtr Code))
compileRegexFromCTypes (regexPointer, regexLength) errorCode errorOffset = do
                       compiledRegex <- c_pcre2_compile regexPointer (fromIntegral regexLength) c_PCRE2_UTF errorCode errorOffset nullPtr
                       if compiledRegex == nullPtr then do
                          errorCodePeek <- peek errorCode
                          errorCodeOffset <- peek errorOffset
                          return $ Left $ CompilationError (fromIntegral errorCodePeek) (fromIntegral errorCodeOffset)
                       else do
                          rc <- c_pcre2_jit_compile compiledRegex c_PCRE2_JIT_COMPLETE
                          if rc /= 0 then
                             return $ Left $ JITCompilationError $ fromIntegral rc
                          else do
                             foreignRegex <- newForeignPtr (c_pcre2_code_free) compiledRegex 
                             return $ Right foreignRegex

-- Compile a regex from a vector of 8 bit chars
compileRegexFromByte8String :: B.ByteString -> IO (Either PCRE2Error (ForeignPtr Code))
compileRegexFromByte8String regex = alloca $ \errorCode -> alloca $ \errorOffset ->
                       B.useAsCStringLen regex $ \regexPointer -> compileRegexFromCTypes regexPointer errorCode errorOffset

-- Compile a regex from a String
compileRegex :: T.Text -> IO (Either PCRE2Error (ForeignPtr Code))
compileRegex = compileRegexFromByte8String . E.encodeUtf8

matchDataPointerCreate :: Ptr Code -> IO (Maybe (ForeignPtr MatchData))
matchDataPointerCreate regex = do
                matchData <- c_pcre2_match_data_create_from_pattern regex nullPtr
                if matchData == nullPtr then return Nothing else do
                   foreignData <- newForeignPtr (c_pcre2_match_data_free) matchData
                   return $ Just foreignData

-- Create a pointer to Match Data to ensure we have somewhere to save the matches
matchDataCreate :: ForeignPtr Code -> IO (Maybe (ForeignPtr MatchData))
matchDataCreate regex = withForeignPtr regex $ \pointerToRegex -> matchDataPointerCreate pointerToRegex

matchResultCode :: CInt -> Either PCRE2Error MatchCount
matchResultCode res | res == -1 = Left NoMatch
                    | res > 0 = Right $ (fromIntegral res) - 1
                    | res == -2 = Left PartialMatch
                    | res == 0 = Left JITMatchVectorOffsetsTooSmall
                    | otherwise = Left $ JITMatchError $ fromIntegral res

performMatch :: Ptr Code -> CStringLen -> Maybe (ForeignPtr MatchData) -> IO (Either PCRE2Error MatchResult)
performMatch _ _ Nothing = return $ Left MatchDataCreateError
performMatch regex (text, textLength) (Just matchData) = withForeignPtr matchData $ \matchDataPtr -> do
             res <- c_pcre2_jit_match regex text (fromIntegral textLength) 0 0 matchDataPtr nullPtr
             let code = matchResultCode res in
                 case code of
                      (Right matchCount) -> return $ Right $ MatchResult matchCount matchData
                      (Left error) -> do
                            finalizeForeignPtr matchData
                            return $ Left error

matchFromCString :: ForeignPtr Code -> CStringLen -> IO (Either PCRE2Error MatchResult)
matchFromCString regex text = withForeignPtr regex $ \regexPointer -> do
                 matchData <- matchDataCreate regex
                 performMatch regexPointer text matchData

matchFromByte8String :: ForeignPtr Code -> B.ByteString -> IO (Either PCRE2Error MatchResult)
matchFromByte8String regex text = B.useAsCStringLen text $ \cString -> matchFromCString regex cString

match :: ForeignPtr Code -> T.Text -> IO (Either PCRE2Error MatchResult)
match regex = matchFromByte8String regex . E.encodeUtf8
