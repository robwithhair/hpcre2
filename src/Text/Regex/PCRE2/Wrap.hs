{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Text.Regex.PCRE2.Wrap(compileRegex
                           , compileRegexFromByte8String
                           , match
                           , matchFromByte8String
                           , jitMatch
                           , jitMatchFromByte8String
                           , serializeRegexs
                           , serializeJitRegexs
                           , serializeRegexsInContext
                           , deserializeRegexs
                           , deserializeRegexsInContext
                           , deserializeJITRegexs
                           , jitCompileRegex
                           , jitCompile
                           , CompiledRegex
                           , JITCompiledRegex
                           , PCRE2Error(..)
                           , Match(..)
                           , MatchPosition
                           , matchPositionStart
                           , matchPositionEnd
) where


import Foreign.C.Types
import Foreign.C.String(CString
                      , peekCAString
                      , CStringLen)
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(peekArray)
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Word
import Data.Char
import Data.Bits((.|.))
import Text.Regex.PCRE2.Wrap.Helper
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import GHC.Generics
import Control.DeepSeq


foreign import capi "pcre2.h value PCRE2_JIT_COMPLETE"
  c_PCRE2_JIT_COMPLETE :: CUInt

foreign import capi "pcre2.h value PCRE2_NOTEMPTY_ATSTART"
  c_PCRE2_NOTEMPTY_ATSTART :: CUInt

foreign import capi "pcre2.h value PCRE2_ANCHORED"
  c_PCRE2_ANCHORED :: CUInt

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

foreign import ccall "pcre2.h pcre2_code_free_8"
  c_pcre2_code_free :: Ptr Code -> IO ()

-- This one is marked unsafe for extra performance.
-- See https://wiki.haskell.org/Performance/FFI
foreign import ccall unsafe "pcre2.h pcre2_jit_match_8"
  c_pcre2_jit_match :: Ptr JITCode -> CString -> CSize -> CSize -> CUInt
    -> Ptr MatchData -> Ptr MatchContext -> IO CInt

foreign import ccall unsafe "pcre2.h pcre2_match_8"
  c_pcre2_match :: Ptr Code -> CString -> CSize -> CSize -> CUInt -> Ptr MatchData -> Ptr MatchContext -> IO CInt

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

foreign import ccall "pcre2.h pcre2_match_data_free_8"
  c_pcre2_match_data_free :: Ptr MatchData -> IO ()

foreign import ccall "pcre2.h pcre2_pattern_info_8"
  c_pcre2_pattern_info :: Ptr Code -> CUInt -> Ptr CSize -> IO (CInt)

foreign import ccall "pcre2.h pcre2_match_context_create_8"
  c_pcre2_match_context_create :: Ptr GeneralContext -> IO (Ptr MatchContext)

foreign import ccall "pcre2.h pcre2_match_context_free_8"
  c_pcre2_match_context_free :: Ptr MatchContext -> IO ()

foreign import ccall "pcre2.h pcre2_get_ovector_pointer_8"
  c_pcre2_get_ovector_pointer :: Ptr MatchData -> IO (Ptr CSize)

foreign import ccall unsafe "pcre2.h pcre2_serialize_encode_8"
  c_pcre2_serialize_encode :: Ptr (Ptr Code) -> CInt -> Ptr (Ptr CUChar) -> Ptr CSize -> Ptr GeneralContext -> IO CInt

foreign import ccall unsafe "pcre2.h pcre2_serialize_decode_8"
  c_pcre2_serialize_decode :: Ptr (Ptr Code) -> CInt -> Ptr CUChar -> Ptr GeneralContext -> IO CInt

foreign import ccall unsafe "pcre2.h pcre2_serialize_free_8"
  c_pcre2_serialize_free :: Ptr CUChar -> IO ()

data MatchData
data MatchContext
data Code
data JITCode
data GeneralContext
data JitStack

type PCRE2ErrorCode = Int
type PCRE2ErrorOffset = Int
type GroupCount = Int
type MatchPosition = (Integer, Integer)

type CompiledRegex = ForeignPtr Code
type JITCompiledRegex = ForeignPtr JITCode

data PCRE2Error = CompilationError PCRE2ErrorCode PCRE2ErrorOffset
     | JITCompilationError PCRE2ErrorCode
     | SerializationError PCRE2ErrorCode
     | DeserializationError PCRE2ErrorCode
     | MatchDataCreateError
     | JITMatchError PCRE2ErrorCode
     | NoMatch
     | PartialMatch
     | KUsedToSetMatchStartAfterEnd
     | JITMatchVectorOffsetsTooSmall deriving (Eq, Show, Generic, NFData)

data Match = Match GroupCount [MatchPosition] deriving (Eq, Show)
data MatchResult = MatchResult GroupCount (ForeignPtr MatchData) deriving (Eq, Show)

matchPositionStart :: MatchPosition -> Integer
matchPositionStart (s, _) = s

matchPositionEnd :: MatchPosition -> Integer
matchPositionEnd (_, e) = e

-- Get csize of vector
cVectorSize :: V.Vector Word8 -> CSize
cVectorSize = fromIntegral . V.length

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [CUInt] -> CUInt
combineOptions = foldr (.|.) 0

-- | Further JIT compile an already compiled regular expression code
jitCompile :: CompiledRegex -> IO (Either PCRE2Error JITCompiledRegex)
{-# NOINLINE jitCompile #-}
jitCompile foreignCompiledRegex = withForeignPtr foreignCompiledRegex $ \compiledRegex  -> do
           rc <- c_pcre2_jit_compile compiledRegex c_PCRE2_JIT_COMPLETE
           if rc /= 0 then
              return $ Left $ JITCompilationError $ fromIntegral rc
           else do
              return $ Right $ castForeignPtr foreignCompiledRegex

compileRegexFromCTypes :: CStringLen -> Ptr CInt -> Ptr CSize -> IO (Either PCRE2Error CompiledRegex)
{-# NOINLINE compileRegexFromCTypes #-}
compileRegexFromCTypes (regexPointer, regexLength) errorCode errorOffset = do
                       compiledRegex <- c_pcre2_compile regexPointer (fromIntegral regexLength) c_PCRE2_UTF errorCode errorOffset nullPtr
                       if compiledRegex == nullPtr then do
                          errorCodePeek <- peek errorCode
                          errorCodeOffset <- peek errorOffset
                          return $ Left $ CompilationError (fromIntegral errorCodePeek) (fromIntegral errorCodeOffset)
                       else do
                            foreignRegex <- newForeignPtr_ compiledRegex
                            return $ Right foreignRegex

-- Compile a regex from a vector of 8 bit chars
compileRegexFromByte8String :: B.ByteString -> IO (Either PCRE2Error CompiledRegex)
{-# NOINLINE compileRegexFromByte8String #-}
compileRegexFromByte8String regex = alloca $ \errorCode -> alloca $ \errorOffset ->
                       B.useAsCStringLen regex $ \regexPointer -> compileRegexFromCTypes regexPointer errorCode errorOffset

-- JIT Compile a regex from a string
jitCompileRegex :: T.Text -> IO (Either PCRE2Error JITCompiledRegex)
{-# NOINLINE jitCompileRegex #-}
jitCompileRegex text = do
                compiledRegex <- compileRegex text
                case compiledRegex of
                     (Left err) -> return $ Left err
                     (Right re) -> jitCompile re


-- Compile a regex from a String
compileRegex :: T.Text -> IO (Either PCRE2Error CompiledRegex)
{-# NOINLINE compileRegex #-}
compileRegex = compileRegexFromByte8String . E.encodeUtf8

matchDataPointerCreate :: Ptr Code -> IO (Maybe (ForeignPtr MatchData))
{-# NOINLINE matchDataPointerCreate #-}
matchDataPointerCreate regex = do
                matchData <- c_pcre2_match_data_create_from_pattern regex nullPtr
                if matchData == nullPtr then return Nothing else do
                   foreignData <- newForeignPtr_  matchData
                   return $ Just foreignData

-- Create a pointer to Match Data to ensure we have somewhere to save the matches
matchDataCreate :: CompiledRegex -> IO (Maybe (ForeignPtr MatchData))
{-# NOINLINE matchDataCreate #-}
matchDataCreate regex = withForeignPtr regex $ \pointerToRegex -> matchDataPointerCreate pointerToRegex

matchResultCode :: CInt -> Either PCRE2Error GroupCount
{-# NOINLINE matchResultCode #-}
matchResultCode res | res == -1 = Left NoMatch
                    | res > 0 = Right $ fromIntegral res - 1
                    | res == -2 = Left PartialMatch
                    | res == 0 = Left JITMatchVectorOffsetsTooSmall
                    | otherwise = Left $ JITMatchError $ fromIntegral res

performJitMatchAtOffset :: Ptr JITCode -> CStringLen -> Ptr MatchData -> Ptr MatchContext -> CUInt -> CSize  -> IO (Either PCRE2Error [Match])
{-# NOINLINE performJitMatchAtOffset #-}
performJitMatchAtOffset regex (text, textLength) matchDataPtr matchContext options offset = do
                     res <- c_pcre2_jit_match regex text (fromIntegral textLength) offset options matchDataPtr matchContext

                     let code = matchResultCode res in
                         case code of
                              (Right matchCount) -> do
                                     ovectorPointer <- c_pcre2_get_ovector_pointer matchDataPtr
                                     let convertToIntegers = fmap (map toInteger) . sequence
                                     xMatchGroups <- convertToIntegers [ peekElemOff ovectorPointer x | i <- [0..matchCount], let x = i * 2 ]
                                     yMatchGroups <- convertToIntegers [ peekElemOff ovectorPointer (x + 1) | i <- [0..matchCount], let x = i * 2]
                                     let matchGroups =  filter (uncurry (<=)) $ zip xMatchGroups yMatchGroups
                                     let endOfRecursion = return $ Right [Match matchCount matchGroups]
                                     let nextOptions = case matchGroups of
                                                            [] -> options
                                                            (start, end) : _ | start == end -> combineOptions [c_PCRE2_NOTEMPTY_ATSTART, c_PCRE2_ANCHORED, options]
                                                                             | otherwise -> options
                                     case matchGroups of
                                          [] -> endOfRecursion
                                          (_, end) : _ | end == toInteger textLength -> endOfRecursion
                                                       | otherwise -> do
                                                                   nextRes <- performJitMatchAtOffset regex (text, textLength) matchDataPtr matchContext nextOptions $ fromInteger end
                                                                   case nextRes of
                                                                        (Right m) -> return $ Right (Match matchCount matchGroups : m)
                                                                        (Left NoMatch) -> return $ Right [Match matchCount matchGroups]
                                                                        (Left err) -> return $ Left err
                              (Left err) -> return $ Left err

performJitMatch :: Ptr JITCode -> CStringLen -> Maybe (ForeignPtr MatchData) -> IO (Either PCRE2Error [Match])
{-# NOINLINE performJitMatch #-}
performJitMatch _ _ Nothing = return $ Left MatchDataCreateError
performJitMatch regex text (Just matchData) = withForeignPtr matchData $ \matchDataPtr -> do
             res <- performJitMatchAtOffset regex text matchDataPtr nullPtr 0 0
             finalizeForeignPtr matchData
             case res of
                  (Right matches) ->
                        return $ Right matches
                  (Left err) ->
                        return $ Left err

jitMatchFromCString :: JITCompiledRegex -> CStringLen -> IO (Either PCRE2Error [Match])
{-# NOINLINE jitMatchFromCString #-}
jitMatchFromCString regex text = withForeignPtr regex $ \regexPointer -> do
                 matchData <- matchDataCreate $ castForeignPtr regex
                 performJitMatch regexPointer text matchData

jitMatchFromByte8String :: JITCompiledRegex -> B.ByteString -> IO (Either PCRE2Error [Match])
{-# NOINLINE jitMatchFromByte8String #-}
jitMatchFromByte8String regex text = B.useAsCStringLen text $ \cString -> jitMatchFromCString regex cString

jitMatch :: JITCompiledRegex -> T.Text -> IO (Either PCRE2Error [Match])
{-# NOINLINE jitMatch #-}
jitMatch regex = jitMatchFromByte8String regex . E.encodeUtf8

serializeRegexsInContext :: ForeignPtr GeneralContext -> [CompiledRegex] -> IO (Either PCRE2Error B.ByteString)
{-# NOINLINE serializeRegexsInContext #-}
serializeRegexsInContext context regexs = do
                out <- withForeignPtr context $ \contextPtr -> V.unsafeWith vectorToForeignRegexs $ \regexsPtr -> alloca $ \serializedPtr -> alloca $ \serializedSizePtr -> do
                    res <- c_pcre2_serialize_encode regexsPtr regexesLength serializedPtr serializedSizePtr contextPtr
                    serializedDataPtr <- peek serializedPtr
                    serializedData <- newForeignPtr_ serializedDataPtr
                    -- check if the serialization was successful
                    if res < 0 then return $ Left $ SerializationError $ fromIntegral res else do
                       serializedSize <- peek serializedSizePtr
                       return $ Right $ BI.fromForeignPtr (castForeignPtr serializedData) 0 $ fromIntegral serializedSize
                mapM_ touchForeignPtr regexs
                return out
                where
                vectorToForeignRegexs = V.fromList $ map unsafeForeignPtrToPtr regexs
                regexesLength = fromIntegral $ V.length vectorToForeignRegexs

serializeRegexs :: [CompiledRegex] -> IO (Either PCRE2Error B.ByteString)
{-# NOINLINE serializeRegexs #-}
serializeRegexs regexs = newForeignPtr_ nullPtr >>= \null -> serializeRegexsInContext null regexs

serializeJitRegexs :: [JITCompiledRegex] -> IO (Either PCRE2Error B.ByteString)
{-# NOINLINE serializeJitRegexs #-}
serializeJitRegexs regexs = serializeRegexs $ map (castForeignPtr) regexs

deserializeRegexsInContext :: ForeignPtr GeneralContext                 -- ^ An optional pointer to a general PCRE2 context or a Null Pointer wrapped as a Foreign Pointer
                           -> B.ByteString                              -- ^ The data to convert to multiple regular expressions
                           -> Int                                       -- ^ The number of regular expressions contained in the data
                           -> IO (Either PCRE2Error [CompiledRegex])  -- ^ Returns an array of Foreign Pointers to Regular Expressions or an error
{-# NOINLINE deserializeRegexsInContext #-}
deserializeRegexsInContext context bytes numberOfRegexs = withForeignPtr context $ \contextPtr -> alloca $ \regexsPtr -> withForeignPtr (castForeignPtr (first3 (BI.toForeignPtr bytes))) $ \bytesPtr -> do
                           res <- c_pcre2_serialize_decode regexsPtr (fromIntegral numberOfRegexs) bytesPtr contextPtr
                           if res < 0 then return $ Left $ DeserializationError $ fromIntegral res else do
                              regexs <- peekArray numberOfRegexs regexsPtr
                              out <- sequence $ map (newForeignPtr_ ) regexs
                              return $ Right out

-- A helper function to pass a Null Pointer as the Context
deserializeRegexs :: B.ByteString -> Int -> IO (Either PCRE2Error [CompiledRegex])
{-# NOINLINE deserializeRegexs #-}
deserializeRegexs bytes numberOfRegexs = newForeignPtr_ nullPtr >>= \n -> deserializeRegexsInContext n bytes numberOfRegexs

-- A helper function to deserialise and JIT compile a regex
deserializeJITRegexs :: B.ByteString -> Int -> IO (Either PCRE2Error [JITCompiledRegex])
{-# NOINLINE deserializeJITRegexs #-}
deserializeJITRegexs bytes numberOfRegexs = do
                     regexs <- deserializeRegexs bytes numberOfRegexs
                     case regexs of
                          (Left e) -> return $ Left e
                          (Right rs) -> do
                                 compiled <- mapM jitCompile rs
                                 return $ handleArrayOfEithers compiled




match :: CompiledRegex -> T.Text -> IO (Either PCRE2Error [Match])
{-# NOINLINE match #-}
match regex = matchFromByte8String regex . E.encodeUtf8

matchFromByte8String :: CompiledRegex -> B.ByteString -> IO (Either PCRE2Error [Match])
{-# NOINLINE matchFromByte8String #-}
matchFromByte8String regex text = B.useAsCStringLen text $ \cString -> matchFromCString regex cString

matchFromCString :: CompiledRegex -> CStringLen -> IO (Either PCRE2Error [Match])
{-# NOINLINE matchFromCString #-}
matchFromCString regex text = withForeignPtr regex $ \regexPointer -> do
                 matchData <- matchDataCreate $ castForeignPtr regex
                 performMatch regexPointer text matchData

performMatch :: Ptr Code -> CStringLen -> Maybe (ForeignPtr MatchData) -> IO (Either PCRE2Error [Match])
{-# NOINLINE performMatch #-}
performMatch _ _ Nothing = return $ Left MatchDataCreateError
performMatch regex text (Just matchData) = withForeignPtr matchData $ \matchDataPtr -> do
             context <- c_pcre2_match_context_create nullPtr
             res <- performMatchAtOffset regex text matchDataPtr context 0 0
             -- finalizeForeignPtr matchData
             c_pcre2_match_context_free context
             case res of
                  (Right matches) ->
                        return $ Right matches
                  (Left err) ->
                        return $ Left err

performMatchAtOffset :: Ptr Code -> CStringLen -> Ptr MatchData -> Ptr MatchContext -> CUInt -> CSize  -> IO (Either PCRE2Error [Match])
{-# NOINLINE performMatchAtOffset #-}
performMatchAtOffset regex (text, textLength) matchDataPtr matchContext options offset = do
                     res <- c_pcre2_match regex text (fromIntegral textLength) offset options matchDataPtr matchContext

                     let code = matchResultCode res in
                         case code of
                              (Right matchCount) -> do
                                     ovectorPointer <- c_pcre2_get_ovector_pointer matchDataPtr
                                     let convertToIntegers = fmap (map toInteger) . sequence
                                     xMatchGroups <- convertToIntegers [ peekElemOff ovectorPointer x | i <- [0..matchCount], let x = i * 2 ]
                                     yMatchGroups <- convertToIntegers [ peekElemOff ovectorPointer (x + 1) | i <- [0..matchCount], let x = i * 2]
                                     let matchGroups =  filter (uncurry (<=)) $ zip xMatchGroups yMatchGroups
                                     let endOfRecursion = return $ Right [Match matchCount matchGroups]
                                     let nextOptions = case matchGroups of
                                                            [] -> options
                                                            (start, end) : _ | start == end -> combineOptions [c_PCRE2_NOTEMPTY_ATSTART, c_PCRE2_ANCHORED, options]
                                                                             | otherwise -> options
                                     case matchGroups of
                                          [] -> endOfRecursion
                                          (_, end) : _ | end == toInteger textLength -> endOfRecursion
                                                       | otherwise -> do
                                                                   nextRes <- performMatchAtOffset regex (text, textLength) matchDataPtr matchContext nextOptions $ fromInteger end
                                                                   case nextRes of
                                                                        (Right m) -> return $ Right (Match matchCount matchGroups : m)
                                                                        (Left NoMatch) -> return $ Right [Match matchCount matchGroups]
                                                                        (Left err) -> return $ Left err
                              (Left err) -> return $ Left err
