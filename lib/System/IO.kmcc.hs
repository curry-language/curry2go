{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module System.Curry_IO (CurryHandle (..), readHandle, writeHandle) where

import qualified Prelude as P
import qualified Control.Concurrent as C
import qualified Control.Monad as C (zipWithM)
import qualified System.IO as S
import BasicDefinitions

data CurryHandle = SingleHandle S.Handle        -- single handle for read/write
                 | DualHandle S.Handle S.Handle -- separate handles for read/write
  deriving (P.Eq)

readHandle :: CurryHandle -> S.Handle
readHandle (SingleHandle h) = h
readHandle (DualHandle h _) = h

writeHandle :: CurryHandle -> S.Handle
writeHandle (SingleHandle h) = h
writeHandle (DualHandle _ h) = h


-- Curryable instance for S.Handle
type instance HsEquivalent CurryHandle = CurryHandle

instance ToHs CurryHandle where
  to = P.return

instance FromHs CurryHandle where
  from = P.id
  elimFlat = P.id

instance ShowTerm CurryHandle where
  showTerm _ _ = P.showString "<<Handle>>"

instance ReadTerm CurryHandle where
  readTerm = P.error "reading a Handle is not possible"

instance ShowFree CurryHandle where
  showsFreePrec _ _ = showsStringCurry "<<Handle>>"

instance NormalForm CurryHandle where
  nfWith _ !x = P.return (P.Right x)

instance Narrowable CurryHandle where
  narrow = P.error "narrowing a Handle is not possible"
  narrowConstr = P.error "narrowing a Handle is not possible"

instance HasPrimitiveInfo CurryHandle where
  primitiveInfo = NoPrimitive

instance Unifiable CurryHandle where
  unifyWith _ _ _ = P.error "unifying a Handle is not possible"

  lazyUnifyVar _ _ = P.error "unifying a Handle is not possible"

instance Curryable CurryHandle


-- type declarations for handles
type Handle_Det# = CurryHandle
type Handle_ND# = CurryHandle

instance ForeignType CurryHandle where
  type Foreign CurryHandle = CurryHandle
  toForeign = P.id
  fromForeign = P.id

-- foreign instance for IOMode
instance ForeignType IOMode_Det where
  type Foreign IOMode_Det = S.IOMode
  toForeign ReadMode_Det = S.ReadMode
  toForeign WriteMode_Det = S.WriteMode
  toForeign AppendMode_Det = S.AppendMode

  fromForeign S.ReadMode = ReadMode_Det
  fromForeign S.WriteMode = WriteMode_Det
  fromForeign S.AppendMode = AppendMode_Det
  fromForeign _ = P.error "invalid IOMode conversion"

-- foreign instance for SeekMode
instance ForeignType SeekMode_Det where
  type Foreign SeekMode_Det = S.SeekMode
  toForeign AbsoluteSeek_Det = S.AbsoluteSeek
  toForeign RelativeSeek_Det = S.RelativeSeek
  toForeign SeekFromEnd_Det = S.SeekFromEnd

  fromForeign S.AbsoluteSeek = AbsoluteSeek_Det
  fromForeign S.RelativeSeek = RelativeSeek_Det
  fromForeign S.SeekFromEnd = SeekFromEnd_Det

-- function defintions
iOdothandleuscoreeq_Det# = liftForeign2 (P.==)
iOdothandleuscoreeq_ND# = liftConvert2 iOdothandleuscoreeq_Det#

iOdotstdin_Det# = SingleHandle S.stdin
iOdotstdin_ND# = P.return iOdotstdin_Det#

iOdotstdout_Det# = SingleHandle S.stdout
iOdotstdout_ND# = P.return iOdotstdout_Det#

iOdotstderr_Det# = SingleHandle S.stderr
iOdotstderr_ND# = P.return iOdotstderr_Det#

iOdotprimuscoreopenFile_Det# s m = S.openFile (toForeign s) (toForeign m) P.>>= P.return P.. SingleHandle
iOdotprimuscoreopenFile_ND# = liftConvertIO2 iOdotprimuscoreopenFile_Det#

iOdotprimuscorehClose_Det# (SingleHandle h)   = fromForeign P.$ S.hClose h
iOdotprimuscorehClose_Det# (DualHandle h1 h2) = fromForeign P.$ S.hClose h1 P.>> S.hClose h2
iOdotprimuscorehClose_ND# = liftConvertIO1 iOdotprimuscorehClose_Det#

iOdotprimuscorehFlush_Det# h = fromForeign P.$ S.hFlush (writeHandle h)
iOdotprimuscorehFlush_ND# = liftConvertIO1 iOdotprimuscorehFlush_Det#

iOdotprimuscorehIsEOF_Det# h = fromForeign P.$ S.hIsEOF (readHandle h)
iOdotprimuscorehIsEOF_ND# = liftConvertIO1 iOdotprimuscorehIsEOF_Det#

iOdotprimuscorehSeek_Det# x y z = fromForeign P.$ S.hSeek (readHandle x) (toForeign y) (toForeign z)
iOdotprimuscorehSeek_ND# = P.return P.$ from iOdotprimuscorehSeek_Det#

iOdotprimuscorehWaitForInput_Det# x y = fromForeign P.$ S.hWaitForInput (readHandle x) (P.fromInteger y)
iOdotprimuscorehWaitForInput_ND# = liftConvertIO2 iOdotprimuscorehWaitForInput_Det#

iOdotprimuscorehWaitForInputs_Det# handles timeout = fromForeign P.$ (selectHandle (P.map readHandle (toForeign handles)) (P.fromInteger timeout) P.>>= P.return P.. P.toInteger)
iOdotprimuscorehWaitForInputs_ND# = liftConvertIO2 iOdotprimuscorehWaitForInputs_Det#

-- run every handle in its own thread
selectHandle :: [S.Handle] -> P.Int -> P.IO P.Int
selectHandle handles timeout = do
  mvar <- C.newEmptyMVar
  threads <- C.zipWithM
              (\ i h -> C.forkIO (waitOnHandle h i timeout mvar))
              [0 ..] handles
  inspectRes (P.length handles) mvar threads

-- return the handle id if it receives input, otherwise return Nothing
waitOnHandle :: S.Handle -> P.Int -> P.Int -> C.MVar (P.Maybe P.Int) -> P.IO ()
waitOnHandle h v timeout mvar = do
  ready <- S.hWaitForInput h timeout
  C.putMVar mvar (if ready then P.Just v else P.Nothing)

-- check if an id has been returned, otherwise return -1
inspectRes :: P.Int -> C.MVar (P.Maybe P.Int) -> [C.ThreadId] -> P.IO P.Int
inspectRes 0 _    _       = P.return (-1)
inspectRes n mvar threads = do
  res <- C.takeMVar mvar
  case res of
    P.Nothing -> inspectRes (n P.- 1) mvar threads
    P.Just v  -> P.mapM_ C.killThread threads P.>> P.return v

iOdotprimuscorehGetChar_Det# h = fromForeign P.$ S.hGetChar (readHandle h)
iOdotprimuscorehGetChar_ND# = liftConvertIO1 iOdotprimuscorehGetChar_Det#

iOdotprimuscorehPutChar_Det# h c = fromForeign P.$ S.hPutChar (writeHandle h) (toForeign c)
iOdotprimuscorehPutChar_ND# = liftConvertIO2 iOdotprimuscorehPutChar_Det#

iOdotprimuscorehIsReadable_Det# h = fromForeign P.$ S.hIsReadable (readHandle h)
iOdotprimuscorehIsReadable_ND# = liftConvertIO1 iOdotprimuscorehIsReadable_Det#

iOdotprimuscorehIsWritable_Det# h = fromForeign P.$ S.hIsWritable (writeHandle h)
iOdotprimuscorehIsWritable_ND# = liftConvertIO1 iOdotprimuscorehIsWritable_Det#

iOdotprimuscorehIsTerminalDevice_Det# h = fromForeign P.$ S.hIsTerminalDevice (writeHandle h)
iOdotprimuscorehIsTerminalDevice_ND# = liftConvertIO1 iOdotprimuscorehIsTerminalDevice_Det#
