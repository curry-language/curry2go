{-# LANGUAGE MultiParamTypeClasses #-}
import           Control.Concurrent
import qualified Control.Exception  as C (IOException, catch, throw)
import           Control.Monad           (zipWithM)
import           System.IO
import           System.IO.Error         (isEOFError)

type C_Handle = PrimData CurryHandle

instance ConvertCurryHaskell C_IOMode IOMode where
  toCurry ReadMode   = C_ReadMode
  toCurry WriteMode  = C_WriteMode
  toCurry AppendMode = C_AppendMode

  fromCurry C_ReadMode   = ReadMode
  fromCurry C_WriteMode  = WriteMode
  fromCurry C_AppendMode = AppendMode
  fromCurry _            = error "IOMode data with no ground term occurred"

instance ConvertCurryHaskell C_SeekMode SeekMode where
  toCurry AbsoluteSeek = C_AbsoluteSeek
  toCurry RelativeSeek = C_RelativeSeek
  toCurry SeekFromEnd  = C_SeekFromEnd

  fromCurry C_AbsoluteSeek = AbsoluteSeek
  fromCurry C_RelativeSeek = RelativeSeek
  fromCurry C_SeekFromEnd  = SeekFromEnd
  fromCurry _            = error "SeekMode data with no ground term occurred"


external_d_C_handle_eq :: C_Handle -> C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_Bool
external_d_C_handle_eq (PrimData h1) (PrimData h2) _ _ = toCurry (h1 == h2)

external_d_C_stdin :: Cover -> ConstStore -> C_Handle
external_d_C_stdin _ _ = PrimData (OneHandle stdin)

external_d_C_stdout :: Cover -> ConstStore -> C_Handle
external_d_C_stdout _ _ = PrimData (OneHandle stdout)

external_d_C_stderr :: Cover -> ConstStore -> C_Handle
external_d_C_stderr _ _ = PrimData (OneHandle stderr)

external_d_C_prim_openFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_IOMode
                           -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Handle
external_d_C_prim_openFile fn  mode _ _ =
  toCurry (\s m -> openFile s m >>= return . OneHandle) fn mode

external_d_C_prim_hClose :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_hClose handle _ _ = toCurry
  (\ch -> case ch of OneHandle h       -> hClose h
                     InOutHandle h1 h2 -> hClose h1 >> hClose h2) handle

external_d_C_prim_hFlush :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_hFlush h _ _ = toCurry (hFlush . outputHandle) h

external_d_C_prim_hIsEOF :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_hIsEOF h _ _ = toCurry (hIsEOF . inputHandle) h

external_d_C_prim_hSeek :: C_Handle -> C_SeekMode -> Curry_Prelude.C_Int
                        -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_hSeek handle mode i _ _
  = toCurry (hSeek . inputHandle) handle mode i

external_d_C_prim_hWaitForInput :: C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore
                                -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_hWaitForInput handle timeout _ _
  = toCurry (myhWaitForInput . inputHandle) handle timeout

myhWaitForInput :: Handle -> Int -> IO Bool
myhWaitForInput h timeout = C.catch (hWaitForInput h timeout) handler
  where
  handler :: C.IOException -> IO Bool
  handler e = if isEOFError e then return False else C.throw e

external_d_C_prim_hWaitForInputs :: Curry_Prelude.OP_List C_Handle -> Curry_Prelude.C_Int
                                 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
external_d_C_prim_hWaitForInputs hs i _ _ = toCurry selectHandle hs i

selectHandle :: [CurryHandle] -> Int -> IO Int
selectHandle handles timeout = do
  mvar <- newEmptyMVar
  threads <- zipWithM
              (\ i h -> forkIO (waitOnHandle (inputHandle h) i timeout mvar))
              [0 ..] handles
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] -> IO Int
inspectRes 0 _    _       = return (-1)
inspectRes n mvar threads = do
  res <- takeMVar mvar
  case res of
    Nothing -> inspectRes (n - 1) mvar threads
    Just v  -> mapM_ killThread threads >> return v

waitOnHandle :: Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v timeout mvar = do
  ready <- myhWaitForInput h timeout
  putMVar mvar (if ready then Just v else Nothing)

external_d_C_prim_hGetChar :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Char
external_d_C_prim_hGetChar h _ _ = toCurry (hGetChar . inputHandle) h

external_d_C_prim_hPutChar :: C_Handle -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_hPutChar h c _ _ = toCurry (hPutChar . outputHandle) h c

external_d_C_prim_hIsReadable :: C_Handle -> Cover -> ConstStore
                              -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_hIsReadable h _ _ = toCurry (hIsReadable . inputHandle) h

external_d_C_prim_hIsWritable :: C_Handle -> Cover -> ConstStore
                              -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_hIsWritable h _ _ = toCurry (hIsWritable . outputHandle) h

external_d_C_prim_hIsTerminalDevice :: C_Handle -> Cover -> ConstStore
                              -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_hIsTerminalDevice h _ _ =
    toCurry (hIsTerminalDevice . outputHandle) h
