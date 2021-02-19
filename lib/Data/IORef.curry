------------------------------------------------------------------------------
--- Library with some useful extensions to the IO monad.
---
--- @author Michael Hanus
--- @version January 2017
--- @category general
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Data.IORef
  (
    IORef, newIORef, readIORef, writeIORef, modifyIORef
  ) where

--- Mutable variables containing values of some type.
--- The values are not evaluated when they are assigned to an IORef.
#ifdef __PAKCS__
data IORef a = IORef a -- precise structure internally defined
#else
external data IORef _ -- precise structure internally defined
#endif
--- Creates a new IORef with an initial value.
newIORef :: a -> IO (IORef a)
newIORef external

--- Reads the current value of an IORef.
readIORef :: IORef a -> IO a
readIORef ref = prim_readIORef $# ref

prim_readIORef :: IORef a -> IO a
prim_readIORef external

--- Updates the value of an IORef.
writeIORef :: IORef a -> a -> IO ()
writeIORef ref val = (prim_writeIORef $# ref) val

prim_writeIORef :: IORef a -> a -> IO ()
prim_writeIORef external

--- Modify the value of an IORef.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f
