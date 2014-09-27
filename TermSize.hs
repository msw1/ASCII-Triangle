-- Taken from https://stackoverflow.com/questions/12806053/get-terminal-width-haskell/12807521#12807521
module TermSize (getTermSize) where

import Foreign
import Foreign.C.Error
import Foreign.C.Types

-- The ws_xpixel and ws_ypixel fields are unused, so I've omitted them here.
data WinSize = WinSize { wsRow, wsCol :: CUShort }

instance Storable WinSize where
  sizeOf _ = ((8))
  alignment _ = (2) 
  peek ptr = do
    row <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
    col <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
    return $ WinSize row col
    poke ptr (WinSize row col) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr row
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr col

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WinSize -> IO CInt

-- | Return current number of (rows, columns) of the terminal.
getTermSize :: IO (Int, Int)
getTermSize = 
  with (WinSize 0 0) $ \ws -> do
    throwErrnoIfMinus1 "ioctl" $
      ioctl (1) (21523) ws
    WinSize row col <- peek ws
    return (fromIntegral row, fromIntegral col)
