{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TrackMouseEvent (
    TMEFlags,
    tME_Cancel,
    tME_Hover,
    tME_Leave,
    tME_NonClient,
    tME_Query,
    hOVER_DEFAULT,
    TRACKMOUSEEVENT(..),
    trackMouseEvent,
    wM_MOUSELEAVE
) where

import Foreign
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Win32
import Graphics.Win32.GDI.Brush
import Graphics.Win32.GDI.Graphics2D
import Graphics.Win32.Message
import Graphics.Win32.Window

foreign import ccall "TrackMouseEvent" c_TrackMouseEvent :: Ptr TRACKMOUSEEVENT -> IO LONG

{-
typedef struct tagTRACKMOUSEEVENT {
  DWORD cbSize; -- 0
  DWORD dwFlags; -- 4
  HWND  hwndTrack; -- 8
  DWORD dwHoverTime; -- 16
} TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT;
-}

type TMEFlags = DWORD

tME_Cancel :: TMEFlags
tME_Cancel = 0x80000000

tME_Hover :: TMEFlags
tME_Hover = 0x00000001

tME_Leave :: TMEFlags
tME_Leave = 0x00000002

tME_NonClient :: TMEFlags
tME_NonClient = 0x00000010

tME_Query :: TMEFlags
tME_Query = 0x40000000

hOVER_DEFAULT :: DWORD
hOVER_DEFAULT = 0xffffffff

{-
struct tagTRACKMOUSEEVENT * 0x00000000`00000001
   +0x000 cbSize           : ??
   +0x004 dwFlags          : ??
   +0x008 hwndTrack        : ???? 
   +0x010 dwHoverTime      : ??
   -}

type TRACKMOUSEEVENT = (TMEFlags, HWND, DWORD)
withTRACKMOUSEEVENT :: TRACKMOUSEEVENT -> (Ptr TRACKMOUSEEVENT -> IO a) -> IO a
withTRACKMOUSEEVENT (tmeFlags, hwnd, time) f =
  allocaBytes 24 $ \ p -> do
  (\hsc_ptr -> pokeByteOff hsc_ptr 0) p (24::DWORD)
  (\hsc_ptr -> pokeByteOff hsc_ptr 4) p tmeFlags
  (\hsc_ptr -> pokeByteOff hsc_ptr 8) p hwnd
  (\hsc_ptr -> pokeByteOff hsc_ptr 16) p time
  f p

trackMouseEvent :: TRACKMOUSEEVENT -> IO Bool
trackMouseEvent tme = withTRACKMOUSEEVENT tme (c_TrackMouseEvent) >>= \x -> return $ x /= 0

wM_MOUSELEAVE :: WindowMessage
wM_MOUSELEAVE = fromIntegral 0x2a3