{-# OPTIONS_GHC -fno-warn-tabs #-}

module GridWin 
(AppState(..),
emptyState, 
wndProc',
wndProc,
startApp,
windowRect,
addColor) where

import Foreign
import Foreign.Ptr
import Graphics.Win32
import Data.IORef
import Graphics.Win32.Message
import Graphics.Win32.Window
import Graphics.Win32.Key
import System.Win32.DLL (getModuleHandle)
import TrackMouseEvent
import Utils (doMsgPump, createClassWindowDo, withPaintPaintStruct, takeRange, if', findAndExcludeFirst, unJust)





data AppState a = AppState {
initialSize :: (Int, Int),
blockDim :: (Int, Int, Int, Int), -- margin gap width height
bgColor :: COLORREF,
timerId :: Maybe UINT,
windowSize :: SIZE,
cursorPos :: Maybe (Int, Int),
blocks :: [(Int, Int, COLORREF)],
transformer :: AppState a -> (AppState a, [RECT]),
appdata :: a}

emptyState :: a -> AppState a
emptyState a = let
        c_sizeX::Int
        c_sizeX = 600
        c_sizeY::Int
        c_sizeY = 450
        bgColor :: COLORREF
        bgColor = fromIntegral 0x303030
        blockMargin::Int
        blockMargin = 2

        blockGap::Int
        blockGap = 2

        blockWidth::Int
        blockWidth = 10

        blockHeight::Int
        blockHeight = 10

    in  AppState 
    (c_sizeX, c_sizeY)
    (blockMargin, blockGap, blockWidth, blockHeight)
    bgColor
    Nothing -- timerId
    (fromIntegral c_sizeX, fromIntegral  c_sizeY)  -- windowSize
    Nothing
    []-- blocks
    (\x -> (x, []))
    a

zxyxyz c (a, b) = (a, b, c)

xyzxy (a, b, _) = (a, b)

addColor :: COLORREF -> (a, b) -> (a, b, COLORREF)
addColor = zxyxyz

windowRect :: AppState a -> RECT
windowRect as = let
        (wsx, wsy) = windowSize as
    in
        (fromIntegral 0, fromIntegral 0, fromIntegral wsx, fromIntegral wsy)

block :: Int -> Int -> Int -> Int -> Int -> Int -> RECT
block bm bg  x y w h = (fromIntegral xStart, fromIntegral yStart, fromIntegral $ xStart + w, fromIntegral $ yStart + h)
    where
        xStart = bm + (w + bg) * x
        yStart = bm + (h + bg) * y     

render :: (IORef (AppState a)) -> HWND -> (HDC, Bool, RECT) -> IO ()
render st hwnd (hdc, _, rect@(l, t, r, b)) = do
    as <- readIORef st
--    printRECT rect
--    print (blocks st)
--    print (nextBlocks' (10, 10) $ blocks st)
    mapM_ (drawObj (blockDim as)) (blocks as)
    drawIfJust (drawObj (blockDim as)) $ fmap (addColor (rgb 0x88 0x88 0x00)) (cursorPos as) 
    where
        drawIfJust ::  ((Int, Int, COLORREF) -> IO()) -> (Maybe (Int, Int, COLORREF)) -> IO ()
        drawIfJust f (Just v) = f v
        drawIfJust _ Nothing  = return ()

        drawObj :: (Int, Int, Int, Int) -> (Int, Int, COLORREF)  -> IO ()
        drawObj  (blockMargin, blockGap, blockWidth, blockHeight)  (x, y, clr) = do
            brush <- createSolidBrush clr
            fillRect hdc (block blockMargin blockGap x y blockWidth blockHeight) brush
            deleteBrush brush

updateRef :: IORef a -> (a -> IO a) -> IO ()
updateRef st f = readIORef st >>= f >>= writeIORef st

onTimerIdM :: IORef (AppState a) -> (Maybe UINT -> IO (Maybe UINT)) -> IO ()
onTimerIdM st f = updateRef st 
    (\x1 -> f (timerId x1) >>= \x2 -> return $ x1 {timerId = x2})

startTimer :: HWND -> IORef (AppState a) -> IO ()
startTimer hwnd st = onTimerIdM st (f)
    where
        f Nothing = setWinTimer hwnd 1 100 >>= return . Just
        f a = return a

stopTimer :: HWND -> IORef (AppState a) -> IO ()
stopTimer hwnd st = onTimerIdM st (f)
    where
        f (Just tid) = killTimer (Just hwnd) tid >> return Nothing
        f Nothing = return Nothing

invalRects :: HWND -> [RECT] -> IO ()
invalRects hwnd rcs = mapM_    f rcs
    where
        f rc = withRECT (rc) (\lpr -> invalidateRect (Just hwnd) (Just lpr) True {- bErase -})

onTime :: HWND -> IORef (AppState a) -> IO ()
onTime hwnd st = do
    --putStrLn "Time"
    as <- readIORef st
    (as', rects) <- return $ (transformer as) as
    writeIORef st as'
    invalRects hwnd rects

onSize :: HWND -> IORef (AppState a) -> LPARAM -> IO()
onSize hwnd st lp = do
    (sx, sy) <- return (lp .&. 0xffff, lp `shift` (-16))
    putStrLn $ unwords ["Size(", show sx, ",", show sy, ")" ]
    updateRef st (\as -> return $ as {windowSize = (fromIntegral sx, fromIntegral sy)})

maybeList :: Maybe a -> [a]
maybeList Nothing = []
maybeList (Just a) = [a]

onMouseMove :: HWND -> IORef (AppState a) -> LPARAM -> IO ()
onMouseMove hwnd st lp = do
    as <- readIORef st
    (sx, sy) <- return (lp .&. 0xffff, lp `shift` (-16))
    (blockMargin, blockGap, blockWidth, blockHeight) <- return $ blockDim as
    pos <- return ((fromIntegral sx) `div` (blockWidth + blockGap), (fromIntegral sy) `div` (blockHeight + blockGap))
    -- putStrLn $ unwords ["Cursor(", show sx, ",", show sy, ")" ]
    invalRects hwnd $ map (\(x, y) -> block blockMargin blockGap x y blockWidth blockHeight) (pos : (maybeList $ cursorPos as))
    trackMouseEvent (tME_Leave, hwnd, hOVER_DEFAULT)
    updateRef st (\x -> return $ x {cursorPos = (Just pos)})


onMouseLeave :: HWND -> IORef (AppState a) -> IO ()
onMouseLeave hwnd st = do
    putStrLn "MouseLeave"
    as <- readIORef st
    (blockMargin, blockGap, blockWidth, blockHeight) <- return $ blockDim as
    invalRects hwnd $ map (\(x, y) -> block blockMargin blockGap x y blockWidth blockHeight) (maybeList $ cursorPos as)
    updateRef st (\as -> return $ as {cursorPos = Nothing})



{-----------------------------------------------------
    W N D  P R O C
-----------------------------------------------------}
wndProc' :: (HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT) -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc' f hwnd wm wp lp = do
    -- putStrLn $ unwords ["msg =", show wm]
    f hwnd wm wp lp

wndProc :: (IORef (AppState a)) -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc st hwnd wm wp lp
    | wm == wM_LBUTTONDOWN = doFinish
    | wm == wM_DESTROY     = postQuitMessage 0 >> return 0
    | wm == wM_PAINT       = withPaintPaintStruct hwnd (render st) >> return 0
    | wm == wM_SETFOCUS    = startTimer hwnd st >> return 0
    | wm == wM_KILLFOCUS   = stopTimer hwnd st >> return 0
    | wm == wM_TIMER       = onTime hwnd st >> return 0
    | wm == wM_SIZE        = onSize hwnd st lp >> return 0
    | wm == wM_MOUSEMOVE   = onMouseMove hwnd st lp >> return 0
    | wm == wM_MOUSELEAVE  = onMouseLeave hwnd st >> return 0
    | otherwise            = defWindowProc (Just hwnd) wm wp lp
    where
        doFinish     = sendMessage hwnd wM_CLOSE 1 0 >> return 0

startApp :: AppState a -> IO ()
startApp as = let
        (c_sizeX, c_sizeY) = initialSize as
        myBgColor = bgColor as
    in do
    appState <- newIORef $ as

    hinst         <- getModuleHandle Nothing
    whiteBrush    <- createSolidBrush myBgColor
    curArrow    <- loadCursor Nothing iDC_ARROW
    createClassWindowDo
        (cS_DBLCLKS,
         hinst,        -- HINSTANCE
         Nothing,    -- Maybe HICON
         Just curArrow,    -- Maybe HCURSOR
         Just whiteBrush,-- Maybe HBRUSH
         Nothing,    -- Maybe LPCTSTR
         mkClassName "My Window Class")

        "Conway's Game of Life"
        (wS_CAPTION + wS_SYSMENU + wS_MAXIMIZE + wS_MINIMIZE + wS_MAXIMIZEBOX + wS_MINIMIZEBOX)
        Nothing         -- Maybe Pos :: x
        Nothing         -- Maybe Pos :: y
        (Just $ c_sizeX + 6)     -- Maybe Pos :: dx
        (Just $ c_sizeY + 29)    -- Maybe Pos :: dy
        Nothing         -- Maybe HWND
        Nothing         -- Maybe HMENU
        (wndProc' (wndProc appState))     -- WindowClosure

        (\hwnd -> do
            showWindow hwnd sW_SHOWNORMAL
            getLastError >>= print
            updateWindow hwnd
            doMsgPump)
