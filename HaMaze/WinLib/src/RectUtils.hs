{-# OPTIONS_GHC -fno-warn-tabs #-}
module RectUtils (
    boundWithinRect,
    moveRect,
    yTop,
    setTop,
    setBottom,
    intersects
) where

import System.Win32.Types
import Graphics.Win32.GDI.Types
import Utils (if')
import Debug.Trace

traceRcs :: RECT -> RECT -> RECT -> RECT
--traceRcs rc1 rc2 rc3 = trace (unwords [show rc1, show rc2, show rc3]) rc3
traceRcs rc1 rc2 rc3 = rc3

boundWithinRect :: RECT -> RECT -> RECT
boundWithinRect (x, y, sx, sy) (x1, y1, x2, y2) = traceRcs (x, y, sx, sy) (x1, y1, x2, y2) r3
	where
		r3 = (x1', y1', x2', y2')
		(x1', x2') = bound x1 x2 x sx
		(y1', y2') = bound y1 y2 y sy
		bound p1 p2 r s = if' (p1 <= r) (r, r + p2 - p1) $ if' (p2 > s) (p1 - (p2 - s), s) (p1, p2)

moveRect :: SIZE -> RECT -> RECT
moveRect (sx, sy) (x1, y1, x2, y2) = (x1 + sx, y1 + sy, x2 + sx, y2 + sy)

yTop :: RECT -> LONG
yTop (_, y, _, _) = y

setTop :: RECT -> LONG -> RECT
setTop (x1, _, x2, y2) y1 = (x1, y1, x2, y2)

setBottom :: RECT -> LONG -> RECT
setBottom (x1, y1, x2, _) y2 = (x1, y1, x2, y2)

intersects :: RECT -> RECT -> Bool
intersects (xt1, yt1, xb1, yb1) (xt2, yt2, xb2, yb2) = (overlap (xt1, xb1) (xt2, xb2)) && (overlap (yt1, yb1) (yt2, yb2))
	where
		overlap (t1, b1) (t2, b2) = (t1 <= t2 && t2 <= b1) || (t2 <= t1 && t1 <= b2)
