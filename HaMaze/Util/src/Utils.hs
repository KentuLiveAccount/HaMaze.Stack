{-# OPTIONS_GHC -fno-warn-tabs #-}
module Utils (
	asLines,
	asUninterspersed,
	before,
	blank,
	breakAt,
	breaks,
	breaks_NoDelim,
	bucketize,
	concatTpl,
	dropFileName,
	enclosed,
	exclude,
	expect,
	findAndExcludeFirst,
	forEachLines,
	forestGraph,
	hPutStrLn_,	
	hPutStrLn__,
	ifNothing,
	ifJust,
	if',
	infiniTale,
	inverse,
	is,
	isNot,
	iso,
	justOr,
	linesBy,
	whenMP,
	maxBy,
	minBy,
	minimumBy,
	maximumBy,
	withPaint,
	withPaintPaintStruct,
	digits,
	doAsLines,
	doForEachLines,
	doForEachLines_,
	doMsg,
	pump,
	doMsgPump,
	createClassWindowDo,
	doIf,
	dropTail,
	dropHeadAndTail,
	dropPrefix,
	dropUpto,
	list,
	lstTpl,
	nemptyF,
	onBreakJoin,
	onFile,
	onFst,
	onSnd,
	onStdInDoForEachLines,
	onStdInDoForEachLines_, 
	onStdInPutStrLnEachLinesWith,
	peekOffStrm,
	prefixMatch,
	printEachLineWith,
	prodZipWith,
	putStrLn_,
	putStrLn__,
	putStrsLn,
	putWordsLn,
	records,
	repeatF,
	replace,
	replace1,
	shiftToFst1,
	swap,
	sortOn,
	simplifyPath,
	splitEvery,
	splitForest,
	splitInto,
	splitOn,
	splits,
	splitTree,
	streamIn,
	tail',
	takeRange,
	toHex,
	transposeFill,
	treeGraph,
	tplList,
	twos,
	twosf,
	uniq,
	unJust,
	unJustOrError,
	unJustOrElse,
	unwrap,
	headOrError,
	unintersperse,
	unright,
	unrightE,
	unUtf8,
	virtsToDot,
	within, withinL, withinR, withinLR,
--	withMappedFileForRead,
    uniqBy,
    insertAfter,
    splitRange
	) where

import Control.Exception (bracket, bracket_, finally)
import Control.Exception.Base
import Control.Monad (MonadPlus, mzero, when)
import Control.Monad.State (runStateT, StateT, get, put, liftIO)
import Data.Char (ord)
import Data.Function (on)
import Data.List (sortBy, isPrefixOf, intercalate, partition, unfoldr)
import Data.Int (Int32)
import Data.IORef
import Data.List (sortBy, isPrefixOf)
import Data.Maybe (maybe, fromJust)
import Data.Tree (rootLabel, Tree (Node), Forest)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (Storable, sizeOf, peekByteOff)
import Graphics.Win32.GDI.Types (HDC, HWND, HMENU, peekRECT)
import Graphics.Win32.Window
import System.IO (stdout, hFlush, hPutStrLn)
import System.Win32.File (BY_HANDLE_FILE_INFORMATION (bhfiSize), getFileInformationByHandle, createFile, closeHandle, gENERIC_READ, fILE_SHARE_READ, oPEN_EXISTING, fILE_ATTRIBUTE_NORMAL)
-- import System.Win32.FileMapping (createFileMapping, mapViewOfFile, fILE_MAP_READ, c_UnmapViewOfFileFinaliser)
import System.Win32.Mem (pAGE_READONLY)
import System.Win32.Types (BYTE, HANDLE)
import Text.Printf

{- Either -}
unright :: Either a b -> b
unright (Right a) = a

unrightE :: (Show a) => Either a b -> b
unrightE (Right a) = a
unrightE (Left a) = error (show a)

{- Maybe -}
ifNothing :: Maybe a -> a -> Maybe a
ifNothing m a = maybe (Just a) (\_ -> Nothing) m

ifJust :: Maybe a -> (a -> b) -> b -> b
ifJust (Just a) f b = f a
ifJust Nothing _ b = b

justOr :: Maybe a -> a -> a
justOr (Just a) _ = a
justOr Nothing a = a

unJust :: Maybe a -> a
unJust (Just a) = a

unJustOrError :: String -> (Maybe a) -> a
unJustOrError _ (Just a) = a
unJustOrError str _ = error str

unJustOrElse :: a -> (Maybe a) -> a
unJustOrElse _ (Just a) = a
unJustOrElse a _ = a

headOrError :: String -> [a] -> a
headOrError _ (a:_) = a
headOrError str _ = error str

expect :: Bool -> String -> a -> a
expect True _ = id
expect _ str = error str

{- Data.Function ? -}
if' :: Bool -> a -> a -> a
if' f a b = if f then a else b

doIf :: Bool -> (a -> a) -> a -> a
doIf True f a = f a
doIf False _ a = a

repeatF :: (a -> a) -> Int -> a -> a
repeatF f i a
	| i <= 0 = a
	| otherwise = repeatF f (i - 1) (f a)


{- Data.List -}
bucketize :: (Eq b) => (a -> b) -> [a] -> [[a]]
bucketize _ [] = []
bucketize fn (a:as) = (a:as') : (bucketize fn as'')
	where
		(as', as'') = partition (\x -> (fn x) == (fn a)) as

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f as = sortBy (compare `on` f) as

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = [[]]
splitEvery i lst = [x] ++ splitEvery i xs
	where
            (x, xs) = splitAt i lst

splitInto :: Int -> [a]-> [[a]]
splitInto i a = splitEvery' cchChunk cchMod (i - 1) a
	where
		cch = length a
		cchChunk = cch `div` i
		cchMod = cch `mod` i
		chunkMod c 0 = c
		chunkMod c m = c + 1
		floorDec 0 = 0
		floorDec i = i - 1
		splitEvery' _ _ 0 a = [a]
		splitEvery' cchChunk cchMod i a = x : (splitEvery' cchChunk (floorDec cchMod) (i - 1) xs)
			where
				(x, xs) = splitAt  (chunkMod cchChunk cchMod) a

dropUpto :: (a -> Bool) -> [a] -> [a]
dropUpto f = tail . dropWhile (not . f)

splits :: [a] -> [([a], [a])]
splits as = _splits [] as
	where
		_splits :: [a] -> [a] -> [([a], [a])]
		_splits preRev [] = [(reverse preRev, [])]
		_splits preRev post@(p:ps) = (reverse preRev, post) : _splits (p:preRev) ps

breakAt :: (Eq a) => [a] -> [a] -> ([a], [a])
breakAt _ [] = ([], [])
breakAt pat src = 
	case (pat `isPrefixOf` src) of
		True -> ([], drop (length pat) src)
		False -> ((head src) : sa, sb)
	where
		(sa, sb) = breakAt pat (tail src)

splitOn1 :: (Eq a) => [a] -> [a] -> [[a]]
splitOn1 _ [] = [[]]
splitOn1 pat src = sa : [sb]
	where
		(sa, sb) = breakAt pat src

replace1 :: (Eq a) => [a]                 -- ^ Text to search for
        -> [a]                 -- ^ Replacement text
        -> [a]                 -- ^ Input text
        -> [a]
replace1 s d = intercalate d . splitOn1 s

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn [] src = [src]
splitOn _ [] = []
splitOn pat src =
	case (breakAt pat src) of
		(sa, []) -> if' (sa ++ pat == src) [sa,[]] [sa]
		(sa, sb) -> sa : splitOn pat sb

{-
replace :: (Eq a) => [a]                 -- ^ Text to search for
        -> [a]                 -- ^ Replacement text
        -> [a]                 -- ^ Input text
        -> [a]
replace s d = intercalate d . splitOn s
-}

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace key repl src = case (filter (isPrefixOf key . snd) (splits src)) of
	[] -> src
	((a, b):_) -> a ++ repl ++ (drop (length key) b)

prefixMatch :: (Eq a) => [a] -> [a] -> Bool
prefixMatch = isPrefixOf

dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix [] str = Just str
dropPrefix (p:ps) (s:ss)
	| p == s   = dropPrefix ps ss
	| otherwise = Nothing

nemptyF :: [a] -> ([a] -> [b]) -> [b]
nemptyF [] _ = []
nemptyF as f = f as

records :: (Eq a) => [a] -> [a] -> [[a]]
records key ins = unfoldr (fn key) (tail $ splits ins)
	where
		fn :: (Eq a) => [a] -> [([a], [a])] -> Maybe ([a], [([a], [a])])
		fn k as = case (dropWhile (not . isPrefixOf k . snd) as) of
			((a, b):_) -> Just (a, tail $ splits b)
			[]         -> Nothing

unwrap :: [a] -> [a]
unwrap [] = []
unwrap (a:as) = reverse $ tail $ reverse as

before :: (Eq a) => a -> [a] -> Maybe a
before _ [] = Nothing
before _ [a] = Nothing
before a (a1:a2:as)
    | a2 == a = Just a1
    | otherwise = before a (a2:as)

exclude :: (Eq a) => a -> [a] -> [a]
exclude _ [] = []
exclude k (a:as)
    | k == a = as
    | otherwise = a: (exclude k as)

findAndExcludeFirst :: (a -> Bool) -> [a] -> ([a], Maybe a)
findAndExcludeFirst f as = (fs ++ rs', h)
	where
		(fs, rs) = break f as
		(h, rs') = if' (null rs) (Nothing, []) (Just $ head rs, tail rs)

asUninterspersed :: Char -> ([String] -> [String]) -> String -> String
asUninterspersed ch f = intercalate [ch] . f . unintersperse (==ch)

asLines :: ([String] -> [String]) -> String -> String
asLines f = unlines . f . lines

doAsLines :: ([String] -> IO a) -> String -> IO a
doAsLines f = f . lines

forEachLines :: (String -> String) -> String -> String
forEachLines f = unlines . map f . lines

doForEachLines :: (String -> IO a) -> String -> IO [a]
doForEachLines f = mapM f . lines

doForEachLines_ f str = doForEachLines f str >> return ()

printEachLineWith :: (String -> String) -> String -> IO()
printEachLineWith f str = doForEachLines_ (putStrLn . f) str

onStdInDoForEachLines :: (String -> IO a) -> IO [a]
onStdInDoForEachLines f = getContents >>= doForEachLines f

onStdInDoForEachLines_ :: (String -> IO ()) -> IO ()
onStdInDoForEachLines_ f = getContents >>= doForEachLines_ f

onStdInPutStrLnEachLinesWith :: (String -> String) -> IO ()
onStdInPutStrLnEachLinesWith f = onStdInDoForEachLines_ $ putStrLn . f

twos :: [a] -> [(a, a)]
twos [] = []
twos (a1: a2: as) = (a1, a2) : (twos as)

twosf :: (a -> a -> b) -> [a] -> [b]
twosf f as = map (uncurry f) $ twos as

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x: xs)
	| x == x' = xs'
	| otherwise = x : xs'
	where
		xs'@(x':_) = uniq xs

head' :: [a] -> [a]
head' [] = []
head' (a:as) = [a]

tail' :: [a] -> [a]
tail' [] = []
tail' (a:as) = as

unintersperse :: (a -> Bool) -> [a] -> [[a]]
unintersperse f as = case (dropWhile f as) of
	[] -> []
	s' -> w : unintersperse f  s''
		where (w, s'') = break f s'

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks _ [] = []
breaks f as = (aa ++ (head' aas)) : breaks f (tail' aas)
	where
		(aa, aas) = break f as

breaks_NoDelim :: (a -> Bool) -> [a] -> [[a]]
breaks_NoDelim _ [] = []
breaks_NoDelim f as = aa : breaks_NoDelim f (tail' aas)
	where
		(aa, aas) = break f as

list :: a -> [a]
list a = [a]

dropTail :: [a] -> [a]
dropTail = reverse . tail . reverse

dropHeadAndTail :: [a] -> [a]
dropHeadAndTail = reverse . tail . reverse .tail

onBreakJoin :: (a -> Bool) -> ([a] -> [a] -> [a]) -> [a] -> [a]
onBreakJoin f fjoin as = case (break f) as of
	(_, []) -> as
	(before, after) -> fjoin before after

linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy = unintersperse

takeRange :: Int -> Int -> [a] -> [a]
takeRange iB iE as = take (iE - iB) $ drop iB as

transposeFill :: [[a]] -> [[a]]
transposeFill [] = []
transposeFill xss
	| allOne xss = xss
	| otherwise  = xs : transposeFill xss'
	where
		xs   = map (head) xss
		xss' = map (tailIfTwo) xss

		tailIfTwo (a:as)
			| null as = [a]
			| otherwise = as

		allOne = and . map (null . tail)

infiniTale :: a -> [a] -> [a]
infiniTale t as = as ++ (repeat t)

enclosed :: (Eq a) => [a] -> [a] -> Maybe [a]
enclosed _ [] = Nothing
enclosed dlms (h:as)
	| not (elem h dlms)  = Nothing
	| null as   = Nothing
	| t /= h    = Nothing
	| null ras  = Nothing
	| otherwise = Just $ reverse ras
	where
		(t:ras) = reverse as


blank :: String -> String
blank = map (const ' ')

prodZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
prodZipWith f as bs = concatMap (\a -> zipWith f (repeat a) bs) as

{- Data.Eq -}
is :: (Eq a) => a -> a -> Bool
is a b = a == b

isNot :: (Eq a) => a -> a -> Bool
isNot a b = a /= b


{- Data.String -}
digits :: String -> String
digits = filter (flip elem ['0'..'9'])

toHex i a = printf (concat ["0x%0", show i, "x"]) a


{- string as filepath -}
dropFileName :: String -> String
dropFileName = reverse . snd . break (== '\\') . reverse

simplifyPath :: String -> String
simplifyPath path = intercalate "\\" (f: fs') 
	where
		(f:fs) = breaks_NoDelim (== '\\') path
		fs' = reverse $ simplify $ reverse fs
		simplify [] = []
		simplify ("..":_:fs) = fs
		simplify (f:fs) = f: simplify fs 

{- ?.GraphVis? -}
fmtVirt :: (String, String) -> String
fmtVirt (stra, strb) = stra ++ " -> " ++ strb ++ ";\n"

virtsToDot :: [(String, String)] -> String
virtsToDot vs = "digraph find {\n" ++ (concatMap fmtVirt) vs ++ "}\n"


{- Data.Tree -}
forestGraph :: Forest a -> [(a, a)]
forestGraph fr = concatMap treeGraph fr

treeGraph :: Tree a -> [(a, a)]
treeGraph (Node a brs) = treeGraph' a brs

treeGraph' :: a -> Forest a -> [(a, a)]
treeGraph' a brs = (map ((\b -> (a,b)) . (rootLabel)) brs) ++ (concatMap treeGraph brs)

splitTree :: Tree a -> [[a]]
splitTree (Node a []) = [[a]]
splitTree (Node a bs) = map (a:) (concatMap splitTree bs)

splitForest :: Forest a -> [[a]]
splitForest fss = concatMap (splitTree) fss


{- Data.Tuple -}
swap ::(a, b) -> (b, a)
swap (a, b) = (b, a)

iso :: Eq a => (a, a) -> Bool
iso (a, b) = a == b

concatTpl :: ([a], [a]) -> [a]
concatTpl = concat . tplList

tplList :: (a, a) -> [a]
tplList (a, b) = [a, b]

lstTpl :: [a] -> (a, a)
lstTpl (a : b: _) = (a, b)

onFst :: (a->b) -> (a, c) -> (b, c)
onFst f (a, b) = (f a, b)

onSnd :: (a->b) -> (c, a) -> (c, b)
onSnd f (a, b) = (a, f b)

shiftToFst1 :: ([a],[a]) -> ([a],[a])
shiftToFst1 b@(_, []) = b
shiftToFst1 (a, b:bs) = (a ++ [b], bs)

{- Control.Monad -}
whenMP :: (MonadPlus m) => Bool -> a -> m a
whenMP f a = if' f (return a) mzero

whenMP' :: (MonadPlus m) => Bool -> m a -> m a
whenMP' f a = if' f a mzero

{- Prelude? -}
minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f a b = if' ((f a) < (f b)) a b

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f a b = if' ((f a) > (f b)) a b

minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy f = foldl1 (minBy f)

maximumBy :: (Ord b) => (a -> b) -> [a] -> a
maximumBy f = foldl1 (maxBy f)

{- Graphics.Win32.Window -}
withPaint :: HWND -> (HWND -> HDC -> IO a) -> IO a
withPaint hwnd fn = allocaPAINTSTRUCT $ \ lpps -> do
	hdc <- beginPaint hwnd lpps
	finally (fn hwnd hdc) $ endPaint hwnd lpps


bEraseOffset = if' (sizeofPAINTSTRUCT < 72) 4 8
bRectOffset = if' (sizeofPAINTSTRUCT < 72) 8 12

readLPPS :: LPPAINTSTRUCT -> IO PAINTSTRUCT
readLPPS lpps = do
	hdc <- peekByteOff (castPtr lpps) 0
	fErase <- (peekByteOff lpps  bEraseOffset) :: IO Int32
	rect <- peekRECT (castPtr $ lpps `plusPtr` bRectOffset)
	return (hdc, fErase /= 0, rect)

{- Graphics.Win32.Window -}
withPaintPaintStruct :: HWND -> (HWND -> PAINTSTRUCT  -> IO a) -> IO a
withPaintPaintStruct hwnd fn = allocaPAINTSTRUCT $ \ lpps -> do
	hdc <- beginPaint hwnd lpps
	ps <- readLPPS lpps
	finally (fn hwnd ps) $ endPaint hwnd lpps

{- Graphics.Win32.Window ? -}
doMsg :: (Monad m) => a -> m b -> m a
doMsg i f = f >> return i


onRegisterClassDo :: WNDCLASS -> (IO a) -> IO a
onRegisterClassDo wc@(_, hi, _, _, _, _, cn) fnDo = bracket_ (registerClass wc >>= return . fromJust) (unregisterClass cn hi) fnDo

createClassWindowDo :: WNDCLASS -> String -> WindowStyle -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe HWND -> Maybe HMENU -> WindowClosure -> (HWND -> IO a) -> IO a
createClassWindowDo wc@(_, hi, _, _, _, _, cn) strTitle ws mX mY mDx mDy mHwnd mHMenu winProc fnDo =
	onRegisterClassDo wc (createWindow cn strTitle ws mX mY mDx mDy mHwnd mHMenu hi winProc >>= fnDo)

pump :: LPMSG -> IO ()
pump lpmsg = do
	fContinue <- getMessage lpmsg Nothing
	when fContinue $ do
		translateMessage lpmsg
		dispatchMessage lpmsg
		pump lpmsg

doMsgPump :: IO ()
doMsgPump = allocaMessage $ pump

{- String.Encoding? -}
unUtf8 :: String -> String
unUtf8 [] = []
unUtf8 (ch:chs)
	| (ord ch) < 0x80 = ch : (unUtf8 chs)
	| (ord ch) < 0xc0 = "Invalid UTF-8 Sequence"
	| (ord ch) < 0xe0 = '?' : (unUtf8 $ tail chs)
	| (ord ch) < 0xf0 = '?' : (unUtf8 $ tail $ tail chs)
	| (ord ch) < 0xf8 = '?' : (unUtf8 $ tail $ tail $ tail chs)
	| (ord ch) < 0xfc = '?' : (unUtf8 $ tail $ tail $ tail $ tail chs)
	| (ord ch) < 0xfc = '?' : (unUtf8 $ tail $ tail $ tail $ tail $ tail chs)
	| (ord ch) < 0xfe = '?' : (unUtf8 $ tail $ tail $ tail $ tail $ tail $ tail chs)
	| otherwise = "Invlaid UTF-8 Sequence"

{- Data.Ord? -}
inverse :: Ordering -> Ordering
inverse LT = GT
inverse GT = LT
inverse EQ = EQ

within, withinL, withinR, withinLR :: (Ord a) => a -> a -> a -> Bool
within l r v = l < v && v < r
withinL l r v = l <= v && v < r
withinR l r v = l < v && v <= r
withinLR l r v = l <= v && v <= r

{- Data.IORef -}
onIORef :: IORef a -> (a -> a) -> IO ()
onIORef = modifyIORef

{- System.IO -}
hPutStrLn__ h str = do
	hPutStrLn h str
	hFlush h

hPutStrLn_ = hPutStrLn__


putStrLn__ str = hPutStrLn stdout str
putStrLn_ = hPutStrLn__

putStrsLn :: [String] -> IO ()
putStrsLn = putStrLn . concat
putWordsLn = putStrLn . unwords

{- Foreign.Ptr -}
peekOffStrm :: (Storable a) => StateT (Ptr BYTE) IO a
peekOffStrm = do
	ptr <- get
	v <- liftIO $ (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
	put $ ptr `plusPtr` (sizeOf v)
	return v

streamIn :: (StateT (Ptr BYTE) IO a) -> (Ptr BYTE) -> IO a
streamIn m p = runStateT m p >>= return . fst

{- System.Win32.File -}
onFile :: FilePath -> (HANDLE -> IO a) -> IO (a)
onFile path f = do
	bracket
		(createFile path gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing)
		(closeHandle)
		$ \fh -> f fh

{- System.Win32.FileMapping 
withMappedFileForRead :: FilePath -> ((Ptr a, Int) -> IO b) -> IO b
withMappedFileForRead path f = do
	onFile path $ \fh -> bracket
            (createFileMapping (Just fh) pAGE_READONLY 0 Nothing)
            (closeHandle)
            $ \fm -> do
                fi <- getFileInformationByHandle fh
                ptr <- mapViewOfFile fm fILE_MAP_READ 0 0
                fp <- newForeignPtr c_UnmapViewOfFileFinaliser ptr
                withForeignPtr fp $ \p -> f (p, fromIntegral $ bhfiSize fi)
-}

uniqBy :: (Eq b) => (a -> b) -> [a] -> [a]
uniqBy f [] = []
uniqBy f [a] = [a]
uniqBy f (a:as) = a : (uniqBy f (nelemBy f a as))
    where
        nelemBy :: (Eq b) => (a -> b) -> a -> [a] -> [a]
        nelemBy f a as = filter (\x -> f a /= f x) as

insertAfter :: (Eq a) => a -> a -> [a] -> [a]
insertAfter idAfter idInsert ids = as ++ (b:idInsert:bs)
    where
        (as, b:bs) = span (idAfter /=) ids

splitRange :: [a] -> (Int, Int) -> ([a], [a], [a])
splitRange para (ichFirst, ichLim) = (f, m, l)
    where
        (f', l) = splitAt ichLim para
        (f, m) = splitAt ichFirst f'

