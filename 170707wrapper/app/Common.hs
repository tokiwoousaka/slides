{-# LANGUAGE FlexibleInstances #-}
module Common where
import Control.Monad.Takahashi
import Control.Lens

setOptions :: Taka ()
setOptions = do
  slideFontSize .= Just 60
  -- title
  titleOption.bgColor .= Just (Color 100 100 100)
  titleOption.fontColor .= Just (Color 255 255 255)
  titleOption.frameColor .= Just (Color 150 150 150)
  -- contents
  contentsOption.bgColor .= Nothing
  contentsOption.fontColor .= Just (Color 0 0 0)
  contentsOption.frameColor .= Just (Color 255 255 255)
  -- contents2
  contentsOption2.bgColor .= Just (Color 200 200 200)
  contentsOption2.fontColor .= Just (Color 0 0 0)
  contentsOption2.frameColor .= Just (Color 255 255 255)
  -- code
  codeOption.fontColor .= Just (Color 0 0 0)
  codeOption.frameColor .= Just (Color 0 0 0)
  codeOption.blockFontSize .= Just 40


-- 見出しページ付与
header :: String -> Taka () -> Taka ()
header s t = do
  stateSandbox $ do
    slideTitle .= ""
    slideFontSize .= Just 60
    taka2 s
  t

big :: String -> Taka () 
big s = do
  stateSandbox $ do
    contentsOption.blockFontSize .= Just 80
    par s

bigList :: [String] -> Taka () 
bigList ss = do
  stateSandbox $ do
    contentsOption.blockFontSize .= Just 80
    list ss

{- tiwnBottomやtwinTopで使う事を想定し、
要素数が二つ以上ある事をある程度保証するためのHack
LikeContentのInstance定義が必ず1要素以上返し
(~~~~)演算子を通してこの値が作られるなら、
要素数が2要素以上なのを保証できる -}
newtype JoinedContents = JoinedContents [Contents]

infixr 3 ====
(====) :: LikeContent c => String -> c -> Taka ()
l ==== r = do
  title <- use slideTitle
  slideTitle .= l
  vertical $ toContents r
  slideTitle .= title

infixr 3 .===
(.===) :: String -> JoinedContents -> Taka ()
l .=== (JoinedContents (x : y : _)) = do
  title <- use slideTitle
  slideTitle .= l
  twinTop x y
  slideTitle .= title

infixr 3 ===.
(===.) :: String -> JoinedContents -> Taka ()
l ===. (JoinedContents (x : y : _)) = do
  title <- use slideTitle
  slideTitle .= l
  twinBottom x y
  slideTitle .= title

class LikeContent c where
  toContents :: c -> [Contents]
  toContents2 :: c -> [Contents]
instance LikeContent String where
  toContents s = [takaCont s]
  toContents2 s = [takaCont2 s]
instance LikeContent [String] where
  toContents xs = [listCont xs]
  toContents2 xs = [listCont2 xs]
instance LikeContent JoinedContents where
  toContents (JoinedContents c) = c
  toContents2 (JoinedContents c) = c

data StringFormat = Par String | Code String
instance LikeContent StringFormat where
  toContents (Par s) = [parCont s]
  toContents (Code s) = [codeCont s]

  toContents2 (Par s) = [parCont2 s]
  toContents2 (Code s) = [codeCont s]

data ImageFormat = Image DrawType String
instance LikeContent ImageFormat where
  toContents (Image d f) = [imgCont d f]
  toContents2 (Image d f) = [imgCont d f]

infixl 4 ~~~~
(~~~~) :: (LikeContent a, LikeContent b) => a -> b -> JoinedContents
l ~~~~ r = let
    l' = toContents l
    r' = if odd $ length l' - 1 then toContents r else toContents2 r
  in JoinedContents $ l' ++ r'

