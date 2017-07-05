module Main where
import Control.Lens
import Control.Monad.Takahashi
import Control.Monad.State
import Common

main :: IO ()
main = do
  let fileName = "../slide.html"
  writeSlide fileName presentation
  putStrLn $ "Sucess : Output to '" ++ fileName ++ "'"

presentation :: Taka ()
presentation = do
  setOptions
  title "ねこでもわかる Lens, Prism" 
    $  "2017/7/05 ちゅーん(@its_out_of_tune)\n"
    ++ "for でこれきさん迎撃会"

  header "自己紹介" profile
  header "はじめに" first
  header "Lensってなに" whatLens
  header "Lensの仕組み" how2worklens
  header "直和型とPrism" whatPrism
  header "Prismの実現方法" how2workprism
  header "Prismの\nちょっと良い話" prismDef

  slideTitle .= ""
  taka "ありがとうございました\nd(*＇-＇*)b ｲｴｰｲ"

first :: Taka ()
first = do
  slideTitle .= "はじめに"
  taka "目標捕捉"
  slideTitle .= "これはねこです"
  stateSandbox $ do
    contentsOption2.blockFontSize .= Just 40
    twinLeft
      ( listCont
        [ "名古屋のワイルドカード"
        , "弊基盤チームの即戦力"
        , "だいたい何でも出来る\n\n"
        , "つまり怖いひと"
        ]
      )
      ( verticalCont
        [ imgCont HStretch "../img/deco_icon.jpg"
        , parCont2
          $  "HN:　でこれき\n\n"
          ++ "Twitter:\n"
          ++ "　@dico_leque"
        ]
      )
  slideTitle .= "注意"
  taka "このスライドは\nねこ向けです"
  taka "皆頑張ってね(^q^)"

whatLens :: Taka ()
whatLens = do
  slideTitle .= "Lensとは"
  taka "Haskellのライブラリ"
  vertical
    [ parCont "Haskellが苦手なこと"
    , codeCont 
      $  "-- 999の部分をピンポイントで取り出したい\n"
      ++  "(1, (2, 3, (4, 999)), 6) -- 毎回パターンマッチ書きたい？\n"
      ++ "-- 5 の部分だけ書きかえたい\n"
      ++  "(1, (2, 3, (4, 5)), 6) -- くぁwせdrftgyふじこlp"
    ] 
  vertical
    [ parCont "lensならこう書ける"
    , codeCont 
      $  "(1, (2, 3, (4, 999)), 6)^._2._3._2 -- 999\n"
      ++ "(1, (2, 3, (4, 5)), 6)&_2._3._2.~999 -- (1, (2, 3, (4, 999)), 6)\n\n"
    ] 
  vertical
    [ parCont 
      $  "_2, _3 みたいなのをアクセッサと呼ぶ[要出展]\n"
      ++ "ただの関数合成で繋げていることに注目"
    , codeCont 
      $  "(1, (2, 3, (4, 999)), 6)^._2._3._2 -- 999\n"
      ++ "(1, (2, 3, (4, 5)), 6)&_2._3._2.~999 -- (1, (2, 3, (4, 999)), 6)\n\n"
    ] 
  par 
    $  "もちろん、任意の型に対して\n"
    ++ "アクセッサを作ることができる"

how2worklens :: Taka ()
how2worklens = do
  slideTitle .= "Lensのアクセッサ"
  twinTop
    ( parCont "関数の型をtype宣言しただけ" )
    ( codeCont 
      $  "type Lens s t a b\n"
      ++ "  = forall f. Functor f => (a -> f b) -> s -> f t" ) 
  twinTop
    ( listCont 
      [ "a : sが内包する型"
      , "b : tが内包する型"
      , "s : 操作したい元の型"
      , "t : 値の書きかえ後の型"
      ] )
    ( codeCont 
      $  "type Lens s t a b\n"
      ++ "  = forall f. Functor f => (a -> f b) -> s -> f t" ) 
  vertical
    [ parCont 
      $  "s と t の型が違う理由、\n"
      ++ "タプルの例がわかりやすいんです"
    , codeCont 
      $  "type Lens (Int, x) (String, x) Int String \n"
      ++ "  = forall f. Functor f => \n"
      ++ "  (Int -> f String) -> (Int, x) -> f (String, x)"
    ]
  vertical
    [ parCont 
      $  "f に入るのは Identity または Const\n"
      ++ "どっちもFunctor、ねこ向けなので詳細は割愛"
    , codeCont 
      $  "Identity a = Identity a\n"
      ++ "Const a b = Const a"
    ]
  vertical
    [ parCont 
      $  "値の書き替え\n"
      ++ "Identityの部分は無視して読めるので、ただのmap関数"
    , codeCont 
      $  "type Lens (Int, x) (String, x) Int String \n"
      ++ "  = (Int -> Identity String) -> (Int, x) -> Identity (String, x)"
    ]
  vertical
    [ parCont 
      $  "値の取り出し\n"
      ++ "Constの第二引数は無視できる事に注目"
    , codeCont 
      $  "type Lens (Int, x) (Int, x) Int String \n"
      ++ "  = (Int -> Const Int Int) -> (Int, x) -> Const Int (Int, x)"
    ]
  vertical
    [ parCont 
      $  "Functorの制約が必要な理由\n"
      ++ "型あわせのため"
    , codeCont 
      $  "_1 :: Lens (a, v) (b, v) a b\n"
      ++ "_1 f (x, y) = fmap (\\x' -> (x', y)) (f x)"
    ]
  
whatPrism :: Taka ()
whatPrism = do
  slideTitle .= "Lensの限界とPrism"
  twinBottom 
    ( parCont "直和型が混ざるとLensだけではキツい" )
    ( codeCont 
      $  "-- Getしたい\n"
      ++ "print $ case foo^._2 of \n"
      ++ "  Left inner -> Just (inner^._1)\n"
      ++ "  Right _ -> Nothing\n\n"
      ++ "-- Setしたい\n"
      ++ "(_2%~(\\case \n"
      ++ "  Left x -> Left $ x&_1.~111\n"
      ++ "  Right x -> Right x)) foo\n"
    )
  twinBottom
    ( parCont "Prismならこう書ける" )
    ( codeCont 
      $  "let rightHoge = Right \"Hoge\" :: Either Int String\n\n"
      ++ "-- Setする時、マッチしない場合は何もしない\n"
      ++ "rightHoge&_Right.~\"Piyo\" -- Right \"Piyo\"\n"
      ++ "rightHoge&_Left.~999  -- Right \"Hoge\"\n\n"
      ++ "-- Getの場合、結果はMaybe型なので演算子が変わる\n"
      ++ "rightHoge^?_Left  -- Nothing\n"
      ++ "rightHoge^?_Right -- Just \"Hoge\"" )
  vertical
    [ parCont 
      $  "ただし、取得したい値がMonoidの場合に限り\n"
      ++ "Lensと同じ演算子でGetできる(マッチしなかったらmempty)"
    , codeCont
      $  "Just \"hoge\"^._Just                  -- OK \"hoge\"\n"
      ++ "Just 114514^._Just                  -- NG\n"
      ++ "Just (Sum 184)^._Just               -- OK Sum { getSum = 184 }\n"
      ++ "(Nothing :: Maybe (Sum Int))^._Just -- OK Sum { getSum = 0 }\n"
    ]
  twinBottom
    ( parCont "Lensと合成した場合、全体はPrismになる" )
    ( codeCont
      $  "let foo = (1, Left (999, 3), 4) \n"
      ++ "    :: (Int, Either (Int, Int) String, Int)\n"
      ++ "let bar = (1, Right \"Test\", 4) \n"
      ++ "    :: (Int, Either (Int, Int) String, Int)\n\n"
      ++ "print $ foo^?_2._Left._1     -- Just 999\n"
      ++ "print $ foo&_2._Left._1.~111 -- (1,Left (111,3),4) \n"
      ++ "print $ bar^?_2._Right       -- Just \"Test\"\n"
      ++ "print $ bar^._2._Rightn      -- \"Test\"\n"
    )
  par "機能だけ見れば\nはぁ、そういう考え方ね？\n\nってなるんですが"
  taka "もちろん"
  taka "ここまで全部"
  taka "型安全ですよ？？"

how2workprism :: Taka ()
how2workprism = do
  slideTitle .= "Prismってどうなってんの"
  twinTop
    ( parCont 
      $  "実際の定義の話は後ほど少しだけ触れるが\n" 
      ++ "Prismは概ねLensのfの制約を、Applicativeまで\n"
      ++ "引きあげたものと考えれば良い" )
    ( codeCont 
      $  "type Prism s t a b\n"
      ++ "  = forall f. Applicative f => (a -> f b) -> s -> f t" ) 
  par
    $  "型レベルの話に限定すれば、f = Identityとする事で、\n"
    ++ "Setする場合は問題無いことがわかると思う\n\n"
    ++ "IdentityはApplicativeのインスタンスなので、\n"
    ++ "アクセッサの実装次第でどうとでもなりそうだ。\n\n"
    ++ "問題は、Getする場合である、つまり……"
  twinTop
    ( takaCont "Constは\nApplicativeにならない" )
    ( codeCont 
      $  "instance Applicative (Const r) where\n"
      ++ "  pure _ = Const ?????\n"
      ++ "  _ <*> _ = Const ?????" )
  taka "ではどうするか？"
  taka "ここから怒涛"
  twinTop
    ( takaCont "Constは、中がモノイドなら\nApplicativeに出来る" )
    ( codeCont 
      $  "instance Monoid m => Applicative (Const m) where\n"
      ++ "  pure _ = Const mempty -- Getの時マッチしなかったらmempty返しとけば良い\n"
      ++ "  Const f <*> Const v = Const (f `mappend` v)" )
  vertical
    [ takaCont "解決1"
    , parCont "MonoidならGetできるようになった"
    ] 
  taka "で、"
  twinTop
    ( takaCont "Maybeって\nMonoidになるんすよねw" )
    ( codeCont 
      $  "instance Monoid (Maybe a) where\n"
      ++ "  mempty = Nothing -- Getの時マッチしなかったらNothingを返したい\n"
      ++ "  r@(Just _) `mappend` _  = r\n"
      ++ "  Nothing `mappend` r = r" )
  vertical
    [ takaCont "解決2"
    , parCont "Getしてマッチしなかったら\nMaybe型が返せるようになった"
    ] 
  par "っていうか"
  taka "もとから\nそうなってた"
  par 
    $  "考え方 : LensはFunctorの性質を利用して\n"
    ++ "様々な構造に対してGetやSetの仕組みを与える。\n\n"
    ++ "しかし、直和型の場合\n"
    ++ "パターンマッチに失敗するかもしれない。\n\n"
    ++ "Setの時は単純に無視すれば良いが、Getの場合そうもいかない"
    ++ "この時、初期値の役割を果すのがApplicativeのpureである"
  
prismDef :: Taka ()
prismDef = do
  slideTitle .= "Prismの真の姿"
  twinBottom
    ( parCont 
      $  "Prismの実際の定義と\n"
      ++ "さっきお見せした定義を比較")
    ( codeCont
      $  "-- さっきの\n"
      ++ "type Prism s t a b\n"
      ++ "  = forall f. Applicative f => (a -> f b) -> s -> f t\n\n" 
      ++ "-- 実際\n"
      ++ "type Prism s t a b\n"
      ++ "  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)"
    )
  par "なんか関数減ったね。"

profile :: Taka ()
profile = do
  slideTitle .= "自己紹介"
  stateSandbox $ do
    contentsOption2.blockFontSize .= Just 40
    twinLeft
      ( listCont
        [ "野生のHaskller(29♂)"
        , "2016年春よりなごやか"
        , "OTR 基盤チームの道化枠\n\n"
        , "雑なゲーム実況"
        , "イカ、ゼルダ、ぷよぷよ"
        , "最近ラット飼いたい"
        ]
      )
      ( verticalCont
        [ imgCont WStretch "../img/my_icon2.gif"
        , parCont2
          $  "HN:　ちゅーん\n\n"
          ++ "Twitter:\n"
          ++ "　@its_out_of_tune\n"
          ++ "Github:\n"
          ++ "　tokiwoousaka\n"
        ]
      )
  taka "たぶん\nだいたいLensのひと"

