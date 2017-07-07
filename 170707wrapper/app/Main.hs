module Main where
import Common
import Control.Lens
import Control.Monad.Takahashi
import Control.Monad.State

main :: IO ()
main = do
  let fileName = "../slide.html"
  writeSlide fileName presentation
  putStrLn $ "Sucess : Output to '" ++ fileName ++ "'"

presentation :: Taka ()
presentation = do
  setOptions
  title "ラッパーテスト" 
    $  "2017/7/17 ちゅーん(@its_out_of_tune)"

  header "テスト" test

  slideTitle .= ""
  taka "ありがとうございました\n(๑•﹏•)"

test :: Taka ()
test = do
  "高橋メソッド" ==== 
    "ほげ"
  "高橋メソッド" ==== 
    "ぴよ"
  "高橋メソッド" ==== 
    "ふが"
  slideTitle .= "高橋メソッドは"
  taka "元の\n書きかたの方が"
  taka "いいかも\nしれない"
  "ちょっと長めの文章" ==== 
    Par
      "むかしむかし あるところに\n\
      \おじいさんと おじいさんが住んでいました\n\
      \二人は愛しあっていました\n\
      \めでたしめでたし"
  "リスト" ==== 
    [ "ゆい"
    , "ゆずこ"
    , "ゆかり"
    , "あいちん"
    , "ふみお"
    , "おかちー"
    ]
  "画像" ====
    Image HStretch "img/neko.png"
  "二段" ==== 
      "一段目"
    ~~~~
      "二段目"
  "三段" ==== 
      "一段目"
    ~~~~
      "二段目"
    ~~~~
      "三段目"
  "四段" ==== 
      "一段目"
    ~~~~
      "二段目"
    ~~~~
      "三段目"
    ~~~~
      "四段目"
  "縦分割" ==== 
    Par
      "ない\n\nあんま使わないから\n作らないかも"
  "コード" ====
      Par "コードの説明とか"
    ~~~~ 
      Code 
        "main :: IO ()\n\
        \main = putStrLn \"Hello, World!\""
  "二段 上広め" .===
      "ひろい"
    ~~~~
      "せまい"
  "二段 下広め" ===.
      "せまい"
    ~~~~
      "ひろい"
