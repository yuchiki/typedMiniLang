# typedMiniLang

型推論器を書く練習

## 言語仕様

e0は以下は再帰を持たない式である。

e0 ::= x | b | n
    | e + e | e - e | e * e | e / e
    | e && e | e || e | !e
    | e < e | e ==_int e
    | if e then e else e
    | let x = e in e
    | fun x -> e | e e

T0 ::= Bool | Int | T0 -> T0

e ::= e0
T ::= T0


## ロードマップ
| 項目 | done?
| :-   | :-
| e0が定義される | not yet
| (e0がparseできる) | not yet
| e0が実行できる | not yet
| e0が型検査できる | not yet
