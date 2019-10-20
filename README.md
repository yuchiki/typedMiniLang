# typedMiniLang

型推論器を書く練習

## 言語仕様

e0 ::= x | b | n
    | e + e | e - e | e * e | e / e
    | e && e | e || e
    | e < e | e == e
    | if e then e else e
    | let x = e in e

T0 ::= Bool | Int | T0 -> T0
