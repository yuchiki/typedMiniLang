[![Build Status](https://travis-ci.com/yuchiki/typedMiniLang.svg?branch=master)](https://travis-ci.com/yuchiki/typedMiniLang)

# typedMiniLang

型推論器を書く練習

## 言語仕様

e0は以下は再帰を持たない式である。

    e0 ::=

    x | b | n


    | e + e | e - e | e * e | e / e


    | e && e | e || e | !e


    | e < e | e ==_int e


    | if e then e else e


    | let x = e in e


    | fun x -> e | e e

e1 はe0に以下を加えたもの

    | nil | e :: e
    | match [] -> e | x :: x -> e

多相的でない型システムT0-System

T0 ::= Bool | Int | T0 -> T0

let多相のある型システムT1-System

not-defined

## ロードマップ
| 項目 | done?
| :-   | :-
| e0が定義される | done
| e1が定義される | done
| T0が定義される | done
| T1が定義される | not yet
| (e0がparseできる) | not yet
| e0が実行できる | done
| e0がT0型検査できる | done
| e0がT1型検査できる | not yet
| e1が実行できる | done
| (e1がparseできる) | not yet
| e1がT0型検査できる | not yet
| e1がT1型検査できる | not yet
