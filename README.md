<div align="center">
<h1>TauLISP <small>by @tani</small></h1>
<a href="https://github.com/tani/tau">🌟 GitHub Star</a>
<a href="https://github.com/sponsors/tani">❤️ GitHub Sponsors</a>
</div>

## 概要

このプロジェクトは単純型付きラムダ計算の型理論に基づいた LISPの開発である．
プログラミング言語の型検査の動作について教育するために開発されている．
型検査はすべて実行前段階で行われ，型検査に成功したコードはプログラミング言語Racket にコンパイル後，実行される．
このソフトウェアを通じて型検査と論理学の関係性，言語処理系内部の動作について知っていただけたら幸いである．

## モーダスポーネンス

日常会話で「明日晴れたら海に行こう．」という文を考えてみる．この文は大きく分けて「明日晴れる」と「海に行こう」という句から構成されている．
この二つの句を繋いでいるのは「たら」という助動詞「た」の仮定形である．
このとき仮定形は仮定(前節)が成立するとき結論(後節)が成立することを意味する表現である．

ところで，この仮定形を抽象的に表現すると仮定を $A$ ，結論を $B$ ，
助動詞を $\rightarrow$ で表して $A\rightarrow B$ と書く． これを「 $A$ ならば $B$ 」と読む．
このとき， $A\rightarrow B$ に対して $A$ が成立することで $A$ が成立することを 以下のように書く．

$$\begin{prooftree}
\AXC{$A$}
\AXC{$A\rightarrow B$}
\BIC{$B$}
\end{prooftree}$$

冒頭の例に戻ると，まず「明日晴れたら海に行こう$A\rightarrow B$」という文に対して「明日晴れる $A$」が成立することで，
「海に行く $B$」が成立するので， 上記の図が書ける．

$$\begin{prooftree}
\AXC{$明日晴れる : A$}
\AXC{$明日晴れたら海に行こう : A\rightarrow B$}
\BIC{$海に行く : B$}
\end{prooftree}$$

このように， $A$ と $A\rightarrow B$ から $B$ を成立させることを論理学ではモーダスポーネンスと言う．

## プログラミング言語 tasm

LISP を例にプログラミング言語を考えると，プログラミング言語には
関数とマクロと，関数でもマクロでもない，特殊形式が存在する．
関数とはデータから別のデータを作る機能であり，マクロとは式を別の式に変換する機能である．
では，特殊形式には何があるのか．例えば，関数定義，代入，条件分岐などの制御構造が特殊形式と呼ばれる．

関数は単純にデータから別のデータを作る機能であるから型を与えるのは簡単である．
ここではマクロを扱わないので，特殊形式に型を与えることを考える．

型検査をするには，全ての特殊形式に対して正しく型を与える必要がある．
しかし，数多ある特殊形式にそれぞれ型を与えていては時間が足りない．
そこで各特殊形式を等価な別のコードに置き換えることで，最小限の特殊形式だけで型検査を実現する．
このとき，型検査を行う対象となる言語を tasm と名付ける． tasmは，
関数の他に特殊形式として無名関数と条件分岐，代入が扱えるプログラミング言語である．

tasm は四則演算やリスト操作などの基本的な関数は扱える．
これらの関数には予め型が与えられている．

```lisp
(* (+ 1 2) (/ 4 2))
(car (cons 1 2))
```

tasm は条件分岐が扱える．

```lisp
(IF (= x 1) 0 1)
```

tasm は代入が扱える．

```lisp
(SET x 10)
```

tasm は無名関数を作れる．特に関数は型宣言を持つ．
以下の例は二つの数値を受け取り，数値を返す関数である．

```lisp
(FUN (number number -> number) (x y) (+ x y))
((FUN (number number -> number) (x y) (+ x y)) 1 2)
```

形式的な定義は以下の通りである． ここで `*` は 0回以上の繰り返しを表わしている．

```
<expr> ::= <appl> | <cond> | <asgn> | <func> | <symb> | <numb>
<appl> ::= (<expr> <expr>*)
<cond> ::= (IF <expr> <expr> <expr>)
<asgn> ::= (SET <symb> <expr>)
<func> ::= (FUN <ftype> (<symb>*) <expr>*)
<symb> ::= alphebets, e.g., foo, bar, etc.
<numb> ::= numerical letters, e.g., 10, 12.02, etc.
<type> ::= <ftyp> | <atyp>
<ftyp> ::= (<type>* -> <type>)
<atyp> ::= alphabetical letters e.g., number, string, etc.
```

以上の形式に対して正しく型を与えることで，型検査を行う．

## tasm の型検査

まずは簡単な例 `(+ 1 2)` の型検査を考える． `+` の型は $number\ number \rightarrow number$
として二つの $number$ 型の値を受け取り $number$ 型の値を返すような型である．
そして，`1` と `2` はどちらも $number$ 型であるから，この関数呼び出しは正しいことが言える．
このとき組 $number\ number$ と _ならば_ の式 $number\ number \rightarrow number$ を受け取ることで，モーダスポーネンス
が成立し 結論 $number$ を得ているとみなすことができる．


$$\begin{prooftree}
\AXC{\texttt{1} : $number$}
\AXC{\texttt{2} : $number$}
\AXC{\texttt{+} : $number\ number \rightarrow number$}
\TIC{(+ 1 2) : number}
\end{prooftree}$$

では，少し複雑な例 `(+ 1 (+ 2 3))` はどうであろうか．
以下の図から正しくモーダスポーネンスが成立していることが示せる。


$$\begin{prooftree}
\AXC{1 : number}
\AXC{2 : number}
\AXC{3 : number}
\AXC{$+ : number\ number \rightarrow number$}
\TIC{(+ 2 3): number}
\AXC{$+ : number\ number \rightarrow number$}
\TIC{(+ 1 (+ 2 3)) : number}
\end{prooftree}$$

このように関数の適用だけのプログラムは簡単に型の検査が行える。
他方で代入と条件分岐は以下のように型付けられる。

$$\begin{prooftree}
\AXC{\texttt{test} : $boolean$}
\AXC{\texttt{then} : $\mathcal{T}$}
\AXC{\texttt{else} : $\mathcal{T}$}
\TIC{\texttt{(IF\ test\ then\ else)} : $\mathcal{T}$}
\end{prooftree}$$

$$\begin{prooftree}
\AXC{var : symbol}
\AXC{val : T}
\BIC{(SET var val) : T}
\end{prooftree}$$

## プログラミング言語 tau

## tau から tasm への翻訳

## tasm から Racket へ翻訳

## Racket での応用

## 支援

このプロジェクトを支援してくださる方は，是非 GitHub Sponsors
のご利用をご検討ください．

## 著作権情報

Copyright 2021--2023 TANIGUCHI Masaya. All rights reserved.

このプロジェクトは MIT ライセンスの元で公開されています．
