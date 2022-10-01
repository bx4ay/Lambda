# Lambda

ラムダ式のβ簡約およびη簡約を行います。カテゴリカルコンビネータ<sup>[1] </sup>を用いて実装されています。

## 文法

ラムダ項`<term>`は以下の文法で定義されます（空白と括弧`(`…`)`は省略してあります）。

```text
<term> ::= <ident> | "\" ( <ident> | "_" )+ "." <term> | <term> <term>
```

識別子`<ident>`やコメントのスタイルはHaskellと同様です。

## 実行

引数を与えずに実行した場合、ラムダ式の評価を対話方式で行います。

```sh
$ lambda
> (\x y z.x z(y z))(\x y.x)(\x y.x)
\a.a
> 
```

1つ以上のファイル名を引数として与えて実行した場合、ファイル内のラムダ式を評価します。

```sh
$ cat test1.txt
(\x.x(\x y.x))(\x.x a b)
$ cat test2.txt
(\x.x(\x y.y))(\x.x a b)
$ lambda test1.txt test2.txt
a
b
```

`-b`オプションをつけた場合、β簡約のみを行います。

```sh
$ cat test3.txt
\x.a x
$ lambda test3.txt
a
$ lambda -b test3.txt
\b.a b
```

## 参考文献

\[1] Curien, P-L. (1986). Categorical Combinators.
*Information and Control, 69* (1-3). 188-254.
