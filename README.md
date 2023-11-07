# Lambda

ラムダ式のβ簡約およびη簡約を行います。
カテゴリカルコンビネータ<sup>[1] </sup>を用いて実装されています。

## 文法

ラムダ項`<term>`は以下の文法で定義されます（空白と括弧`(`…`)`は省略しています）。

```text
<term> ::= <ident> | "\" ( <ident> | "_" )+ "." <term> | <term> <term>
```

識別子`<ident>`は小文字アルファベットから始まり、アルファベット、数字、`_`、`'`から構成されます。

## 実行

ラムダ式の評価を対話方式で行います。

```sh
$ runghc lambda.hs
> (\x y z.x z(y z))(\x y.x)(\x y.x)
\a.a
> 
```

関数に名前をつけることができます。
名前は大文字アルファベットから始まります。

```sh
$ runghc lambda.hs
> S = \x y z.x z(y z)
> K = \x y.x
> I = S K K
> I
\a.a
> 
```

`-b`オプションをつけた場合、β簡約のみを行います。

```sh
$ runghc lambda.hs
> \x.f x
f
> 

$ runghc lambda.hs -b
> \x.f x
\a.f a
> 
```

## 参考文献

\[1] Curien, P-L. (1986). Categorical Combinators.
*Information and Control, 69* (1-3). 188-254.
