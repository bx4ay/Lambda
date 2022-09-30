# Lambda

ラムダ式のβ簡約とη簡約を行います。カテゴリカルコンビネータ<sup>[1] </sup>を用いて実装されています。

## 文法

ラムダ項`<term>`は以下の文法で定義されます（空白と括弧`(`…`)`は省略してあります）。

```text
<term> ::= <ident> | "\" <ident>* "." <term> | <term> <term>
```

識別子`<ident>`やコメントのスタイルはHaskellと同様です。

## 実行

```sh
$ lambda
```

ラムダ式を対話方式で評価します。

```sh
$ lambda -b
```

ラムダ式を対話方式で評価します（β簡約のみ）。

```sh
$ lambda filename
```

ファイル内のラムダ式を評価します。

```sh
$ lambda -b filename
```

ファイル内のラムダ式を評価します（β簡約のみ）。

## 参考文献

\[1] Curien, P-L. (1986). Categorical combinators.
*Information and Control, 69* (1-3). 188-254.

