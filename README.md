# PhiScript — A Yet Another Language Variant of AiScript

PhiScript はスクリプト言語 [AiScript](https://github.com/aiscript-dev/aiscript) の構文・言語機能を刷新し，より開発を平易にすることを目的とする言語です．
AiScript へのトランスパイルを提供し，既存のコードベースと両立します．

## インストール

AiScript の処理系は [本家リポジトリ](https://github.com/aiscript-dev/aiscript) を `clone` して取得してください．

現在，PhiScript のトランスパイラの実行には [.NET Core 7.0](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) が必要です．
本リポジトリを `clone` したのち，`dotnet run --project ./src/Example` を実行してください．

## コード例

```js
var x = 0
var y = 1

for i in 20
    if x < 100
        <: x
    else
        <: "too big!"
    end
    let f = (x, y) -> x + y
    z := f(x, y)
    x <- y; y <- z
end

let xy = [x, y]
```
