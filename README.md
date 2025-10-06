# kvconf

![GitHub Actions workflow](https://github.com/shiguredo/kvconf/actions/workflows/ci.yml/badge.svg)
[![GitHub tag](https://img.shields.io/github/tag/shiguredo/kvconf.svg)](https://github.com/shiguredo/kvconf)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

- kvconf は Erlang アプリケーションで Erlang Term を設定ファイルに利用するのに疲れた人のための仕組みです
- INI 形式ベースで、文字列は `""` を囲わず、コメントは `#` を採用しています
- INI 形式のセクション機能は利用できない
- 設定ファイルに対するバリデーションを Erlang のレコードを利用して定義することができるため Dialyzer の恩恵を受けることができます
- 環境変数による設定の上書きが可能で、プレフィックスを指定することもできます

## rebar.conf

```erlang
{deps, [{kvconf, "2024.3.1"}]}.
```

## 設定ファイル例

```text
etc/app.conf
```

```ini
# コメント
key = value
```

## 設定定義例

```erlang
[
    #kvc{key = a, type = #kvc_atom{candidates = [x,y,z]}, default = x},
    #kvc{key = b, type = #kvc_string{}, required = true},
    #kvc{key = c, type = #kvc_interval{min = {10, s}, max = {30, s}, out_time_unit = millisecond}, default = {20, s}},
    #kvc{key = d, type = #kvc_integer{min = 10, max = 100}},
    #kvc{key = e, type = #kvc_boolean{}, default = true}
]
```

## 環境変数による上書き

環境変数が設定されている場合、INI ファイルの値より優先されます。

### 変換ルール

- INI キー `abc_efg` → 環境変数 `ABC_EFG` (prefix なし)
- INI キー `abc_efg` → 環境変数 `MYAPP_ABC_EFG` (prefix = `<<"myapp">>`)
- アンダースコアは維持され、全体が大文字化されます

### 使用例

```erlang
%% INI ファイル (app.conf)
%% port = 3000

%% 環境変数 PORT=8080 を設定
os:putenv("PORT", "8080"),

%% 環境変数の値 8080 が優先される
{ok, _UnknownKeys, _UndocKvList} = kvconf:initialize([
    #kvc{key = port, type = #kvc_port_number{}, required = true}
], Binary),
8080 = kvconf:get_value(port).
```

### プレフィックス付き環境変数

プレフィックスを指定したい場合は `kvconf:initialize/3` を使用します。

```erlang
%% 環境変数 MYAPP_PORT=8080 を設定
os:putenv("MYAPP_PORT", "8080"),

{ok, _UnknownKeys, _UndocKvList} = kvconf:initialize([
    #kvc{key = port, type = #kvc_port_number{}, required = true}
], Binary, #{env_prefix => <<"myapp">>}),

8080 = kvconf:get_value(port).
```

## ライセンス

```text
Copyright 2019-2025, Shiguredo Inc.
Copyright 2019-2021, Shunichi Shinohara (Original Author)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
