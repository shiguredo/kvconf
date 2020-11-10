# kvconf

kvconf は Erlang アプリケーション時に Erlang Term を設定ファイルに疲れた人のための仕組みです。
設定ファイルに対するバリデーションを Erlang のレコードを利用して定義することができるため Dialyzer の恩恵を受けることができます。

## 設定ファイル例

```
etc/app.conf
```

```ini
key = value
```

## 設定定義例

```erlang
[
    #kvc{key = a, type = #kvc_atom{candidates = [x,y,z]}, default = x},
    #kvc{key = b, type = #kvc_string{}, required = true},
    #kvc{key = c, type = #kvc_interval{min = {10, s}, max = {30, s}, out_time_unit = millisecond}, deafult = {20, s}},
    #kvc{key = d, type = #kvc_integer{min = 10, max = 100}},
    #kvc{key = e, type = #kvc_boolean{}, default = true}
]
```

## ライセンス

```
Copyright 2019-2020, Shiguredo Inc.

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


