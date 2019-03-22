# Mix 笔记

mix deps.get timeout 可通过以下设置解决
mix hex.config http_concurrency 1
mix hex.config http_timeout 120

Mix支持"环境"的概念.它们允许开发者为特定的情景自定义编译和其它选项.Mix默认接受三种环境:

:dev--Mix任务(例如compile)按默认设置运行
:test--由mix test使用
:prod--在项目产品运行时用到的

设置环境，可在shell中用Mix.env()查看环境
$ MIX_ENV=prod mix compile
在Windows中:
$ set "MIX_ENV=prod" && mix compile
$ set "MIX_ENV=dev" && mix compile

通过环境变量来管理config
import_config "#{Mix.env}.exs"