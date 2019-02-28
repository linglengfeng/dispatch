# Note

mix deps.get timeout 可通过以下设置解决
mix hex.config http_concurrency 1
mix hex.config http_timeout 120

