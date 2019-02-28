defmodule MyServerTest do
  use ExUnit.Case
  doctest MyServer

  test "greets the world" do
    assert MyServer.hello() == :world
  end
end
