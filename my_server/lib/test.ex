defmodule Test do
  
  def test0(a, b, c) do
    "here is Test test0, #{a} + #{b} + #{c} = #{a + b + c}"
  end

  def add(a, b) do
    "here is add/2, #{a} + #{b} = #{a + b}"
  end

  def add(a, b, c) do
    "here is add/3, #{a} + #{b} + #{c} = #{a + b + c}"
  end

  def map(%{a: a, b: b}) do
    "map_sum:#{a + b}"
  end
end