defmodule General do

  def list_put(list, id) do
    list |> MapSet.new() |> MapSet.put(id) |> MapSet.to_list
  end

  def list_del(list, id) do
    list |> MapSet.new() |> MapSet.delete(id) |> MapSet.to_list
  end
end