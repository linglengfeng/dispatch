defmodule Test do
  
  def positive_add(a, b, {id, _data}) do
    with true <- a > 0 && b > 0 do
      msg =  "id:#{id}, here is add/2, #{a} + #{b} = #{a + b}"
      {:notify, msg, %{}}
    else
      _ ->
        :ok
    end
  end

end