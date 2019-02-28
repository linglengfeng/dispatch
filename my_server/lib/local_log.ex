defmodule LocalLog do

  def log(msg_type, data) do
    {{year, mon, day}, {hour, min, sec}} = Timex.local() |> Timex.Protocol.to_erl()
    File.write("log.txt", "#{year}-#{mon}-#{day}-#{hour}-#{min}-#{sec}  
      #{inspect msg_type}\n #{inspect data}\n\n", [:append])
  end

end