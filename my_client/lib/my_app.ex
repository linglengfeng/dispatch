defmodule MyApp do
  use Application
  require Logger

  def start(_Type, _Args) do
    ip = Application.get_env(:my_client, :ip, {0, 0, 0, 0})
    port = Application.get_env(:my_client, :port, 1111)
    {:ok, connect_socket} = :gen_tcp.connect(ip, port,[:binary, {:packet, 0}])
    Logger.debug(fn -> "connect_socket:#{inspect connect_socket}" end)
    loop(connect_socket)
  end
  
  def stop(_State) do
    :ok
  end

  def loop(socket) do
    orign_str = IO.gets("------input------\n")
    len = String.length(orign_str)
    final_str = String.slice(orign_str, 0..(len - 2))
    case final_str do
        "closed" <> _ -> 
          Logger.debug(fn -> "closed\n" end)
          :gen_tcp.close(socket)

        _ -> 
            :gen_tcp.send(socket, :erlang.term_to_binary(final_str))
            receive do
                {_tcp, socket, data} ->
                  data = :binary.decode_unsigned(data, :big)
                    |> :binary.encode_unsigned(:little)
                    |> :erlang.binary_to_term()

                  Logger.debug(fn -> "receive data:\n #{inspect data}\n" end)
                  loop(socket)

                err ->
                  Logger.debug(fn -> "receive err:\n #{inspect err}\n" end)
            end
    end
  end

end