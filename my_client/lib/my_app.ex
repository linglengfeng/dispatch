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
    orign_str = IO.gets("\ninput:  ") # Test test0 [1,2,3]
    len = String.length(orign_str)
    final_str = String.slice(orign_str, 0..(len - 2))
    case final_str do
      "closed" <> _ -> 
        Logger.debug(fn -> "closed\n" end)
        :gen_tcp.close(socket)

      _ -> 
        :gen_tcp.send(socket, encode(final_str))
        receive do
            {:tcp, socket, packet} ->
              packet = decode(packet)
              Logger.debug(fn -> "receive packet:\n #{inspect packet}\n" end)
              loop(socket)

            {:closed, socket} ->
              Logger.debug(fn -> "closed socket:#{inspect socket}" end)
              :ok
              
            {:error, socket, reason} ->
              Logger.debug(fn -> "error socket:#{inspect socket},reason:#{inspect reason}" end)
              :ok

            other ->
              Logger.debug(fn -> "other msg:#{inspect other}" end)
              loop(socket)

        after
            3000 ->
              loop(socket)
        end
    end
  end

  def reverse_decode(packet) do
    :binary.decode_unsigned(packet, :big)
    |> :binary.encode_unsigned(:little)
    |> :erlang.binary_to_term()
  end

  def decode(packet) do
      packet
  end

  def encode(packet) do
    if is_binary(packet) do
      [packet]
    else
      [:erlang.term_to_binary(packet)]
    end
  end

end