defmodule MyProtocol do
  use GenServer
  require Logger
  @timeout :infinity

  def start_link(ref, socket, transport, opts) do
    Logger.debug(fn -> "MyProtocol start_link:#{inspect {ref, socket, transport, opts}}\n" end)
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, socket, transport, opts}])}
  end

  def init({ref, socket, transport, _Opts}) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport}, @timeout)
  end

  def handle_info({:tcp, socket, packet}, state = %{socket: socket, transport: transport}) do
    packet = decode(packet)

    transport.setopts(socket, [{:active, :once}])
    transport.send(socket, encode(packet))
    {:noreply, state, @timeout}
  end

  def handle_info({:tcp_closed, _Socket}, state) do
    {:stop, :normal , state}
  end

  def handle_info({:tcp_error, _, reason}, %{socket: socket, transport: transport} = state) do
    Logger.debug(fn -> "tcp_error reason:#{inspect reason}, socket:#{inspect socket}" end)
    transport.close(socket)
    {:stop, reason, state}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal , state}
  end

  def handle_info(info, state) do
    Logger.debug(fn -> "handle_info:#{inspect info}" end)
    {:stop, :normal, state}
  end

  def handle_call(request, from, state) do
    Logger.debug(fn -> "handle_call:#{inspect request}, from:#{inspect from}" end)
    {:reply, :ok , state}
  end

  def handle_cast(msg, state) do
    Logger.debug(fn -> "handle_cast:#{inspect msg}, inspect:#{inspect state}" end)
    {:noreply, state}
  end

  def terminate(_Reason, _Stage) do
    :ok
  end

  def reverse_encode(packet) do
    if is_binary(packet) do
      [
        packet
        |> :binary.part({0, byte_size(packet)}) 
        |> :binary.bin_to_list() 
        |> :lists.reverse()
        |> :erlang.list_to_binary()
      ]
    else
      [packet]
    end
  end

  def decode(packet) do
    if is_bitstring(packet) do
      case String.split(packet) do
        [mod, func, args] ->
          mod = String.capitalize(mod)
          mod = Module.concat(Elixir, mod)
          func = func |> String.downcase() |> String.to_atom()
          args = (args <> ".") |> String.to_charlist()
          with {:ok, list, _} <- :erl_scan.string(args),
            {:ok, l2} <- :erl_parse.parse_exprs(list),
            {:value, args, _} <- :erl_eval.exprs(l2, [])
          do
            if function_exported?(mod, func, length(args)) do
              try do
                apply(mod, func, args)
              rescue
                err ->
                  Logger.debug(fn -> "#{mod}.#{func} error: #{inspect(err, pretty: true)}\n}" end)
              end
            else
              Logger.debug(fn -> "func non-existent, #{mod}.#{func}/#{length(args)}\n" end)
              packet
            end
          else
            _ ->
              Logger.debug(fn -> "analysis failed, packet:#{inspect packet}\n" end)
              packet
          end

        _ ->
          Logger.debug(fn -> "format err, packet:#{inspect packet}\n" end)
          packet
      end
    else
      packet
    end  
  end

  def encode(packet) do
    [packet]
  end

end