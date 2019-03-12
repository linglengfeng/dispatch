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
    case decode(packet) do
      :ok ->
        :ok

      :err ->
        :err
      
      msg ->
        transport.setopts(socket, [{:active, :once}])
        transport.send(socket, encode(msg))
    end
    {:noreply, state, @timeout}
  end

  def handle_info({:tcp_closed, _socket}, %{socket: socket, transport: transport} = state) do
    Logger.debug(fn -> "tcp_closed, socket:#{inspect socket}" end)
    transport.close(socket)
    {:stop, :normal , state}
  end

  def handle_info({:tcp_error, _, reason}, %{socket: socket, transport: transport} = state) do
    Logger.debug(fn -> "tcp_error reason:#{inspect reason}, socket:#{inspect socket}" end)
    transport.close(socket)
    {:stop, reason, state}
  end

  def handle_info(:timeout, %{socket: socket, transport: transport} = state) do
    transport.close(socket)
    {:stop, :normal , state}
  end

  def handle_info(info, %{socket: socket, transport: transport} = state) do
    Logger.debug(fn -> "handle_info:#{inspect info}" end)
    transport.close(socket)
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

  def decode(packet) do
    with true <- is_binary(packet),
      [mod, func, args] <- :erlang.binary_to_term(packet),
      true <- is_list(args) && is_atom(func)
    do
      apply_func(mod, func, args)
    else
      err ->
        Logger.debug(fn -> "decode err, err:#{inspect err}, packet:#{inspect packet}\n" end)
        :err
    end
  end

  def encode(msg) do
    msg |> :erlang.term_to_binary()
  end

  def apply_func(mod, func, args) do
    if function_exported?(mod, func, length(args)) do
      try do
        apply(mod, func, args)
      rescue
        err ->
          Logger.debug(fn -> "#{mod}.#{func} error: #{inspect(err, pretty: true)}\n}" end)
          :err
      end
    else
      Logger.debug(fn -> "func non-existent, #{mod}.#{func}/#{length(args)}\n" end)
      :err
    end
  end

end