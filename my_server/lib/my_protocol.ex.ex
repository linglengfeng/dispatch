defmodule MyProtocol do
  use GenServer
  require Logger
  @timeout :infinity
  @socket_opts [{:active, 60}]

  def start_link(ref, socket, transport, opts) do
    Logger.debug(fn -> "MyProtocol start_link:#{inspect {ref, socket, transport, opts}}\n" end)
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, socket, transport, opts}])}
  end

  def init({ref, socket, transport, _Opts}) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, @socket_opts)
    :gen_server.enter_loop(__MODULE__, [], {-1, %{}, %{socket: socket, transport: transport}}, @timeout)
  end

  def handle_call({:get_info}, _from, state) do
    {:reply, state, state}
  end

  def handle_call(request, from, state) do
    Logger.debug(fn -> "handle_call:#{inspect request}, from:#{inspect from}" end)
    {:reply, :ok , state}
  end

  def handle_cast({:notify, msg, changed}, {id, data, %{socket: socket, transport: transport} = session}) do
    transport.send(socket, encode(msg))
    new_state = {id, Map.merge(data, changed), session}
    {:noreply, new_state}
  end

  def handle_cast(msg, state) do
    Logger.debug(fn -> "handle_cast:#{inspect msg}, inspect:#{inspect state}" end)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, packet}, 
    {id, data, %{socket: socket, transport: transport} = session} = state) do
    case decode(packet, state) do
      :ok ->
        {:noreply, state}

      :err ->
        {:noreply, state}

      {:noreply, new_state} ->
        {:noreply, new_state}

      {:reply, sign, new_state} ->
        {:reply, sign, new_state}

      {:notify, client_msg, changed} ->
        transport.send(socket, encode(client_msg))
        new_state = {id, Map.merge(data, changed), session}
        {:noreply, new_state, @timeout}
      
      msg ->
        transport.send(socket, encode(msg))
        {:noreply, state}
    end
  end

  def handle_info({:tcp_passive, _socket}, {_id, _data, %{socket: socket, transport: transport}} = state) do
    transport.setopts(socket, @socket_opts)
    {:noreply, state, @timeout}
  end

  def handle_info({:tcp_closed, _socket}, {_id, _data, %{socket: socket, transport: transport}} = state) do
    Logger.debug(fn -> "tcp_closed, socket:#{inspect socket}" end)
    transport.close(socket)
    {:stop, :normal , state}
  end

  def handle_info({:tcp_error, _, reason}, {_id, _data, %{socket: socket, transport: transport}} = state) do
    Logger.debug(fn -> "tcp_error reason:#{inspect reason}, socket:#{inspect socket}" end)
    transport.close(socket)
    {:stop, reason, state}
  end

  def handle_info(:timeout, {_id, _data, %{socket: socket, transport: transport}} = state) do
    transport.close(socket)
    {:stop, :normal , state}
  end

  def handle_info(info, {_id, _data, %{socket: socket, transport: transport}} = state) do
    Logger.debug(fn -> "handle_info:#{inspect info}" end)
    transport.close(socket)
    {:stop, :normal, state}
  end

  def terminate(_Reason, _Stage) do
    :ok
  end

  def decode(packet, {id, data, _} = state) do
    with true <- is_binary(packet),
      [mod, func, args] <- :erlang.binary_to_term(packet),
      true <- is_list(args) && is_atom(func)
    do
      case handle_request(mod, func, args, state) do
        {:noreply, new_state} ->
          {:noreply, new_state}
  
        {:reply, sign , new_state} ->
          {:reply, sign , new_state}

        _ ->
          check(id) && apply_func(mod, func, args, {id, data}) || "unlogin, id:#{id}"
      end
    else
      err ->
        Logger.debug(fn -> "decode err, err:#{inspect err}, packet:#{inspect packet}\n" end)
        :err
    end
  end

  def check(id) do
    id > 0
  end

  def encode(msg) do
    msg |> :erlang.term_to_binary()
  end

  def apply_func(mod, func, args, state) do
    Code.ensure_loaded(mod)
    if function_exported?(mod, func, length(args) + 1) do
      try do
        case apply(mod, func, args ++ [state]) do
          {:notify, _client_info, changed} = msg when is_map(changed) ->
            msg

          :ok ->
            :ok

          _ ->
            :err
        end
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

  defp handle_request(:login, :player, [id], {_id, data, session} = _state) do
    Progression.register(id, self())
    {:noreply, {id, data, session}}
  end

  defp handle_request(_, _, _, _) do
    :ok
  end

end