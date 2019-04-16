defmodule Session do
  use GenServer
  require Logger
  @timeout :infinity
  @socket_opts [{:active, 60}]

  def start_link(ref, socket, transport, opts) do
    Logger.debug(fn -> "Session start_link:#{inspect {ref, socket, transport, opts}}\n" end)
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, socket, transport, opts}])}
  end

  def init({ref, socket, transport, _Opts}) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, @socket_opts)
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport}, @timeout)
  end

  # GenServer.call(id, {:get_info})
  def handle_call({:get_info}, _from, state) do
    {:reply, state, state}
  end

  def handle_call(request, from, state) do
    Logger.debug(fn -> "handle_call:#{inspect request}, from:#{inspect from}" end)
    {:reply, :ok , state}
  end

  def handle_cast({:reply, {:data, %{id: id} = data}}, %{socket: socket, transport: transport} = state) do
    transport.send(socket, encode(data))
    state = state |> Map.put(:data, data) |> Map.merge(Chat.init(id))
    {:noreply, state}
  end

  def handle_cast({:notify, msg}, %{socket: socket, transport: transport} = state) do
    transport.send(socket, encode(msg))
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    Logger.debug(fn -> "handle_cast:#{inspect msg}, inspect:#{inspect state}" end)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, packet}, %{socket: socket, transport: transport} = state) do
    case decode(packet, state) do
      :ok ->
        {:noreply, state}

      :err ->
        {:noreply, state}

      {:noreply, new_state} ->
        {:noreply, new_state}

      {:reply, sign, new_state} ->
        transport.send(socket, encode(sign))
        {:noreply, new_state}

      {:notify, client_msg} ->
        transport.send(socket, encode(client_msg))
        {:noreply, state, @timeout}

      {:notify, client_msg, changed} when is_map(changed) ->
        transport.send(socket, encode(client_msg))
        {:noreply, state |> Map.merge(changed), @timeout}
      
      _msg ->
        # transport.send(socket, encode(msg))
        {:noreply, state}
    end
  end

  def handle_info({:tcp_passive, _socket}, %{socket: socket, transport: transport} = state) do
    transport.setopts(socket, @socket_opts)
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

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.debug(fn -> "handle_info EXIT reason:#{inspect reason}, pid:#{inspect pid}" end)
    {:stop, reason, state}
  end

  def handle_info(info, %{socket: socket, transport: transport} = state) do
    Logger.debug(fn -> "handle_info:#{inspect info}" end)
    transport.close(socket)
    {:stop, :normal, state}
  end

  def terminate(_Reason, _Stage) do
    :ok
  end

  def decode(packet, state) do
    with true <- is_binary(packet),
      [mod, func, args] when is_atom(func) <- :erlang.binary_to_term(packet),
      true <- is_list(args) && is_atom(func)
    do
      case handle_request(mod, func, args, state) do
        {:noreply, new_state} ->
          {:noreply, new_state}
  
        {:reply, sign, new_state} ->
          {:reply, sign, new_state}

        _ ->
          case Map.get(state, :id) do
            id when is_integer(id) -> 
              mod = Module.concat(:Elixir, String.capitalize("#{inspect mod}"))
              Progression.cast(id, {:dispatch, mod, func, args})
          
            _ ->
              Logger.debug(fn -> "not id, state:#{inspect state}" end)
              :ok
          end
          {:noreply, state}
      end
    else
      err ->
        Logger.debug(fn -> "decode err, err:#{inspect err}, packet:#{inspect packet}\n" end)
        :err
    end
  end

  def encode(msg) do
    msg |> :erlang.term_to_binary()
  end

  defp handle_request(:login, :player, [role_id], state) do
    # todo: Reconnect
    case Roles.Supervisor.start_child({role_id, self()}, role_opts(role_id)) do
      {:ok, role_pid} ->
        Process.flag(:trap_exit, true)
        Process.link(role_pid)
        Progression.cast(role_pid, {:login, role_id})
        {:noreply, state |> Map.merge(%{id: role_id})}

      err ->
        Logger.debug(fn -> "login player err:#{inspect err}" end)
        {:noreply, state}
    end
  end

  defp handle_request(:chat, :channel, [channel, msg], state) do
    Chat.handle_request({:channel, channel, msg}, state)
  end

  defp handle_request(:chat, :people, [other_id, msg], state) do
    Chat.handle_request({:people, other_id, msg}, state)
  end

  defp handle_request(_, _, _, state) do
    {:noreply, state}
  end

  defp role_opts(role_id) do
    [name: {:global, role_id}]# 全局名
  end

end