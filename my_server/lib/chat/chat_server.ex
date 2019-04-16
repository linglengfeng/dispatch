defmodule Chat.Server do
  use GenServer
  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(_) do
    {:ok, %{}}
  end

  def cast(request) do
    GenServer.cast(__MODULE__, request)
  end

  def call(request) do
    GenServer.call(__MODULE__, request)
  end

  def req_info(request, delay \\ 0) do
    Process.send_after(__MODULE__, request, delay)
  end

  # Chat.Server.call({:get_info})
  def handle_call({:get_info}, _from, state) do
    {:reply, state, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :invalid, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  def terminate(_reason, _stage) do
    :ok
  end

  def dispatch({:channel, channel, atom}, %{msg: msg} = info) when is_atom(atom) do
    Registry.dispatch(Chat.chat_name(), Chat.chat_key(), fn x ->
      for {session_pid, id} <- x do
        try do
          case atom do
            :all -> 
              Progression.online?(id) && Progression.cast(session_pid, {:notify, %{msg: msg, channel: channel}})
            :some -> 
              kick_ids = Map.get(info, :ids, [])
              Progression.online?(id) && (!(id in kick_ids)) && 
                Progression.cast(session_pid, {:notify, %{msg: msg, channel: channel}})
            _ -> :ok
          end

        rescue
          err -> Logger.debug(fn -> "chat server dispatch err:#{inspect err}" end)
        end
      end
    end)
  end

  def dispatch({:people, other_id, self_id}, %{msg: msg}) when is_integer(self_id) do
    case Registry.match(Chat.chat_name(), Chat.chat_key(), other_id) do
      [{session_pid, _} | _] ->
        try do
          Progression.online?(other_id) && Progression.cast(session_pid, {:notify, %{msg: msg, channel: other_id}})
        rescue
          err -> Logger.debug(fn -> "chat server dispatch err:#{inspect err}" end)
        end

      _ -> :ok
    end
  end

  def dispatch(_, _) do
    :ok
  end

end