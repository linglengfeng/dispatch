defmodule Chat.Server do
  use GenServer

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

  def handle_cast({:join, channel, id, prev_channel}, state) do
    changed = 
      if prev_channel == channel do
        ids = General.list_put(Map.get(state, channel, []), id)
        %{channel => ids}
      else
        prec_c_ids = General.list_del(Map.get(state, prev_channel, []), id)
        ids = General.list_put(Map.get(state, channel, []), id)
        %{channel => ids, prev_channel => prec_c_ids}
      end

    {:noreply, state |> Map.merge(changed)}
  end

  def handle_cast({:join_send, channel, id, msg}, state) do
    ids = General.list_put(Map.get(state, channel, []), id)
    ids |> Enum.each(fn id ->
      Progression.cast_role(id, {:notify, msg, %{}})
    end)
    {:noreply, state |> Map.merge(%{channel => ids})}
  end

  def handle_cast({:send, channel, _id, msg}, state) do
    Map.get(state, channel, [])
    |> Enum.each(fn id ->
      Progression.cast_role(id, {:notify, msg, %{}})
    end)
    {:noreply, state}
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

end