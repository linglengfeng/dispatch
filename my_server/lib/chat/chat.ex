defmodule Chat do
  alias Chat.Server

  @channel_cd %{1 => 10, 2 => 20, 3 => 30}
  @chat_name Registry.Chat
  @chat_key "chat"
  
  def chat_name(), do: @chat_name
  def chat_key(), do: @chat_key

  def init(id) do
    Registry.register(chat_name(), chat_key(), id)
    %{channels: Map.keys(@channel_cd), chat_cd: %{}}
  end

  def handle_request({:channel, channel, msg}, %{channels: channels, id: id} = state) do
    with true <- channel in channels do
      Server.dispatch({:channel, channel, :all}, %{msg: msg})
      {:reply, {:ok, id, channel}, state}
    else
      _ -> {:noreply, state}
    end
  end

  def handle_request({:people, other_id, msg}, %{id: id} = state) do
    with true <- Progression.online?(other_id) do
      Server.dispatch({:people, other_id, id}, %{msg: msg})
      {:reply, {:ok, id, other_id}, state}
    else
      _ -> {:noreply, state}
    end
  end

  def handle_request({:some, other_ids, msg}, %{id: id} = state) do
    other_ids |> Enum.each(fn other_id ->
      Progression.online?(other_id) && Server.dispatch({:people, other_ids, id}, %{msg: msg})
    end)
    {:reply, {:ok, id, other_ids}, state}
  end

end