defmodule Chat do
  alias Chat.Server

  @default_channel 1
  @channel_list [1, 2, 3, 4, 5]
  def join(channel, {id ,data}) do
    with true <- channel in @channel_list do
      prev_channel = Map.get(data, :channel, @default_channel)
      Server.cast({:join, channel, id, prev_channel})
      {:notify, "join channel:#{inspect channel}, id:#{id}", %{channel: channel}}
    else
      _ -> :ok
    end
  end

  def send(msg, {id, data}) do
    case Map.get(data, :channel) do
      nil -> 
        channel = @default_channel
        Server.cast({:join_send, channel, id, msg})
        {:notify, "join_send channel:#{inspect channel}, msg:#{inspect msg}, id:#{id}", %{channel: channel}}
      
      channel -> 
        Server.cast({:send, channel, id, msg})
        {:notify, "send channel:#{inspect channel}, msg:#{inspect msg}, id:#{id}", %{channel: channel}}
    end
  end

end