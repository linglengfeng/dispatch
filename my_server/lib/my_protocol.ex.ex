defmodule MyProtocol do
  use GenServer
  require Logger
  @timeout :infinity

  def start_link(ref, socket, transport, opts) do
    Logger.debug(fn -> "#{inspect {ref, socket, transport, opts}}\n" end)
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, socket, transport, opts}])}
  end

  def init({ref, socket, transport, _Opts}) do
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport}, @timeout)
  end

  # |> String.downcase()
  def handle_info({:tcp, socket, data}, state = %{socket: socket, transport: transport}) do
    if  byte_size(data) > 1 do
      IO.inspect data
      transport.setopts(socket, [{:active, :once}])
      transport.send(socket, reverse_binary(data))
      {:noreply, state, @timeout}
    else
      IO.inspect "please input data len > 1"
      transport.setopts(socket, [{:active, :once}])
      transport.send(socket, reverse_binary(data))
      {:noreply, state, @timeout}
    end
  end

  def handle_info({:tcp_closed, _Socket}, state) do
    {:stop, :normal , state}
  end

  def handle_info({:tcp_error, _, reason}, state) do
    # transport.close(socket)
    {:stop, reason , state}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal , state}
  end

  def handle_info(_Info, state) do
    {:stop, :normal, state}
  end

  def handle_call(_Request, _From, state) do
    {:reply, :ok , state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _Stage) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def reverse_binary(b) do
    if is_binary(b) do
    [:binary.part(b, {0,byte_size(b)}) |> :binary.bin_to_list() |>:lists.reverse()
      |> :erlang.list_to_binary()]
      |> IO.inspect 
    end
  end

end