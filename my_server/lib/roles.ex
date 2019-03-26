defmodule Roles.Supervisor do
  use Supervisor

  # def start_avatar(avatar_id, session) do
  #   Avatar.Supervisor.start_child {avatar_id, session},
  #     [name: {:global, {:name, Guid.name(avatar_id)}},
  #      spawn_opt: [min_heap_size: 32 * 1024]]
  # end

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  def start_child(args, opts) do
    Supervisor.start_child(__MODULE__, [args, opts])
  end

  def init(_) do
    children = [
      worker(Role, [], restart: :temporary)
    ]
    # children:[{Role, {Role, :start_link, []}, :temporary, 5000, :worker, [Role]}]
    Supervisor.init(children, strategy: :simple_one_for_one)
  end

end

defmodule Role do
  use GenServer
  require Logger

  def start_link(args, opts) do
    GenServer.start_link(__MODULE__, args, opts)
  end

  def init({id, session}) do
    data = %{} # todo: load data from db
    Process.flag(:trap_exit, true)
    # 已经在start_child role_opts中选了,不必再注册
    # Progression.register(id, self())
    data = Map.put(data, :id, id)
    {:ok, {id, session, data}}
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

  def handle_call({:get_info}, _from, state) do
    {:reply, state, state}
  end

  def handle_call(_request, _from, state) do
    {:reply, :invalid, state}
  end

  def handle_cast({:dispatch, mod, func, args}, {id, _session, data} = state) do
    dispatch(mod, func, args, {id, data})
    |> handle_result(state)
  end

  def handle_cast({:notify, role_id}, state) do
    Logger.debug(fn -> "role login, role_id:#{role_id}" end)
    {:noreply, state}
  end

  def handle_cast({:notify, client_info, changed}, state) when is_map(changed) do
    handle_result({:notify, client_info, changed}, state)
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

  defp handle_result(:ok, state) do
    {:noreply, state}
  end

  defp handle_result(:err, state) do
    {:noreply, state}
  end

  defp handle_result({:notify, client_info, changed}, {id, session, data} = _state) do
    new_data = Map.merge(data, changed)
    Progression.cast(session, {:notify, client_info})
    {:noreply, {id, session, new_data}}
  end

  defp handle_result(_msg, state) do
    {:noreply, state}
  end

  defp dispatch(mod, func, args, state) do
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

end