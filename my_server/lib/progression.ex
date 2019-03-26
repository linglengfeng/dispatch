defmodule Progression do

  def call(pid, request) when is_pid(pid) do
    GenServer.call(pid, request)
  end

  def call(id, request) do
    case whereis(id) do
      :undefined -> :ok
      pid -> GenServer.call(pid, request)
    end
  end

  def cast(id, request) when is_pid(id) do
    GenServer.cast(id, request)
  end

  def cast(id, request) do
    case whereis(id) do
      :undefined -> :ok
      pid -> GenServer.cast(pid, request)
    end
  end

  def info(id, request, delay \\ 0)
  def info(pid, request, delay) when is_pid(pid) do
    Process.send_after(pid, request, delay)
  end

  def info(id, request, delay) do
    case whereis(id) do
      :undefined -> :ok
      pid -> Process.send_after(pid, request, delay)
    end
  end
  
  def register(name, pid) do
    :global.re_register_name(name, pid)
  end

  def whereis(name) do
    :global.whereis_name(name)
  end

  def pid_state(pid) do
    :sys.get_state(pid)
  end

  def process_info(id) do
    if is_pid(id) do
      Process.info(id)
    else
      whereis(id) |> Process.info()
    end
  end

  # Roles.Supervisor
  def onlines(module \\ Roles.Supervisor) do
    Supervisor.which_children(module)
    |> Enum.map(fn info -> pid_name(elem(info, 1)) end)
  end

  def pid_name(pid) do
    list = :global.registered_names() |> Enum.map(fn name -> {whereis(name), name} end)
    case Enum.find(list, fn {p, _name} -> p == pid end) do
      {_pid, name} -> %{id: name}
      nil -> nil
    end
  end

end