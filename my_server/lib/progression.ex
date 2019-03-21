defmodule Progression do

  def call_role(id, request) do
    GenServer.call(whereis(id), request)
  end

  def cast_role(id, request) do
    GenServer.cast(whereis(id), request)
  end

  def info_role(id, request, delay \\ 0) do
    Process.send_after(whereis(id), request, delay)
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

  def onlines(module) do
    Supervisor.which_children(module)
    |> Enum.map(fn info -> pid_name(elem(info, 1)) end)
  end

  def pid_name(pid) do
    list = :global.registered_names() |> Enum.map(fn name -> {whereis(name), name} end)
    case Enum.find(list, fn {p, _name} -> p == pid end) do
      {_pid, name} -> name
      nil -> nil
    end
  end

end