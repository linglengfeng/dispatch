defmodule MyApp do
  use Application
  require Logger

  def start(_Type, _Args) do
    Logger.debug(fn -> "Application start\n" end)
    port = Application.get_env(:my_server, :port, 1111)
    {:ok, _} = :ranch.start_listener(:server, :ranch_tcp, [{:port, port}], MyProtocol, [])
    children = []
    Supervisor.start_link(children, [strategy: :one_for_one, name: __MODULE__])
  end

  def stop(_State) do
    :ok
  end

end