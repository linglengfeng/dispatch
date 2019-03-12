defmodule MyApp do
  use Application
  use Supervisor

  def start(_Type, _Args) do
    children = [
      worker(MyClient, [MyClient])
    ]
    Supervisor.start_link(children, [strategy: :one_for_one, name: __MODULE__])
  end
  
  def stop(_State) do
    :ok
  end

end