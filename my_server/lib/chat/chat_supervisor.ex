defmodule Chat.Supervisor do
  use Supervisor

  def star_link(name) do
    Supervisor.start_link(__MODULE__, :ok, name: name)
  end

  def init(_) do
    children = [
      {Chat.Server, [name: Chat.Server]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

end