defmodule Chat.Supervisor do
  use Supervisor
  require Logger
  @name Registry.Chat

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  def init(_) do
    children = [
      {Chat.Server, [name: Chat.Server]},
      supervisor(Registry, [:duplicate, @name])
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

end