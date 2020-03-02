defmodule Mal.Atom do
  alias Mal.Function

  def new(value) do
    {:ok, pid} = Agent.start_link(fn -> value end)
    pid
  end

  def deref({:atom, pid}) do
    Agent.get(pid, fn value -> value end)
  end

  def reset!({:atom, pid}, new_value) do
    Agent.update(pid, fn _ -> new_value end)
    new_value
  end

  def swap!({:atom, pid}, %Function{value: func}, args) do
    Agent.get_and_update(pid, fn state ->
      func_args = [state | args]
      new = func.(func_args)
      {new, new}
    end)
  end
end
