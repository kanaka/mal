defmodule Mal.Env do
  def initialize(outer \\ nil) do
    Agent.start_link(fn ->
      %{outer: outer, env: %{}}
    end)
  end

  def set(pid, key, value) do
    Agent.update(pid, fn map ->
      %{map | :env => Map.put(map.env, key, value)}
    end)
  end

  def merge(pid, env_values) do
    Agent.update(pid, fn map ->
      %{map | :env => Map.merge(map.env, env_values)}
    end)
  end

  defp find_callback(map, key) do
    IO.inspect(map.env)
  end

  def find(pid, key) do
    Agent.get(pid, fn map ->
      case Map.has_key?(map.env, key) do
        true -> pid
        false -> map.outer && find(map.outer, key)
      end
    end)
  end

  def retrieve_key(pid, key) do
    Agent.get(pid, fn map ->
      case Map.fetch(map.env, key) do
        {:ok, value} -> {:ok, value}
        :error -> :not_found
      end
    end)
  end

  def get(pid, key) do
    case find(pid, key) do
      nil -> :not_found
      env -> retrieve_key(env, key)
    end
  end
end
