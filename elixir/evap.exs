defmodule Evaporator do
  
  @spec evaporator(number, number, number) :: number
  
  def evaporator(content, evap_per_day, threshold) do
    r = evap_per_day / 100
    c = 1 - r
    t = threshold / 100
    days = :math.log(t) / :math.log(c) + 1
    Float.floor(days) |> round
  end
  
end