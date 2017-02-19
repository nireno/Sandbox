defmodule AltCase do
  def alter_char_case(c) do
  	if String.upcase(c) == c, do: String.downcase(c), else: String.upcase(c)
  end
  
  def alter_case(str) do
    str |> String.codepoints |> Enum.map(&alter_char_case(&1)) |> Enum.join
  end
end