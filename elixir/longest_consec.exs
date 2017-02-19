defmodule LongestConsec do
	def longest_consec(words, k) do
		if k <= 0 || k > Enum.count(words) do
			""
		else
			a = Enum.take(words, k) |> Enum.join
			b = tl(words) |> longest_consec(k)
			if String.length(a) > String.length(b), do: a, else: b
		end
	end
end