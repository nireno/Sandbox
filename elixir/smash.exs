defmodule Smash do
	def smash([]), do: []
	def smash([first]), do: [first]
	def smash([first | rest]), do: [(&("#{&1} ")).(first) | smash(rest)]
end