defmodule Identicon do
	require Integer
	# @white = {255, 255, 255}

	def main(input) do
		hash_input(input)
		|> pick_color	
		|> hex_layout
		|> color_layout
		|> pixel_map
		|> raw_image
		|> save(input)
	end

	def hash_input(input) do
		%Identicon.Image{
			hex: :crypto.hash(:md5, input) |> :binary.bin_to_list
		}
	end

	def pick_color(image) do
		%Identicon.Image{hex: [r, g, b | _rest]} = image
		%{image | color: {r, g, b}}
	end

	def hex_layout(%Identicon.Image{hex: hex} = image) do
		left_center = Enum.chunk(hex, 5)
		right = Enum.take(left_center, 2) |> Enum.reverse
		
		%{image | hex_layout: left_center ++ right |> cols_to_rows}
	end

	def cols_to_rows([[]|_]), do: []
	def cols_to_rows(cols) do
		Enum.map(cols, &hd/1)	++ cols_to_rows(Enum.map(cols, &(tl(&1))))
	end

	def color_layout(%Identicon.Image{hex_layout: layout} = image) do
		%{image | color_layout: Enum.map(layout, &Integer.is_odd/1)}
	end

	def grid do
		for y <- 0..4, x <- 0..4, do: {x * 50, y * 50} 
	end

	def pixel_map(%Identicon.Image{color_layout: color_layout} = image) do
		m = Enum.zip(color_layout, Identicon.grid) |> Enum.filter_map(fn {b, _} -> b end, fn {_, p} -> p end)
		|> Enum.map(fn {x, y} -> {{x, y}, {x+50, y+50}} end)

		%{image | pixel_map: m}
	end

	def raw_image(%Identicon.Image{color: color, pixel_map: pixel_map}) do
		image = :egd.create(250, 250)
		fill = :egd.color(color)
		Enum.each(pixel_map, fn {p1, p2} -> 
			:egd.filledRectangle(image, p1, p2, fill)
		end)

		:egd.render(image)
	end

	def save(image, input) do
		File.write("#{input}.png", image)
	end
end
