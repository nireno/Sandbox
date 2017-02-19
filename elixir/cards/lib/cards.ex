defmodule Cards do

	# def create_deck([], _), do: []

	# def create_deck([suit|suits], values) do
	# 	Enum.map(values, &("#{&1} of #{suit}")) ++ create_deck(suits, values)
	# end

	# def create_deck do
	# 	values = ["Ace", "Two", "Three", "Four", "Five"]
	# 	suits = ["Hearts", "Clubs", "Spades", "Diamonds"]

	# 	create_deck(suits, values)
	# end

	def create_deck do
		values = ["Ace", "Two", "Three", "Four", "Five"]
		suits = ["Hearts", "Clubs", "Spades", "Diamonds"]

		for s <- suits, v <- values do
    	"#{v} of #{s}"
		end

	end

	def shuffle(cards) do
		Enum.shuffle(cards)
	end

	def deal(cards, n) do
		Enum.split(cards, n)
	end

	def contains?(cards, card) do
		cards |> Enum.member?(card)
	end

	def save(deck, filename) do
		binary = :erlang.term_to_binary(deck)
		File.write(filename, binary)
	end

	def load(filename) do
		case File.read(filename) do
			{:ok, binary} -> :erlang.binary_to_term(binary)
			{:error, reason} -> reason
		end 
	end

	def create_hand(n) do
		create_deck |> shuffle |> deal(n)
	end
end
