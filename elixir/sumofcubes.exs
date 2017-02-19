defmodule SumOfCubes do
	# def soc(m, n, sum) do
	# 	sum = :math.pow(n, 2) * :math.pow(n+1, 2) / 4
	# 	#sum + :math.pow(n, 3)
	# 	IO.puts(":n #{n}, :sum #{sum}, :truncsum #{trunc sum},:m #{m}")
	# 	cond do
	# 		sum == m/1 -> n
	# 		sum < m -> soc(m, n+1, sum)
	# 		true -> -1
	# 	end
	# end

	def soc(m, n) do
		sum = soc(n)
		IO.puts(":m #{m} :sum #{sum} :n #{n}")
		cond do
			m*4 == sum -> n
			m*4 - sum > 0 -> soc(m, n+1)
			true -> -1
		end
	end

	def soc(n) do
		:math.pow(n, 2) * :math.pow(n+1, 2)
		#n * n * (n + 1) * (n + 1) / 4
	end

end