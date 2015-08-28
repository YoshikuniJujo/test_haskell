class Calc
	N_ROW = 5
	N_COL = 5
	N_VAR = 4

	def initialize
		@a = [
			[1.0, 1.0, 0.0, 0.0, 1.0],
			[1.0, 0.0, 1.0, 0.0, 1.0],
			[0.0, 1.0, 0.0, 1.0, 1.0],
			[0.0, 0.0, 1.0, 1.0, 1.0],
			[-9.0, -10.0, -2.0, -4.0, 0.0]
		]
	end

	def calc_linear_programming
		loop do
			min, y = select_col(9999)
			break if min >= 0
			min, x = select_row(9999, y)
			divide_pibot_var(x, y)
			sweap_out(x, y)
		end
		display
	end

	private

	def select_col(min)
		y = 0
		0.upto(N_COL - 2) do |k|
			if @a[N_ROW - 1][k] < min
				min = @a[N_ROW - 1][k]
				y = k
			end
		end
		return [min, y]
	end

	def select_row(min, y)
		x = 0
		0.upto(N_ROW - 2) do |k|
			p = @a[k][N_COL - 1] / @a[k][y].to_f
			if @a[k][y] > 0 && p < min
				min = p
				x = k
			end
		end
		return [min, x]
	end

	def divide_pibot_var(x, y)
		p = @a[x][y]
		0.upto(N_COL - 1) { |k| @a[x][k] = @a[x][k] / p.to_f }
	end

	def sweap_out(x, y)
		0.upto(N_ROW - 1) do |k|
			unless k == x
				d = @a[k][y]
				0.upto(N_COL - 1) { |j| @a[k][j] -= d * @a[x][j] }
			end
		end
	end

	def display
		0.upto(N_VAR - 1) do |k|
			flag = -1
			0.upto(N_ROW - 1) { |j| flag = j if @a[j][k] == 1 }
			printf("x%d = %8.4f\n", k, flag == -1 ? 0.0 : @a[flag][N_COL - 1])
		end
		printf(" z = %8.4f\n", @a[N_ROW - 1][N_COL - 1])
	end
end

Calc.new.calc_linear_programming
