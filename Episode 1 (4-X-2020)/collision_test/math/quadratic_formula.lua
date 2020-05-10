--- Formulae for solving quadratic equations.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local sqrt = math.sqrt

-- Exports --
local M = {}

--- Solver for the most general case.
-- @number a Coefficient of quadratic term, where `abs(a)` is assumed to be sufficiently non-zero.
-- @number b Coefficent of linear term.
-- @number c Constant term.
-- @treturn[1] number First solution...
-- @treturn[1] number ...and second one, &ge; the first.
-- @return[2] **false**, indicating no solution.
function M.Quadratic (a, b, c)
	local disc = b^2 - 4 * a * c

	if disc >= 0 then
		local rd, x1, x2 = sqrt(disc)

		if b >= 0 then -- see e.g. https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
			local bmd = -b - rd

			x1, x2 = bmd / (2 * a), 2 * c / bmd
		else
			local bpd = -b + rd

			x1, x2 = 2 * c / bpd, bpd / (2 * a)
		end

		if x1 < x2 then
			return x1, x2
		else
			return x2, x1
		end

		-- Alternatively, from "Geometric Tools For Computer Graphics":
		--
		-- A = b / (2 * a)
		-- B = (4 * a * c) / (b^2)
		--   = c / (a * A^2)
		-- C = -1 - sqrt(1 - B)
		-- x1, x2 = A * C, A * B / C
	else
		return false
	end
end

--- Variant of @{Quadratic} for the case where _a_ is known to be positive.
-- This removes the need to sort the results.
function M.Quadratic_PositiveA (a, b, c)
	local disc = b^2 - 4 * a * c

	if disc >= 0 then
		local rd, x1, x2 = sqrt(disc)

		if b >= 0 then
			local bmd = -b - rd

			x1, x2 = bmd / (2 * a), 2 * c / bmd
		else
			local bpd = -b + rd

			x1, x2 = 2 * c / bpd, bpd / (2 * a)
		end

		return x1, x2 -- a > 0 and rd >= 0, thus already in order
	else
		return false
	end
end

--- Variant of @{Quadratic_PositiveA} that only returns the first solution, if any.
function M.Quadratic_PositiveA_GetFirst (a, b, c)
	local disc = b^2 - 4 * a * c

	if disc >= 0 then
		local rd = sqrt(disc)

		if b >= 0 then
			return (-b - rd) / (2 * a) -- cf. Quadratic_PositiveA on order
		else
			return 2 * c / (-b + rd)
		end
	else
		return false
	end
end

-- Variant of @{Quadratic} for the common case where `b` is double some known quantity.
-- Given the case `(t*A + B)^2 = (A^2)t^2 + (2 * A * B) * t + (B^2)`, for instance, we would
-- provide `A * B` rather than `2 * A * B`. This case allows for a simpler computation.
function M.Quadratic_TwoB (a, b, c)
	local disc = b^2 - a * c

	if disc >= 0 then
		local rd, x1, x2 = sqrt(disc)

		if b >= 0 then
			local bmd = -b - rd

			x1, x2 = bmd / a, c / bmd
		else
			local bpd = -b + rd

			x1, x2 = c / bpd, bpd / a
		end

		if x1 < x2 then
			return x1, x2
		else
			return x2, x1
		end
	else
		return false
	end
end

--- Variant of @{Quadratic_TwoB} for the case where _a_ is known to be positive.
function M.Quadratic_TwoB_PositiveA (a, b, c)
	local disc = b^2 - a * c

	if disc >= 0 then
		local rd, x1, x2 = sqrt(disc)

		if b >= 0 then
			local bmd = -b - rd

			x1, x2 = bmd / a, c / bmd
		else
			local bpd = -b + rd

			x1, x2 = c / bpd, bpd / a
		end

		return x1, x2
	else
		return false
	end
end

--- Variant of @{Quadratic_TwoB_PositiveA} that only returns the first solution, if any.
function M.Quadratic_TwoB_PositiveA_GetFirst (a, b, c)
	local disc = b^2 - a * c

	if disc >= 0 then
		local rd = sqrt(disc)

		if b >= 0 then
			return (-b - rd) / a
		else
			return c / (-b + rd)
		end
	else
		return false
	end
end

-- Export the module.
return M