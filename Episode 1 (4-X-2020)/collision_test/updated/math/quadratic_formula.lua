--- Quadratic formulae.

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

-- Cached module references --
local _BQF_
local _Quadratic_

-- DOCME
function M.BQF (a, b, c) -- b = 2 * b'
	local disc = b^2 - a * c

	if 1 + disc^2 == 1 then
		return -b / a
	elseif disc < 0 then
		return false
	else
		local rd = sqrt(disc)
		local factor = -b + (b >= 0 and -rd or rd)
		local a1, a2 = factor / a, c / factor

		if a1 < a2 then
			return a1, a2
		else
			return a2, a1
		end
	end
end

--
local function Max (a1, a2)
	if a1 then
		local amax = a2 <= 1 and a2 or a1

		return amax >= 0 and amax
	end

	return false
end

--- DOCME
function M.BQF_ClampedMax (a, b, c)
	return Max(_BQF_(a, b, c))
end

--
local function Min (a1, a2)
	if a1 then
		local amin = a1 >= 0 and a1 or a2

		return amin <= 1 and amin
	end

	return false
end

--- DOCME
function M.BQF_ClampedMin (a, b, c)
	return Min(_BQF_(a, b, c))
end

--- DOCME
function M.Quadratic (a, b, c)
	local disc = b^2 - 4 * a * c

	if 1 + disc^2 == 1 then
		return -b / (2 * a)
	elseif disc < 0 then
		return false
	else
		local rd = sqrt(disc)
		local factor = -b + (b >= 0 and -rd or rd) -- see e.g. https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
		local a1, a2 = factor / (2 * a), 2 * c / factor

		if a1 < a2 then
			return a1, a2
		else
			return a2, a1
		end
	end
end

--- DOCME
function M.Quadratic_ClampedMax (a, b, c)
	return Max(_Quadratic_(a, b, c))
end

--- DOCME
function M.Quadratic_ClampedMin (a, b, c)
	return Min(_Quadratic_(a, b, c))
end

-- Cache module members.
_BQF_ = M.BQF
_Quadratic_ = M.Quadratic

-- Export the module.
return M