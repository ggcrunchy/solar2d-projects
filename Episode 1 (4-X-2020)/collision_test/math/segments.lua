--- Operations on segments.

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
local abs = math.abs
local sqrt = math.sqrt

-- Modules --
local qf = require("math.quadratic_formula")

-- Cached module references --
local _Intersect_

-- Exports --
local M = {}

--
--
--

local function Lerp (x, y, vx, vy, s)
	return x + s * vx, y + s * vy
end

--- DOCME
function M.Intersect (ax, ay, bx, by, cx, cy, dx, dy)
	local vx, vy, wx, wy = bx - ax, by - ay, dx - cx, dy - cy
	local vpx, vpy, wpx, wpy = -vy, vx, -wy, wx
	local pqx, pqy, vpw = cx - ax, cy - ay, vpx * wx + vpy * wy

	-- Common case.
	if 1 + vpw^2 ~= 1 then
		local s, t = -(wpx * pqx + wpy * pqy) / vpw, -(vpx * pqx + vpy * pqy) / vpw

		if s >= 0 and s <= 1 and t >= 0 and t <= 1 then
			return Lerp(ax, ay, vx, vy, s)
		end

	-- Segments are parallel or degenerate.
	else
		local v2, w2, px, py, qx, qy = vx^2 + vy^2, wx^2 + wy^2
		local ft = 1 + v2 ~= 1 and (pqx * vx + pqy * vy) / v2
		local gt = 1 + w2 ~= 1 and -(pqx * wx + pqy * wy) / w2

		-- Proper segments.
		if ft and gt then
			if ft >= 0 and ft <= 1 then -- one endpoint from segment #2 in #1's interval?
				qx, qy, px, py = cx, cy, Lerp(ax, ay, vx, vy, ft)
			elseif gt >= 0 and gt <= 1 then -- or one from #1 in #2's?
				qx, qy, px, py = ax, ay, Lerp(cx, cy, wx, wy, gt)
			else
				pqx, pqy = dx - ax, dy - ay -- todo: are these correct / enough?

				ft, gt = (pqx * vx + pqy * vy) / v2, -(pqx * wx + pqy * wy) / w2

				if ft >= 0 and ft <= 1 then
					qx, qy, px, py = dx, dy, Lerp(ax, ay, vx, vy, ft)
				elseif gt >= 0 and gt <= 1 then
					qx, qy, px, py = ax, ay, Lerp(dx, dy, wx, wy, gt)
				else
					-- TODO: answer to above probably NO:
						-- i.e. one segment inside the other
						-- (ft < 0 and gt > 1) or (gt < 0 and ft > 1) might be the criterion?
					return false
				end
			end

			if 1 + (px - qx)^2 + (py - qy)^2 == 1 then
				return px, py
			end

		-- Segment and point...
		elseif ft then
			if ft >= 0 and ft <= 1 then
				return Lerp(ax, ay, vx, vy, ft)
			end

		-- ...point and segment.
		elseif gt then
			if gt >= 0 and gt <= 1 then
				return Lerp(cx, cy, wx, wy, gt)
			end

		-- Points.
		elseif 1 + pqx^2 + pqy^2 == 1 then
			return ax, ay
		end
	end

	return false
end

--- DOCME
function M.IntersectCircle (px, py, qx, qy, cx, cy, radius)
	-- v = q - p
	-- [(p + t*v) - c].[(p + t*v) - c] = r^2
	-- d: p - c
	-- (d + t*v).(d + t*v) = r^2
	-- After grouping:
	-- a: v.v
	-- b: 2*v.d
	-- c: d.d - r^2
	-- t = solve binary quadratic
	local vx, vy = qx - px, qy - py
	local dx, dy = px - cx, py - cy
	local a = vx^2 + vy^2
	local b = vx * dx + vy * dy
	local c = dx^2 + dy^2 - radius^2
	local t1, t2 = qf.Quadratic_TwoB_PositiveA(a, b, c)

	if t1 and not (t1 > 1 or t2 < 0) then
		return Lerp(px, py, vx, vy, t1 >= 0 and t1 or t2)
	else
		return false
	end
end

--- DOCME
function M.IntersectCircleWithMotion (px, py, qx, qy, cx, cy, radius, vx, vy)
	-- Segment-cylinder intersection:
	-- [(c + t*v) - p].perp{q - p}/(d.d) = (+-)r
	-- w: c - p
	-- [w + t*v].N = (+-)r
	-- [w.N] + t*[v.N] = (+-)r
	-- After squaring and grouping:
	-- a: [v.N]^2
	-- b: [v.N][w.N] (TODO: this might always be negative when valid, i.e. moving "down"... not sure if edges differ)
	-- c: [w.N]^2 - r^2
	-- t = solve binary quadratic
	local dx, dy = qx - px, qy - py
	local nx, ny = -dy, dx
	local vn = vx * nx + vy * ny
	local a = vn^2

	if 1 + a ~= 1 then
		local wn = (cx - px) * nx + (cy - py) * ny
		local nn = dx^2 + dy^2
		local b = vn * wn
		local c = wn^2 - nn * radius^2  -- in fact, `c` is `wn^2 / nn - radius^2`, and likewise `a` and `b` have an
										-- implicit divide by nn; however, within the quadratic equation
										-- the majority of these cancel out, giving us this `c'`
		local t = qf.Quadratic_TwoB_PositiveA_GetFirst(a, b, c) or -1

		if 1 + t^2 == 1 then
			t = 0
		end

		if t >= 0 and t <= 1 then
			local ix, iy = cx + t * vx, cy + t * vy
			local s = (ix - px) * dx + (iy - py) * dy

			if s >= 0 and s <= nn then
				return ix, iy, Lerp(px, py, dx, dy, s / nn)
			end
		end
	end

	return false
end

--- DOCME
function M.IntersectUnitCircle (px, py, qx, qy, cx, cy)
	-- as in IntersectCircle(), but c = d.d - 1
	local vx, vy = qx - px, qy - py
	local dx, dy = px - cx, py - cy
	local a = vx^2 + vy^2
	local b = vx * dx + vy * dy
	local c = dx^2 + dy^2 - 1
	local t1, t2 = qf.Quadratic_TwoB(a, b, c)

	if t1 and not (t1 > 1 or t2 < 0) then
		return Lerp(px, py, vx, vy, t1 >= 0 and t1 or t2)
	else
		return false
	end
end

--- DOCME
function M.IntersectUnitCircleWithMotion (px, py, qx, qy, cx, cy, vx, vy)
	local dx, dy = qx - px, qy - py
	local dlen, vlen = sqrt(dx^2 + dy^2), sqrt(vx^2 + vy^2)
	local udx, udy = dx / dlen, dy / dlen
	local uvx, uvy, scale = vx / vlen, vy / vlen, vlen + 1
	local ix, iy = _Intersect_(px, py, qx, qy, cx, cy, cx + scale * uvx, cy + scale * uvy)

	if ix then
		-- reverse course until circle tangent to segment:
		-- X tan(theta) = 1
		-- X sec(theta) = hyp
		-- hyp = csc(theta) | sin(theta) = det(uv, ud)
		local det = 1 / abs(uvy * udx - uvx * udy) -- abs() to put on our side of the segment

		ix, iy = ix - det * uvx, iy - det * uvy
		dx, dy = ix - px, iy - py

		local s = dx * udx + dy * udy

		if s >= 0 and s <= dlen then
			return ix, iy, Lerp(px, py, udx, udy, s)
		end
	end

	return false
end

_Intersect_ = M.Intersect

return M