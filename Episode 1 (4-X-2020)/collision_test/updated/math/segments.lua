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

-- Exports --
local M = {}

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
-- todo: one more check, but can use that ft < 0 and gt > 1, or the other way around? (possibly more urgent than thought!)
-- hmm, note to self: try to recover this!
			else
				pqx, pqy = dx - ax, dy - ay
				ft, gt = (pqx * vx + pqy * vy) / v2, -(pqx * wx + pqy * wy) / w2

				if ft >= 0 and ft <= 1 then
					qx, qy, px, py = dx, dy, Lerp(ax, ay, vx, vy, ft)
				elseif gt >= 0 and gt <= 1 then
					qx, qy, px, py = ax, ay, Lerp(dx, dy, wx, wy, gt)
				else
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
function M.IntersectPlane (ax, ay, az, bx, by, bz, px, py, pz, nx, ny, nz)
	local dx, dy, dz = bx - ax, by - ay, bz - az
	local apx, apy, apz, ndd = px - ax, py - ay, pz - az, nx * dx + ny * dy + nz * dz

	-- Common case: n.(x - p) = 0 | x = a + t(b - a).
	if 1 + ndd^2 ~= 1 then
		-- n.a + t[n.(b - a)] - n.p = 0
		-- n.(a - p) = -t[n.(b - a)]
		-- t = n.(p - a) / n.(b - a)
		local t = (nx * apx + ny * apy + nz * apz) / ndd

		if t >= 0 and t <= 1 then
			return ax + t * dx, ay + t * dy, az + t * dz
		end

	-- Segment is parallel to plane.
	elseif 1 + apx^2 + apy^2 + apz^2 == 1 then
		return px, py, pz
	end

	return false
end

-- Export the module.
return M