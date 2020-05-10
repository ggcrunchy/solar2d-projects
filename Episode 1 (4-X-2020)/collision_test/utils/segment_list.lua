--- Collision-related operations on lists of segments.

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
local max = math.max
local min = math.min
local sort = table.sort
local sqrt = math.sqrt

-- Modules --
local hit = require("utils.hit")
local qf = require("math.quadratic_formula")

-- Exports --
local M = {}

--
--
--

local function PutSolidSegmentsFirst (seg1, seg2)
	return seg1.is_solid and not seg2.is_solid
end

-- Gather nearby segments
function M.Gather (list, cx, cy, radius, dx, dy, source)
	local nsolid, ntotal = 0, 0

	-- Begin with a tight square around the circle. Linear motion on the circle's part will
	-- invalidate the fit, so scale the square up by the length plus a small tolerance.
	local delta = radius + max(abs(dx), abs(dy)) + .01
	local xmin, ymin = cx - delta, cy - delta
	local xmax, ymax = cx + delta, cy + delta

	-- Gather the segments not trivially outside the box, putting any solid ones up front.
	for i = 1, #source do
		local seg = source[i]
		local x1, y1, x2, y2 = seg.x1, seg.y1, seg.x2, seg.y2

		if not (max(x1, x2) < xmin or max(y1, y2) < ymin or min(x1, x2) > xmax or min(y1, y2) > ymax) then
			list[ntotal + 1], ntotal = seg, ntotal + 1

			if seg.is_solid then
				nsolid = nsolid + 1
			end
		end
	end

	for i = #list, ntotal + 1, -1 do
		list[i] = nil
	end

	sort(list, PutSolidSegmentsFirst)

    return nsolid, ntotal
end

local Px, Py, Fx, Fy, Vx, Vy

--- DOCME
function M.CheckMovingCircleHits (list, n, cx, cy, radius, vx, vy, keep_speed)
	local a, best_dsq, seg = vx^2 + vy^2, 1 / 0

	for i = 1, n do
        local cur = list[i]

		local ix, iy, fx, fy = hit.HitsMovingCircle(cur, cx, cy, radius, vx, vy)

		if ix then
			local dsq = (ix - cx)^2 + (iy - cy)^2

			if dsq < best_dsq then
				if dsq > a then
--[[
print("penetrating...",dsq-a,a,ix-cx,iy-cy) -- see below too
print("T",T1,T2)]]
				end

				Px, Py, Fx, Fy = ix, iy, fx, fy
                seg, best_dsq = cur, dsq
			end
		end
	end

	local r2 = radius^2

	for i = 1, n do
		local cur, x, y = list[i]

		for j = 1, 2 do
			if j == 1 then
				x, y = cur.x1, cur.y1
			else
				x, y = cur.x2, cur.y2
			end

			-- Moving circle-point intersection:
			-- [p - (c + t*v)].[p - (c + t*v)] = r^2
			-- d: p - c
			-- [d + t*v].[d + t*v] = r^2
			-- After grouping:
			-- a: v.v
			-- b: -2*v.d
			-- c: d.d - r^2
			local dx, dy = x - cx, y - cy
			local dlen_sq = dx^2 + dy^2
			local c = dlen_sq - r2

			if c < 0 then -- endpoint has penetrated circle?
				local scale = radius / sqrt(dlen_sq)
--print("!!!!!",best_dsq and best_dsq>a) -- leave this for now in case it's still a problem
				Px, Py, Fx, Fy = x - scale * dx, y - scale * dy, x, y
				seg, best_dsq = cur, 0
			else
				local b = -(vx * dx + vy * dy)
				local t = qf.Quadratic_TwoB_PositiveA_GetFirst(a, b, c) or -1
				local t2 = t^2

				if 1 + t2 == 1 then
					t = 0
				end

				if t >= 0 and t <= 1 then
					local dsq = t2 * a -- [I - c].[I - c] | I = p + t*v

					if dsq < best_dsq then
						Px, Py, Fx, Fy = cx + t * vx, cy + t * vy, x, y
						seg, best_dsq = cur, dsq
					end
				end
			end
		end
	end

	if seg then
		local scale = .00125 / sqrt(a)
		local dvx, dvy = scale * vx, scale * vy

		Px, Py = Px - dvx, Py - dvy
	--	Fx, Fy = Fx - dvx, Fy - dvy
		vx, vy = vx - dvx, vy - dvy

		vx, vy = cx + vx - Px, cy + vy - Py -- leftover velocity = destination - intersection

		local nx, ny = Px - Fx, Py - Fy
		local vn = (vx * nx + vy * ny) / (nx^2 + ny^2)

		Vx, Vy = vx - nx * vn, vy - ny * vn -- slide

		if keep_speed then -- keep "leftover" speed?
			local scale = sqrt((vx^2 + vy^2) / (Vx^2 + Vy^2))

			Vx, Vy = Vx * scale, Vy * scale
		end
--[[
		TODO: friction
		-- ^^ actually this probably belongs elsewhere, say in update
		local f=.9
Vx,Vy=f*Vx,f*Vy]]
	end

	return seg
end

--- DOCME
function M.GetContactFoot ()
    return Fx, Fy
end

--- DOCME
function M.GetContactPosition ()
    return Px, Py
end

--- DOCME
function M.GetContactVelocity ()
    return Vx, Vy
end

return M