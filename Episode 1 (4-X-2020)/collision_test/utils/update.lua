--- UPDATE OBJECT

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

-- Modules --
local segment_list = require("utils.segment_list")

-- Cached module references --
local _GetUpComponent_
local _GoingUp_

-- Exports --
local M = {}

--
--
--

local Iterations = 5

local LowSpeedSquared = 1.75

local DidAdvance = false

--- DOCME
function M.Advance (cx, cy, radius, segments, vx, vy, nsolid, ntotal, rescale)
	local n, seg = _GoingUp_(vx, vy) and nsolid or ntotal
	-- ^^^ This solid / ntotal distinction actually works rather well but snags on vertices on the way down
	-- possible workaround: visit the remaining segments, note any penetrating vertices, ignore them
	-- seems a bit hard to get right, though :/ (How do you clear it? What if they're moving?)
	-- we can get somewhat okay behavior by ignoring the depenetration logic if the segment isn't solid,
	-- but that breaks down if we approach from the side
	-- we could mention that some segments are non-solid and then detect already-penetrated segments,
	-- then just ignore vertices on these?

	DidAdvance = false

	for _ = 1, Iterations do
		if vx^2 + vy^2 > LowSpeedSquared then
			local hit = segment_list.CheckMovingCircleHits(segments, n, cx, cy, radius, vx, vy, rescale)

			if hit then
				local old_cx, old_cy, old_vx, old_vy = cx, cy, vx, vy

				cx, cy = segment_list.GetContactPosition()
				vx, vy = segment_list.GetContactVelocity()

				if 1 + (cx - old_cx)^2 + (cy - old_cy)^2 ~= 1 or 1 + (vx - old_vx)^2 + (vy - old_vy)^2 ~= 1 then
					DidAdvance, seg = true, hit
				end
			else
				DidAdvance, cx, cy = true, cx + vx, cy + vy

				break
			end
		else
			break
		end
	end

    return cx, cy, vx, vy, seg
end

--- DOCME
function M.DidAdvance ()
	return DidAdvance
end

local Rx, Ry = 1, 0

--- DOCME
function M.GetRight ()
	return Rx, Ry
end

local Ux, Uy = 0, -1

--- DOCME
function M.GetUp ()
	return Ux, Uy
end

--- DOCME
function M.GetUpComponent (vx, vy)
	return vx * Ux + vy * Uy
end

--- DOCME
function M.GoingUp (vx, vy)
	return _GetUpComponent_(vx, vy) > 0
end

--- DOCME
function M.SetIterationCount (iterations)
	local old = Iterations

	Iterations = iterations

	return old
end

--- DOCME
function M.SetLowSpeedSquared (low_speed_squared)
	local old = LowSpeedSquared

	LowSpeedSquared = low_speed_squared

	return old
end

-- Cache module members.
_GetUpComponent_ = M.GetUpComponent
_GoingUp_ = M.GoingUp

return M