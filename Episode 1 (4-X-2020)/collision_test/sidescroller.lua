--- Side-scroller tests, preparatory to a more ambitious top-down generalization.

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
local assert = assert
local max = math.max
local min = math.min
local sqrt = math.sqrt

-- Modules --
local segment_list = require("utils.segment_list")
local update = require("utils.update")

-- Boundary of "player" object --
local BX, BY = 100, 100

-- Left- or right-movement speed, e.g. for walking --
local WalkSpeed = 200

-- Vertical impulse applied to jump --
local JumpImpulse = -590

-- Gravity --
local G = 875

-- Damping applied to vertical speed --
local Damping = .975

-- Set up the box graphics.
local R = 15

local Box = display.newCircle(BX, BY, R)

Box:setFillColor(0, 0)

Box.strokeWidth = 2

Box.m_x, Box.m_y = BX, BY

-- Test level --
local Segments = {
	{ x1 = 300, x2 = 500, y = 350 },
	{ x1 = 400, x2 = 475, y = 340 },
	{ x1 = 400, x2 = 475, y = 240, is_solid = true },
	{ x1 = 50, y1 = 300, x2 = 200, y2 = 400 },
	{ x1 = 150, x2 = 250, y = 175 },
	{ x = 250, y1 = 25, y2 = 175 },
	{ x1 = 50, y1 = 600, x2 = 200, y2 = 700 },
	{ x1 = 200, y1 = 700, x2 = 400, y2 = 500 },
	{ x1 = -50, y1 = 800, x2 = 200, y2 = 950 },
	{ x1 = 200, x2 = display.contentWidth + 50, y = 950, type = --"conveyer", momentum = -250 },
														 "ice", damping = .675 },
	{ x1 = 400, x2 = 450, y = 940 },
	{ x = 200, y1 = 400, y2 = 500, is_solid = true },
	{ x1 = 420, x2 = 490, y = 500, move = { dy = 1, pos1 = 0, pos2 = 300, t = 5, dir = true } },
	{ x1 = 200, x2 = 350, y = 800, move = { dx = 1, pos1 = -100, pos2 = 200, t = 3, dir = true } },
	{ x1 = 400, x2 = 475, y = 140, is_solid = true, move = { dx = 3, dy = 1, pos1 = -50, pos2 = 30, t = 3 } },
	{ x1 = 100, x2 = 150, y = 500, move = { dx = 1, pos1 = -100, pos2 = 200, t = 2 } },
	{ x = 300, y1 = 850, y2 = 950, move = { dx = 1, pos1 = -100, pos2 = display.contentWidth - 300, t = 8 }}
}

-- Floor segment, if available; else false --
local OnFloor = false

-- Vertical speed --
local V = 0

-- How many frames are left to forgive walking off edge --
local WaitTime = 0

-- Collection of nearby segments --
local SegmentList = {}

-- Apply a vertical impulse to perform a hop
local function Hop (amount)
	OnFloor, V, WaitTime = false, amount, 0
end

-- Segments that make up the platform currently being stepped --
local Platform = {}

local function AuxHorz (x, y, radius, segments, dx, nsolid, ntotal, fseg, push)
	local old_rx, old_ry = update.GetRight()
	local rx, ry, check_hit = old_rx, old_ry

	if push and ntotal == 1 then
		fseg = segments[1]
	end

	if fseg then -- follow floor direction
		local edx, edy = fseg.x2 - fseg.x1, fseg.y2 - fseg.y1
		local re = rx * edx + ry * edy
		local epx, epy = re * edx, re * edy
		local rlen_sq = epx^2 + epy^2

		if 1 + rlen_sq ~= 1 then -- not vertical?
			local rlen = sqrt(rlen_sq)

			rx, ry, check_hit = epx / rlen, epy / rlen, true

			if push then
				dx = dx / abs(rx)
			end
		end
	end

	local newx, newy, vx, vy, hit = update.Advance(x, y, radius, segments, rx * dx, ry * dx, nsolid, ntotal, push)

	if hit and check_hit and hit ~= fseg then 	-- we ran into something, so try the floor-unaware version;
												-- the "jump" that tends to happen is actually better here
												-- TODO: could check if the segments are attached, if necessary
		newx, newy, vx, vy = update.Advance(x, y, radius, segments, old_rx * dx, old_ry * dx, nsolid, ntotal, push)
	end

	return newx, newy, vx, vy
end

local function AuxVert (x, y, radius, segments, dy, nsolid, ntotal)
	local ux, uy = update.GetUp()

	return update.Advance(x, y, radius, segments, -ux * dy, -uy * dy, nsolid, ntotal)
end

-- Currently active moving platforms --
local MovingPlatforms = {}

-- --
local PlatformSegmentList = {}

-- --
local DepenetrationScale = .00325 -- TODO: .00125 seemed to be enough for push, but needs to be at least nearly this high for slopes...
-- ^^^^ maybe we could just back up a full radius plus some epsilon, augmenting the velocity likewise?

-- --
local TooManySteps = 10 -- TODO?: ...and this to climb the slope

-- --
local TooLittleMovementSquared = .0625

local function ActuallyMoved (dx, dy)
	return dx^2 + dy^2 > TooLittleMovementSquared
end

-- Do some basic setup: add line display objects for the segments and orient the segments in a
-- nice order to make some other operations above easier.
for _, sdata in ipairs(Segments) do
	if sdata.x then
		assert(not (sdata.x1 or sdata.x2), "Inconsistent x-coordinates")

		sdata.x1, sdata.x2, sdata.x = sdata.x, sdata.x
	end

	if sdata.y then
		assert(not (sdata.y1 or sdata.y2), "Inconsistent y-coordinates")

		sdata.y1, sdata.y2, sdata.y = sdata.y, sdata.y
	end
	
	local seg = display.newLine(sdata.x1, sdata.y1, sdata.x2, sdata.y2)

	seg:setStrokeColor(math.random(), math.random(), math.random())

	seg.strokeWidth = 3

	if sdata.x2 < sdata.x1 then
		sdata.x1, sdata.x2 = sdata.x2, sdata.x1
		sdata.y1, sdata.y2 = sdata.y2, sdata.y1
	end

	-- Add some state for segments that move.
	local move = sdata.move

	if move then
		local dx, dy, p1, p2 = move.dx or 0, move.dy or 0, move.pos1 or 0, move.pos2 or 0
		local ap1, ap2, duration, dir, range = abs(p1), abs(p2), move.t, not not move.dir, sqrt(dx^2 + dy^2)
		local t, dp_dt = ap1 * duration / (ap1 + ap2), (p2 - p1) / duration

		dx, dy = dx / range, dy / range

		MovingPlatforms[#MovingPlatforms + 1] = function(bx, by, radius, fseg, dt)
			-- Step the platform along its path.
			local now = t + (dir and dt or -dt)

			if now <= 0 or now >= duration then
				now, dir = max(0, min(now, duration)), not dir  -- n.b. throwing away fragments of time, so inadequate for
																-- anything that needs accurate timing; could use mod?
																-- A challenge with doing it the right way is that the platform
																-- might turn around, complicating the possible interactions
			end

			local dp = (now - t) * dp_dt
			local vx, vy = dx * dp, dy * dp

			-- Update the platform visually.
			-- TODO: allow more segments, e.g. for compound shapes (or rather, use a better object than a line)
			seg.x, seg.y = seg.x + vx, seg.y + vy

			-- Gather any segments near our platform.
			-- TODO: ditto
			Platform[1] = sdata

			local svx, svy = DepenetrationScale * vx, DepenetrationScale * vy
			local avx, avy = vx + svx, vy + svy
			local nsolid, ntotal = segment_list.Gather(PlatformSegmentList, bx, by, radius, avx, avy, Platform)

			-- Handle any interactions that result from their movement.
			local mx, my, n = 0, 0, update.GoingUp(-vx, -vy) and nsolid or ntotal

			if sdata == fseg then
				mx, my = vx, vy
				-- TODO: not very stable if on descending platform
				-- ^^^ NOTE: marked improvement by doubling my... probably does need some gravity-aware shift
			end
local aa
			if n > 0 and 1 + vx^2 + vy^2 ~= 1 then
				local abx, aby = bx + svx, by + svy
				local hit = segment_list.CheckMovingCircleHits(PlatformSegmentList, n, abx, aby, radius, -avx, -avy)

				if hit then
					local px, py = segment_list.GetContactPosition()
					local pdx, pdy = vx + (px - abx), vy + (py - aby)

					if hit == fseg then
						mx, my = 0, 0
					end

					if 1 + pdx^2 ~= 1 then
						local xdest, step, moved, to_go = abx + pdx, 1, true -- TODO: assumes x is horizontal...

						repeat
							if moved then
								to_go = xdest - bx
							else -- "stuck", so see if aiming further will do any good, e.g. pushing us over a step
								to_go = 2 * to_go
aa=true
							end

							local old_bx, old_by = bx, by

							nsolid, ntotal = segment_list.Gather(SegmentList, bx, by, radius, to_go, 0, Segments)
							bx, by = AuxHorz(bx, by, radius, SegmentList, to_go, nsolid, ntotal, fseg, true)

							if 1 + (xdest - bx)^2 == 1 then -- TODO?: check whether at least past xdest along forward, to handle overshot?
								break
							else
								moved = ActuallyMoved(bx - old_bx, by - old_by, TooLittleMovementSquared) -- not just shifting direction?
							end

							-- if stuck
								-- handle obstruction:
									-- break it... (remove from list and continue)
									-- ...or take damage and add it to ignore list for a while...
									-- ...or get squished! (cancel loop)
									-- etc.
							step = step + 1
						until step == TooManySteps -- catch-all
if aa then
--	print("N",step)
end
					end

					if 1 + pdy^2 ~= 1 then
						nsolid, ntotal = segment_list.Gather(SegmentList, bx, by, radius, 0, pdy + svy, Segments)

						bx, by = AuxVert(bx, by, radius, SegmentList, pdy + svy, nsolid, ntotal, fseg)
						-- TODO: ditto from horizontal, mutatis mutandis
					end
				end
			end

			-- Update the data itself, along with the time step. Report any floor motion.
			sdata.x1, sdata.y1 = sdata.x1 + vx, sdata.y1 + vy
			sdata.x2, sdata.y2 = sdata.x2 + vx, sdata.y2 + vy

			t = now

			return bx, by, mx, my
		end
	end
end

-- Update the character per frame.
local X, Prev = 0

local TooQuickSquared = 80 -- this (and the damping) could probably be properties of different built-in ice types

local function UpdateMomentum (vx, vy, cur, seg, delta)
	if seg then
		local stype = seg.type

		if stype == "conveyer" then -- probation, but seems fine
			return seg.momentum
		elseif stype == "ice" then -- WIP!
			if 1 + delta^2 ~= 1 then
				local vlen_sq = min(vx^2 + vy^2, TooQuickSquared)
				local damped, rx, ry = seg.damping * sqrt(vlen_sq), update.GetRight()
				local dir_damped = vx * rx + vy * ry > 0 and damped or -damped -- "right" or "left"?

				return dir_damped / delta
			else
				return cur
			end
		end
	end

	return 0
end

-- Did we try to jump? --
local Jumped

local Momentum = 0

Runtime:addEventListener("enterFrame", function(event)
	-- Get the time elapsed since the last frame.
	Prev = Prev or event.time

	local delta = (event.time - Prev) / 1000

	Prev = event.time

	-- Update moving blocks. We might be on a moving floor, so accumulate any motion and let
	-- the normal movement logic handle anything that results. This logic presumes multiple
	-- simultaneous collisions will be rare, so imposes no specific order among the platforms
	-- and allots each one the full time slice.
	local old_iters, old_too_low, mx, my = update.SetIterationCount(10), update.SetLowSpeedSquared(1e-6), 0, 0

	for i = 1, #MovingPlatforms do
		local bx, by, pdx, pdy = MovingPlatforms[i](BX, BY, R, OnFloor, delta)

		BX, BY, mx, my = bx, by, mx + pdx, my + pdy
	end

	update.SetIterationCount(old_iters)
	update.SetLowSpeedSquared(old_too_low)

	-- Execute a jump, if one was attempted. This is done after moving platforms since any
	-- logic of theirs that puts us on the floor would abort the jump impulse.
	if Jumped then
		Hop(JumpImpulse)

		Jumped = false
	end

	-- Find the list of segments we might possibly hit.
	local dx, dy = mx + delta * (Momentum + X), my + V * delta
	local nsolid, ntotal = segment_list.Gather(SegmentList, BX, BY, R, dx, dy, Segments)

	-- If we are forgiving a walk off an edge, simply decrement the counter. Otherwise, apply
	-- damping and gravity to the vertical speed.
	WaitTime = max(WaitTime - delta, 0)

	delta = delta - WaitTime
	
	if delta > 0 then
		V = V * (1 - Damping * delta)
		V = V + G * delta
	end

	-- Perform updates along each component of motion.
	local x, y, hvx, hvy = BX, BY, 0, 0

	if 1 + dx^2 ~= 1 then
		x, y, hvx, hvy = AuxHorz(x, y, R, SegmentList, dx, nsolid, ntotal, OnFloor)
	end

	local bx, by, vx, vy, hit = AuxVert(x, y, R, SegmentList, dy, nsolid, ntotal)
-- TODO: if WaitTime still matters, would probably involve a check like "if OnFloor and not hit then"...
-- however, the "player" being a circle actually seems to effect the behavior well enough (at the cost of more slipping)
	if update.DidAdvance() then
		OnFloor = nil

		if hit then
			local fx, fy = segment_list.GetContactFoot()
			local px, py = segment_list.GetContactPosition()

			if update.GoingUp(px - fx, py - fy) then -- is this possibly a floor?
				-- ^^^ TODO: omit steep / vertical walls (dot product, e.g. as somewhere above)
--[=[
-- FROM EARLIER CODE:
-- Squared steep angle cosine --
local CosAngleSq = math.cos(math.rad(75))^2

-- Avoid too-steep slopes being used as attachments
local function IsLowEnough (seg)
	local dxsq = (seg.x2 - seg.x1)^2
	local casq = dxsq / (dxsq + (seg.y2 - seg.y1)^2)

	return casq > CosAngleSq
end
]=]
				OnFloor = hit
			end

			V = -update.GetUpComponent(vx, vy) / delta -- velocity component going "down"
		end
	end

	-- Usually we damp the movement completely, but some surfaces have their own effect.
	Momentum = UpdateMomentum(hvx, hvy, Momentum, OnFloor, delta)

	-- Update the box graphic.
	BX, Box.x = bx, bx
	BY, Box.y = by, by
end)

-- Install some controls.
Runtime:addEventListener("key", function(event)
	local dx = 0

	if event.keyName == "up" then
		if (OnFloor or WaitTime > 0) and event.phase == "down" then
			Jumped = true
		end
	elseif event.keyName == "left" then
		dx = -WalkSpeed
	elseif event.keyName == "right" then
		dx = WalkSpeed
	else
		return
	end

	if event.phase == "up" then
		dx = -dx
	end

	X = X + dx
end)