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
local max = math.max
local min = math.min
local sort = table.sort
local sqrt = math.sqrt

-- Modules --
local qf = require("math.quadratic_formula") -- n.b. not yet used here
local segments = require("math.segments")

-- Boundary of "player" object --
local BX, BY = 100, 100
local BW, BH = 40, 30

-- Left- or right-movement speed, e.g. for walking --
local WalkSpeed = 150

-- Small clearance to avoid becoming stuck on ceilings --
local CeilingEpsilon = .05

-- Small clearance to allow gravity-ish checks on the floor --
local FloorEpsilon = .01

-- Closeness of edges that allows them to hit stairs --
local StairsProximity = BW / 2

-- Upper and lower heights of bounding box that can hit stairs --
local StairsTop, StairsBottom = BH / 2 * .25, BH / 2 * .95

-- Upper and lower heights of bounding box that can be impeded by walls --
local WallTop, WallBottom = -BH / 2, StairsTop - 3

-- Vertical impulse applied when stairs are triggered --
local StairsImpulse = -115

-- Vertical impulse applied to jump --
local JumpImpulse = -590

-- Gravity --
local G = 875

-- Damping applied to vertical speed --
local Damping = .975

-- Number of frames to forgive walking off ledges --
local WaitAmount = 3.75 / 30

-- Set up the box graphics.
local Box = display.newGroup()

do
	local border = display.newLine(Box, BX + BW / 2, BY + BH / 2, BX + BW / 2, BY - BH / 2)

	border:append(BX - BW / 2, BY - BH / 2)
	border:append(BX - BW / 2, BY + BH / 2)
	border:append(BX + BW / 2, BY + BH / 2)

	border.strokeWidth = 2

	Box.m_x, Box.m_y = BX, BY
end

-- Test level --
local Segments = {
	{ x1 = 300, y1 = 350, x2 = 500, y2 = 350 },
	{ x1 = 400, y1 = 340, x2 = 475, y2 = 340 },
	{ x1 = 400, y1 = 240, x2 = 475, y2 = 240, is_solid = true },
	{ x1 = 50, y1 = 300, x2 = 200, y2 = 400 },
	{ x1 = 150, y1 = 175, x2 = 250, y2 = 175 },
	{ x1 = 250, y1 = 25, x2 = 250, y2 = 175 },
	{ x1 = 50, y1 = 600, x2 = 200, y2 = 700 },
	{ x1 = 200, y1 = 700, x2 = 400, y2 = 500 },
	{ x1 = -50, y1 = 800, x2 = 200, y2 = 950 },
	{ x1 = 200, y1 = 950, x2 = display.contentWidth + 50, y2 = 950 },
	{ x1 = 400, y1 = 940, x2 = 450, y2 = 940 },
	{ x1 = 200, y1 = 400, x2 = 200, y2 = 500, is_solid = true },
	{ x1 = 420, y1 = 500, x2 = 490, y2 = 500, move = { dy = 1, pos1 = 0, pos2 = 300, t = 5, dir = true } },
	{ x1 = 200, y1 = 800, x2 = 350, y2 = 800, move = { dx = 1, pos1 = -100, pos2 = 200, t = 3, dir = true } },
	{ x1 = 400, y1 = 140, x2 = 475, y2 = 140, is_solid = true, move = { dx = 3, dy = 1, pos1 = -50, pos2 = 30, t = 3 } },
	{ x1 = 100, y1 = 500, x2 = 150, y2 = 500, move = { dx = 1, pos1 = -100, pos2 = 200, t = 2 } },
	{ x1 = 300, y1 = 850, x2 = 300, y2 = 950, move = { dx = 1, pos1 = -100, pos2 = display.contentWidth - 300, t = 8 }}
}

-- Nearest collision point --
local PX, PY

-- Floor segment, if available; else false --
local OnFloor = false

-- Unit direction of floor segment --
local LX, LY

-- Vertical speed --
local V = 0

-- How many frames are left to forgive walking off edge --
local WaitTime = 0

-- Collection of nearby segments --
local SegmentList = {}

-- Number of solid segments in list; total number of segments --
local NSolid, NTotal

-- Comparator to put solid segments in front of the rest in a segment list
local function SolidComp (seg1, seg2)
	return seg1.is_solid and not seg2.is_solid
end

-- Get a list of nearby segments
local function GatherSegments (dx, dy, sgroup)
	NSolid, NTotal, sgroup = 0, 0, sgroup or Segments

	-- Augment the bounding box by the maximum motion component plus some tolerance.
	local delta = max(abs(dx), abs(dy)) + .01

	dx, dy = BW / 2 + delta, BH / 2 + delta

	local xmin, ymin = BX - dx, BY - dy
	local xmax, ymax = BX + dx, BY + dy

	-- Gather any segments with components not obviously outside the bounding box, i.e. with
	-- both components on one side of an edge. Make special note of any solid ones.
	for i = 1, #sgroup do
		local seg = sgroup[i]
		local x1, y1, x2, y2 = seg.x1, seg.y1, seg.x2, seg.y2

		if not (max(x1, x2) < xmin or max(y1, y2) < ymin or min(x1, x2) > xmax or min(y1, y2) > ymax) then
			SegmentList[NTotal + 1], NTotal = seg, NTotal + 1

			if seg.is_solid then
				NSolid = NSolid + 1
			end
		end
	end

	-- Evict any lingering segments from the last frame. Move any solid segments up front.
	for i = #SegmentList, NTotal + 1, -1 do
		SegmentList[i] = nil
	end

	sort(SegmentList, SolidComp)
end

-- Helper to detect a moving point hitting a segment
local function HitSegment (index, x1, y1, x2, y2)
	local seg = SegmentList[index]

	return segments.Intersect(seg.x1, seg.y1, seg.x2, seg.y2, x1, y1, x2, y2)
end

-- Perform some common bookkeeping when a floor segment is in effect
local function UpdateFloorSegment (seg)
	if seg ~= OnFloor then
		LX, LY = seg.x2 - seg.x1, seg.y2 - seg.y1

		local slen = sqrt(LX^2 + LY^2)

		LX, LY = LX / slen, LY / slen

		if LX < 0 then
			LX, LY = -LX, -LY
		end
	end

	OnFloor, V, WaitTime = seg, 0, 0
end

-- Segments to disregard when checking for nearest hit segment --
local Ignore = {}

-- Given some moving point, find its earliest collision with one of the nearby segments 
local function CheckHits (n, x1, y1, x2, y2)
	local seg, t

	for si = 1, n do
		if not Ignore[si] then
			local ix, iy = HitSegment(si, x1, y1, x2, y2)

			if ix then
				local dsq = (ix - x1)^2 + (iy - y1)^2

				if not t or dsq < t then
					PX, PY, seg, t = ix, iy, SegmentList[si], dsq
				end
			end
		end
	end

	return seg
end

-- Apply a vertical impulse to perform a hop
local function Hop (amount)
	OnFloor, V, WaitTime = false, amount, 0
end

-- Helper to attempt a hop when stairs are hit; true = failed
local function TryToHop (x1, y1, x2, y2)
	for j = 1, NTotal do
		if HitSegment(j, x1, y1, x2, y2) then
			return true
		end
	end

	Hop(StairsImpulse)
end

-- Get the component keys for the matching endpoints of two attached segments, given a direction
local function GetVertexKeys (dx)
	if dx > 0 then
		return "x2", "y2", "x1", "y1"
	else
		return "x1", "y1", "x2", "y2"
	end
end

-- Given one endpoint, is it attached to the corresponding segment?
local function IsAttached (px, py, seg, nnx, nny)
	local nx, ny = seg[nnx], seg[nny]

	return 1 + (nx - px)^2 + (ny - py)^2 == 1
end

-- Squared steep angle cosine --
local CosAngleSq = math.cos(math.rad(75))^2

-- Avoid too-steep slopes being used as attachments
local function IsLowEnough (seg)
	local dxsq = (seg.x2 - seg.x1)^2
	local casq = dxsq / (dxsq + (seg.y2 - seg.y1)^2)

	return casq > CosAngleSq
end

-- Given a direction, is the current floor segment attached to another?
local function HasAttachment (dx)
	local ntx, nty, nnx, nny = GetVertexKeys(dx)
	local tx, ty = OnFloor[ntx], OnFloor[nty]

	for i = 1, NTotal do
		local seg = SegmentList[i]

		if seg ~= OnFloor and not Ignore[i] and IsAttached(tx, ty, seg, nnx, nny) and IsLowEnough(seg) then
			Ignore[i] = true -- will be another floor segment, rather than an obstacle

			return seg, tx, ty
		end
	end
end

-- Clear any ignored segments, such as floor attachments
local function WipeIgnores ()
	for i = 1, NTotal do
		Ignore[i] = nil
	end
end

-- Helper for horizontal hit checks
local function HorzHit (n, dx, y)
	return CheckHits(n, BX, y, BX + dx, y)
end

-- Helper for wall hit checks
local function WallHit (x, dx, dy)
	local wt, wb = BY + WallTop, BY + WallBottom

	return CheckHits(NTotal, x, wt, x + dx, wt + dy) or CheckHits(NTotal, x, wb, x + dx, wb + dy)
end

-- Update the bounding box horizontally
local function UpdateHorz (dx)
	local aseg, dx0, left, vx = OnFloor and NTotal > 1, dx, abs(dx), BW / 2

	-- Ignore the floor segment, if there is one.
	for i = 1, NTotal do
		Ignore[i] = SegmentList[i] == OnFloor
	end

	-- Loop in case we happen to be on the floor, to allow moving across attachments.
	repeat
		local along_floor, dy, tx, ty = OnFloor or WaitTime > 0, 0

		-- If a horizontal motion suddenly put us on a floor, lock the box to that position.
		if not along_floor then
			local seg = HorzHit(NTotal, dx, BY + BH / 2)
-- todo: avoid vertical "floors", i.e. 1 + (seg.x2 - seg.x1)^2 ~= 1... if we guard against that here, though, we just pass through :/
-- in theory, needs some sort of tiny platform in place
			if seg then
				BX, dx, along_floor, aseg = PX, dx - (PX - BX), true, NTotal > 1
				left = abs(dx)

				UpdateFloorSegment(seg)
			end
		end

		-- If we are or recently were on the floor, follow its direction. In the first
		-- situation, detect if we happen to be moving onto an attached segment, in which
		-- case we only move as far as necessary to get there.
		if along_floor then
			if aseg then
				aseg, tx, ty = HasAttachment(dx0)

				if aseg and left > 0 then
					local dist = sqrt((BX - tx)^2 + (BY + BH / 2 - ty)^2)

					dx, left = min(dist, left), left - dist

					if dx0 < 0 then
						dx = -dx
					end
				end
			end

			dx, dy = dx * LX, dx * LY
		end

		-- Check if we hit any walls when moving forward. If so, put the box there and quit.
		local px = BX + (dx > 0 and vx or -vx)

		if WallHit(px, dx, dy) then
			BX = PX + (dx > 0 and -BW or BW) / 2

			return WipeIgnores()

		-- Otherwise, check for stairs, unless we have an attachment to follow. If no stairs
		-- were found, advance the box.
		else
			if not aseg then
				for i = 1, NTotal do
					local seg, ok = SegmentList[i]

					-- Is the segment near enough to hit?
					if dx > 0 then
						ok = px - StairsProximity < seg.x1 and px - StairsProximity < seg.x2
					else
						ok = px + StairsProximity > seg.x1 and px + StairsProximity > seg.x2
					end

					-- If so and the right part of the box hit it, do a hop if nothing stands in the way.
					-- TODO: Is the "try" redundant in light of the wall check? (or needs to augment with hop?)
					if ok and HitSegment(i, px, BY + StairsBottom, px + dx, BY + StairsTop) then
						if not TryToHop(px, BY - BH / 2, px + dx, BY + StairsTop) then
							return WipeIgnores()
						end
					end
				end
			end
			
			BX, BY = BX + dx, BY + dy
		end

		-- If there is an attachment to follow and still some motion left, loop again.
		local done = not aseg or left <= 0

		if not done then
			UpdateFloorSegment(aseg)
		end
	until done

	return WipeIgnores()
end

-- Helper for vertical hit checks
local function VertHit (n, dy, y)
	return CheckHits(n, BX, y, BX, y + dy)
end

-- Update the bounding box vertically
local function UpdateVert (dy)
	-- Are we moving down?
	if dy > 0 then
		-- See if we hit anything.
		local by = BY + BH / 2 - FloorEpsilon
		local seg = VertHit(NTotal, dy, by)

		-- If so, put / keep the box on the floor.
		if seg then
			BY = PY - BH / 2

			UpdateFloorSegment(seg)

		-- Otherwise, if we just left the floor, cue a brief grace period.
		elseif OnFloor then
			OnFloor, WaitTime = false, WaitAmount

		-- Finally, if no such grace period is in effect, fall.
		elseif WaitTime == 0 then
			BY, OnFloor = by + FloorEpsilon + dy - BH / 2, false
		end

	-- ...or rising?
	elseif dy < 0 then
		-- See if we hit anything solid.
		local by = BY - BH / 2
		local seg = VertHit(NSolid, dy, by)

		-- If so, stop rising, moving slightly away from the ceiling to not get stuck.
		if seg then
			BY, V = PY + BH / 2 + CeilingEpsilon, 0

		-- Otherwise, continue upward.
		else
			BY = by + dy + BH / 2
		end
	end
end

-- Segments that make up the platform currently being stepped --
local Platform = {}
--local HH,HI={}
-- Update a moving platform horizontally (WIP)
-- TODO: see note on UpdatePlatformVert
local function UpdatePlatformHorz (dx)
	local dx0, vx = dx, BW / 2

	-- Ignore the floor segment, if there is one.
	for i = 1, NTotal do
		Ignore[i] = SegmentList[i] == OnFloor
	end

	-- Check if a wall hit us. If so, let it carry us along.
	local px = BX + (dx < 0 and vx or -vx)
	local wseg = WallHit(px, -dx, 0)

	if wseg then
--HH[0]=dx
		dx = dx + PX - px

		if dx0 < 0 then
			dx = dx - FloorEpsilon -- TODO: WallEpsilon
		else
			dx = dx + FloorEpsilon
		end

		--
		local n, ni = NTotal, 0
--[[
		local jjjj
HIT=true
PPX=px
DDX=px-BX
AAX=wseg.x1
HH[1],HI=abs(PX-px),1
if HH[1]>0 and HH[1]<1e-7 then
print("I!!!!",px,PX,BX)
jjjj=true
if OnFloor then
print("JJJ",OnFloor.x1,OnFloor.y1,OnFloor.x2,OnFloor.y2)
end
III=true
end
]]
		GatherSegments(dx, 0, Segments)

		for i = 1, NTotal do
			Ignore[i] = false

			for j = 1, n do
				if Platform[j] == SegmentList[i] then
					Ignore[i], ni = true, ni + 1

					break
				end
			end
		end

		local aseg, left = OnFloor and NTotal - ni > 1, abs(dx)

		-- Loop in case we happen to be on the floor, to allow moving across attachments.
		repeat
			local along_floor, dy, tx = OnFloor or WaitTime > 0, 0

			-- If a horizontal motion suddenly put us on a floor, lock the box to that position.
			if not along_floor then
				local seg = HorzHit(NTotal, dx, BY + BH / 2)

				if seg then
					BX, dx, along_floor, aseg = PX, dx - (PX - BX), true, true
					left = abs(dx)

					UpdateFloorSegment(seg)
				end
			end

			-- If we are or recently were on the floor, follow its direction. In the first
			-- situation, detect if we happen to be moving onto an attached segment, in which
			-- case we only move as far as necessary to get there.
			if along_floor then
				if aseg then
					aseg, tx = HasAttachment(dx0)

					if aseg and left > 0 then
						local dist = abs(BX - tx)

						dx, left = min(dist, left), left - dist
					else
						dx = left
					end

					if dx0 < 0 then
						dx = -dx
					end
				end

				dy = dx * (LY / LX)
			end

			-- TODO: hit a wall?? (crush, repel platform, etc.)

			-- Otherwise, check for stairs, unless we have an attachment to follow. If no stairs
			-- were found, advance the box.
			if not aseg then
				for i = 1, NTotal do
					local seg, ok = SegmentList[i]

					-- Is the segment near enough to hit?
					if dx > 0 then
						ok = px - StairsProximity < seg.x1 and px - StairsProximity < seg.x2
					else
						ok = px + StairsProximity > seg.x1 and px + StairsProximity > seg.x2
					end

					-- If so and the right part of the box hit it, do a hop if nothing stands in the way.
					-- TODO: Is the "try" redundant in light of the wall check? (or needs to augment with hop?)
					if ok and seg ~= wseg and HitSegment(i, px, BY + StairsBottom, px + dx, BY + StairsTop) then
						if not TryToHop(px, BY - BH / 2, px + dx, BY + StairsTop) then
							return WipeIgnores()
						end
					end
				end
			end

			BX, BY = BX + dx, BY + dy
--HH[HI+1],HI=dx,HI+1
			-- If there is an attachment to follow and still some motion left, loop again.
			local done = not aseg or left <= 0

			if not done then
				UpdateFloorSegment(aseg)
			end
		until done
	else
-- floor: put on floor... return remaining dx
-- step: as per floor, but snap to step height
-- no: return dx
	end

	--
	WipeIgnores()

	for i = 1, not wseg and NTotal or 0 do
		if SegmentList[i] == OnFloor then
			return dx
		end
	end
end

-- Update a moving platform vertically
-- TODO: allow handlers for objects "between a rock and a hard place", e.g. reverse direction, crush object, etc.
local function UpdatePlatformVert (dy)
	-- If the platform is rising, see if we hit anything.
	if dy < 0 then
		local seg = VertHit(NTotal, -dy, BY + BH / 2 - FloorEpsilon)

		-- If so, put / keep the object on the floor.
		if seg then
			BY = PY + dy - BH / 2

			UpdateFloorSegment(seg)
		end

	-- On the way down, check if any solid segments ran into anything.
	elseif dy > 0 then
		local seg = VertHit(NSolid, -dy, BY - BH / 2)

		-- If so, push the object down and cancel its upward movement, if any. Add some
		-- downward speed as well to avoid getting carried along with the platform.
		if seg then
			BY, V = PY + dy + BH / 2 + CeilingEpsilon, 30 / dy
		end
	end

	--
	for i = 1, NTotal do
		if SegmentList[i] == OnFloor then
			return true
		end
	end
end

-- Currently active moving platforms --
local MovingPlatforms = {}

-- Do some basic setup: add line display objects for the segments and orient the segments in a
-- nice order to make some other operations above easier.
for _, sdata in ipairs(Segments) do
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

		MovingPlatforms[#MovingPlatforms + 1] = function(dt)
			-- Step the platform along its path.
			local now = t + (dir and dt or -dt)

			if now <= 0 or now >= duration then
				now, dir = max(0, min(now, duration)), not dir
			end

			local dp = (now - t) * dp_dt
			local vx, vy = dx * dp, dy * dp

			-- Update the platform graphics.
			-- TODO: allow more segments, e.g. for rectangular or other shapes
			seg.x, seg.y = seg.x + vx, seg.y + vy

			-- Gather any segments near us.
			-- TODO: ditto
			Platform[1] = sdata

			GatherSegments(vx, vy, Platform)

			-- If we found any, handle interactions that result from their movement.
			local mx = 0
--HIT=false
			if NTotal > 0 then
				if vy ~= 0 then
					if UpdatePlatformVert(vy) and vy >= 0 then
						BY = BY + vy
					end
				end

				if 1 + vx^2 ~= 1 then 
					mx = UpdatePlatformHorz(vx) or 0
				end
			end

			-- Update the data itself, along with the time step. Report any floor motion.
			sdata.x1, sdata.y1 = sdata.x1 + vx, sdata.y1 + vy
			sdata.x2, sdata.y2 = sdata.x2 + vx, sdata.y2 + vy
--[[
if HIT then
print("???", PX,AAX)
print("!!!",BX+DDX,sdata.x1)local aa=0
for i = 1, HI do
	aa=aa+abs(HH[i])
end
print("0",HH[0],aa)
for i = 1, HI do
print("HI",i,HH[i])
end

print("")
end]]
			t = now

			return mx
		end
	end
end

-- Did we try to jump? --
local Jumped

-- Update the character per frame.
local X, Prev = 0

Runtime:addEventListener("enterFrame", function(event)
	-- Get the time elapsed since the last frame.
	Prev = Prev or event.time

	local delta = (event.time - Prev) / 1000

	Prev = event.time

	-- Update moving blocks. We might be on a moving floor, so accumulate any motion and let
	-- the normal movement logic handle anything that results. This logic presumes multiple
	-- simultaneous collisions will be rare, so imposes no specific order among the platforms
	-- and allots each one the full time slice.
	local mx = 0

	for i = 1, #MovingPlatforms do
		mx = mx + MovingPlatforms[i](delta)
	end

	-- Execute a jump, if one was attempted. This is done after moving platforms since any
	-- logic of theirs that puts us on the floor would abort the jump impulse.
	if Jumped then
		Hop(JumpImpulse)

		Jumped = false
	end

	-- Find the list of segments we might possibly hit.
	local dx, dy = mx + delta * X, V * delta

	GatherSegments(dx, dy)

	-- If we are forgiving a walk off an edge, simply decrement the counter. Otherwise, apply
	-- damping and gravity to the vertical speed.
	WaitTime = max(WaitTime - delta, 0)

	delta = delta - WaitTime
	
	if delta > 0 then
		V = V * (1 - Damping * delta)
		V = V + G * delta
	end

	-- Perform updates along each component of motion.
	UpdateVert(dy)

	if 1 + dx^2 ~= 1 then
		UpdateHorz(dx)
	end

	-- Update the box graphic.
	dx, dy = BX - Box.m_x, BY - Box.m_y

	Box.x, Box.y = Box.x + dx, Box.y + dy

	Box.m_x, Box.m_y = BX, BY
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