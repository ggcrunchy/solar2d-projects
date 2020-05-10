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
local qf = require("math.quadratic_formula")
local segments = require("math.segments")

-- --
local BX, BY = 100, 100
local BW, BH = 40, 30

-- --
local WalkSpeed = 150

-- --
local CeilingEpsilon = .05

-- --
local FloorEpsilon = .01

-- --
local StairsProximity = BW / 4

-- --
local StairsTop, StairsBottom = BH / 2 * .25, BH / 2 * .95

-- --
local WallTop, WallBottom = -BH / 2, StairsTop - 3

-- --
local StairsImpulse = -115

-- --
local JumpImpulse = -590

-- --
local G = 875

-- --
local Damping = .975

-- --
local WaitCount = 7

--
local R = 15

local Box = display.newCircle(BX, BY, R)--[[display.newGroup()

do
	local border = display.newLine(Box, BX + BW / 2, BY + BH / 2, BX + BW / 2, BY - BH / 2)

	border:append(BX - BW / 2, BY - BH / 2)
	border:append(BX - BW / 2, BY + BH / 2)
	border:append(BX + BW / 2, BY + BH / 2)

	border.strokeWidth = 2

	for i = 0, 2 do
		local gfeeler = display.newLine(Box, BX + (i - 1) * BW / 2, BY + BH / 2, BX + (i - 1) * BW / 2, BY + BH)

		gfeeler.strokeWidth = 2
	end

	Box.m_x, Box.m_y = BX, BY
end]]

Box:setFillColor(0, 0)

Box.strokeWidth = 2

Box.m_x, Box.m_y = BX, BY

-- --
local Segments = {
	{ x1 = 300, y1 = 350, x2 = 500, y2 = 350 },
	{ x1 = 400, y1 = 340, x2 = 475, y2 = 340 },
	{ x1 = 400, y1 = 240, x2 = 475, y2 = 240, is_solid = true },
	{ x1 = 50, y1 = 300, x2 = 200, y2 = 400 },
	{ x1 = 150, y1 = 175, x2 = 250, y2 = 175 },
	{ x1 = 250, y1 = 25, x2 = 250, y2 = 175 },
	{ x1 = 50, y1 = 600, x2 = 200, y2 = 700 },
	{ x1 = 200, y1 = 700, x2 = 400, y2 = 500 },
}

-- --
local PX, PY, Segs, T = {}, {}, {}, {}

-- --
local OnFloor = false

-- --
local LX, LY, Len

-- --
local V = 0

-- --
local WaitFrames = 0

-- --
local NSolid, NTotal

-- --
local SegmentList = {}

--
local function SolidComp (seg, _)
	return seg.is_solid
end

-- --
local function GatherSegments (dx, dy)
	NSolid, NTotal = 0, 0

	--
	local delta = max(abs(dx), abs(dy)) + .01

	dx, dy = BW / 2 + delta, BH / 2 + delta

	local xmin, ymin = BX - dx, BY - dy
	local xmax, ymax = BX + dx, BY + dy

	--
	for i = 1, #Segments do
		local seg = Segments[i]
		local x1, y1, x2, y2 = seg.x1, seg.y1, seg.x2, seg.y2

		if not (max(x1, x2) < xmin or max(y1, y2) < ymin or min(x1, x2) > xmax or min(y1, y2) > ymax) then
			SegmentList[NTotal + 1], NTotal = seg, NTotal + 1

			if seg.is_solid then
				NSolid = NSolid + 1
			end
		end
	end

	--
	for i = #SegmentList, NTotal + 1, -1 do
		SegmentList[i] = nil
	end

	sort(SegmentList, SolidComp)
end

--
local function HitSegment (index, x1, y1, x2, y2)
	local seg = SegmentList[index]

	return segments.Intersect(seg.x1, seg.y1, seg.x2, seg.y2, x1, y1, x2, y2)
end

--
local function UpdateFloorSegment (seg)
	seg = seg or Segs[2]

	if seg ~= OnFloor then
		LX, LY = seg.x2 - seg.x1, seg.y2 - seg.y1
		Len = sqrt(LX^2 + LY^2)
		LX, LY = LX / Len, LY / Len

		if LX < 0 then
			LX, LY = -LX, -LY
		end
	end

	OnFloor, V, WaitFrames = seg, 0, 0
end

--
local function CheckHits (vi, n, x1, y1, x2, y2)
	local t

	T[vi] = false

	for si = 1, n do
		local ix, iy = HitSegment(si, x1, y1, x2, y2)

		if ix then
			local dsq = (ix - x1)^2 + (iy - y1)^2

			if not t or dsq < t then
				PX[vi], PY[vi], Segs[vi], t = ix, iy, SegmentList[si], dsq
			end
		end
	end

	if t then
		T[vi] = t

		return true
	end
end

local function HitSegment2 (index, cx, cy, radius, vx, vy)
	local seg = SegmentList[index]

	return segments.IntersectCircleWithMotion(seg.x1, seg.y1, seg.x2, seg.y2, cx, cy, radius, vx, vy)
end

local Fx, Fy, Vx, Vy

local function CheckHits2 (n, cx, cy, vx, vy)--x1, y1, x2, y2)
	local t
local vi = 2
	T[vi] = false

	for si = 1, n do
		local VX,VY=vx,vy
		local ix, iy, fx, fy, vx, vy = HitSegment2(si, cx, cy, R, vx, vy)--HitSegment(si, x1, y1, x2, y2)

		if ix then
--[[
			print("??",vx,vy,VX,VY)
local cc=display.newCircle(cx,cy,5)
local vv=display.newCircle(cx+vx,cy+vy,5)
local ii=display.newCircle(ix,iy,5)
local ff=display.newCircle(fx,fy,5)
cc:setFillColor(0,1,0)
vv:setFillColor(1,0,0)
ii:setFillColor(1,0,1)
ff:setFillColor(1,1,0)]]
			local dsq = (ix - cx)^2 + (iy - cy)^2

			if not t or dsq < t then
local dx, dy = ix - fx, iy - fy
local len = .025 / sqrt(dx^2+dy^2)
ix, iy = ix - len * dx, iy - len * dy 
				PX[vi], PY[vi], Segs[vi], t = ix, iy, SegmentList[si], dsq
				Fx, Fy, Vx, Vy = fx, fy, vx, vy
			end
		end
	end

	if t then
		T[vi] = t

		return true
	end
end


--
local function Hop (amount)
	OnFloor, V, WaitFrames = false, amount, 0

	Segs[2] = nil
end

--
local function TryToHop (x1, y1, x2, y2)
	for j = 1, NTotal do
		if HitSegment(j, x1, y1, x2, y2) then
			return true
		end
	end

	Hop(StairsImpulse)
end

--
local function InSegment (bx, by, seg)
	local t = (bx - seg.x1) * LX + (by - seg.y1) * LY
print("T",t,Len)
	return (t >= 0 and t <= Len) and t
end

-- --
local Visited = {}

--
local function GetVertexKeys (dx)
	if dx > 0 then
		return "x2", "y2", "x1", "y1"
	else
		return "x1", "y1", "x2", "y2"
	end
end

--
local function IsAttached (px, py, seg, nnx, nny)
	local nx, ny = seg[nnx], seg[nny]

	return 1 + (nx - px)^2 + (ny - py)^2 == 1
end

--
local function AdjustSegment (dx, dy)
	local bx, by = BX, BY + BH / 2

	--
	for i = 1, NTotal do
		Visited[i] = SegmentList[i] == OnFloor
	end

	--
	local dist, seg, ntx, nty, nnx, nny = sqrt(dx^2 + dy^2), OnFloor, GetVertexKeys(dx)
	local of, wf, lx, ly, len, any = OnFloor, WaitFrames, LX, LY, Len

	for _ = 1, NTotal - 1 do
		--
		local tx, ty, nseg = seg[ntx], seg[nty]

		for i = 1, NTotal do
			if not Visited[i] then
				nseg = SegmentList[i]

				if IsAttached(tx, ty, nseg, nnx, nny) then
					Visited[i], any = true, true

					break
				else
					nseg = nil
				end
			end
		end

		--
		local delta = dist

		if nseg then
			delta = min(sqrt((bx - tx)^2 + (by - ty)^2), dist)

			UpdateFloorSegment(nseg)

			dist, seg = dist - delta, nseg

			if dx < 0 then
				delta = -delta
			end
		end
print("!!!",delta,LX,LY,OnFloor,of,any)
		bx, by = bx + delta * LX, by + delta * LY
	end
print("")
	--
	if any then
		BX, BY = bx, by - BH / 2
	else
		BX, BY = BX + dx, BY + dy
		OnFloor, WaitFrames, LX, LY, Len = of, wf, lx, ly, len
	end
end

--
local function HasAttachment (dx)
	local ntx, nty, nnx, nny = GetVertexKeys(dx)
	local tx, ty = OnFloor[ntx], OnFloor[nty]

	for i = 1, NTotal do
		local seg = SegmentList[i]

		if seg ~= OnFloor and IsAttached(tx, ty, seg, nnx, nny) then
			return true
		end
	end
end

--
local function UpdateHorz (dx)
	local vx, dy = BW / 2, 0

	--
	if OnFloor or WaitFrames > 0 then
		dx, dy = dx * LX, dx * LY

	--
	else
		local by, hit = BY + BH / 2
	
		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NTotal, px, by, px + dx, by) or hit
		end

		if hit and T[2] then
			BX = PX[2]

			return UpdateFloorSegment()
		end
	end

	--
	local px, wt, wb = BX + (dx > 0 and vx or -vx), BY + WallTop, BY + WallBottom

	if CheckHits(2, NTotal, px, wt, px + dx, wt + dy) or CheckHits(2, NTotal, px, wb, px + dx, wb + dy) then
		BX = PX[2] + (dx > 0 and -BW or BW) / 2

	--
	else
		local bx, by = BX + dx, BY + dy

		--
		if OnFloor and NTotal > 1 and HasAttachment(dx) then
			AdjustSegment(dx, dy)
		else
			for i = 1, NTotal do
				local seg, ok = SegmentList[i]

				--
				if dx > 0 then
					ok = px - BW / 2 < seg.x1 and px - BW / 2 < seg.x2
				else
					ok = px + BW / 2 > seg.x1 and px + BW / 2 > seg.x2
				end

				--
				if ok and HitSegment(i, px, BY + StairsBottom, px + dx, BY + StairsTop) then
					if not TryToHop(px, BY - BH / 2, px + dx, BY + StairsTop) then
						return
					end
				end
			end

			BX, BY = bx, by
		end
	end
end

local function UpdateHorz2 (dx)
--	local vx, dy = BW / 2, 0
local dy=0
for i = 1, 5 do
	if CheckHits2(NTotal, BX, BY, dx, dy) then
		BX, BY = PX[2], PY[2]
		dx,dy = Vx,Vy
		if dy > 0 then
			OnFloor = true
			V=0
		end
		if 1 + (dx^2+dy^2) ~= 1 then
			break
		end
	else
		BX,BY = BX + dx,BY+dy
		break
	end
end
--[[
	--
	if OnFloor or WaitFrames > 0 then
		dx, dy = dx * LX, dx * LY

	--
	else
		local by, hit = BY + BH / 2
	
		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NTotal, px, by, px + dx, by) or hit
		end

		if hit and T[2] then
			BX = PX[2]

			return UpdateFloorSegment()
		end
	end

	--
	local px, wt, wb = BX + (dx > 0 and vx or -vx), BY + WallTop, BY + WallBottom

	if CheckHits(2, NTotal, px, wt, px + dx, wt + dy) or CheckHits(2, NTotal, px, wb, px + dx, wb + dy) then
		BX = PX[2] + (dx > 0 and -BW or BW) / 2

	--
	else
		local bx, by = BX + dx, BY + dy

		--
		if OnFloor and NTotal > 1 and HasAttachment(dx) then
			AdjustSegment(dx, dy)
		else
			for i = 1, NTotal do
				local seg, ok = SegmentList[i]

				--
				if dx > 0 then
					ok = px - BW / 2 < seg.x1 and px - BW / 2 < seg.x2
				else
					ok = px + BW / 2 > seg.x1 and px + BW / 2 > seg.x2
				end

				--
				if ok and HitSegment(i, px, BY + StairsBottom, px + dx, BY + StairsTop) then
					if not TryToHop(px, BY - BH / 2, px + dx, BY + StairsTop) then
						return
					end
				end
			end

			BX, BY = bx, by
		end
	end
]]
end

--
local function UpdateVert (dy)
	--
	local vx, hit = BW / 2

	if dy > 0 then
		local by = BY + BH / 2 - FloorEpsilon

		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NTotal, px, by, px, by + dy) or hit
		end

		--
		if hit and T[2] then
			BY = PY[2] - BH / 2

			UpdateFloorSegment()
		elseif OnFloor then
			OnFloor, WaitFrames = false, WaitCount
		elseif WaitFrames == 0 then
			BY, OnFloor = by + FloorEpsilon + dy - BH / 2, false
		end

	--
	elseif dy < 0 then
		local by = BY - BH / 2

		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NSolid, px, by, px, by + dy) or hit
		end

		--
		if hit and T[2] then
			BY, V = PY[2] + BH / 2 + CeilingEpsilon, 0
		else
			BY = by + dy + BH / 2
		end
	end
end

local function UpdateVert2 (dy)
local dx=0
for i = 1, 5 do
	if CheckHits2(NTotal, BX, BY, dx, dy) then
		BX, BY = PX[2], PY[2]
		dx,dy = Vx,Vy
		if dy > 0 then
			OnFloor = true
			V=0
		end
		if 1 + (dx^2+dy^2) ~= 1 then
			break
		end
	else
		BX,BY = BX + dx,BY+dy
		break
	end
end
--[[
	--
	local vx, hit = BW / 2

	if dy > 0 then
		local by = BY + BH / 2 - FloorEpsilon

		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NTotal, px, by, px, by + dy) or hit
		end

		--
		if hit and T[2] then
			BY = PY[2] - BH / 2

			UpdateFloorSegment()
		elseif OnFloor then
			OnFloor, WaitFrames = false, WaitCount
		elseif WaitFrames == 0 then
			BY, OnFloor = by + FloorEpsilon + dy - BH / 2, false
		end

	--
	elseif dy < 0 then
		local by = BY - BH / 2

		for i = 0, 2 do
			local px = BX + (i - 1) * vx

			hit = CheckHits(i + 1, NSolid, px, by, px, by + dy) or hit
		end

		--
		if hit and T[2] then
			BY, V = PY[2] + BH / 2 + CeilingEpsilon, 0
		else
			BY = by + dy + BH / 2
		end
	end
]]
end

--
for _, sdata in ipairs(Segments) do
	local seg = display.newLine(sdata.x1, sdata.y1, sdata.x2, sdata.y2)

	seg:setStrokeColor(math.random(), math.random(), math.random())

	seg.strokeWidth = 3

	if sdata.x2 < sdata.x1 then
		sdata.x1, sdata.x2 = sdata.x2, sdata.x1
		sdata.y1, sdata.y2 = sdata.y2, sdata.y1
	end
end

--
local X, Y = 0, 0

local prev

Runtime:addEventListener("enterFrame", function(event)
	--
	prev = prev or event.time

	local delta = (event.time - prev) / 1000

	prev = event.time

	--
	local dx, dy = delta * X, V * delta

	GatherSegments(dx, dy)

	--
	if WaitFrames == 0 then
		V = V * (1 - Damping * delta)
		V = V + G * delta
	else
		WaitFrames = WaitFrames - 1
	end

	--
	UpdateVert2(dy)

	if X ~= 0 then
		UpdateHorz2(dx)
	end

	--
--	local 
	dx, dy = BX - Box.m_x, BY - Box.m_y

	Box.x, Box.y = Box.x + dx, Box.y + dy

	Box.m_x, Box.m_y = BX, BY
end)

--
Runtime:addEventListener("key", function(event)
	local dx = 0

	if event.keyName == "up" then
		if (OnFloor or WaitFrames > 0) and event.phase == "down" then
			Hop(JumpImpulse)
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