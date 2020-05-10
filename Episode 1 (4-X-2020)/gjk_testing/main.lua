--- Experiments driver.

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
---[[

local gjk = require("gjk")

local moving = {
	150, 500,
	225, 300,
	400, 400,
	500, 600,
	370, 720,
	175, 600
}

local MN = #moving / 2

local static = {
	250, 100,
	400, 50,
	500, 200,
	470, 320,
	275, 200
}

local sline = display.newLine(unpack(static))

sline:append(static[1], static[2])

sline.strokeWidth = 3

sline:setStrokeColor(1, 0, 0)

local SN = #static / 2

local dline, closest, simplex, s2
local c1a, c2a, c1b, c2b
local ss = display.newText("", display.contentCenterX, 100, native.systemFontBold, 25)
local function UpdateMoving (dx, dy)
	display.remove(dline)
	display.remove(closest)
display.remove(c1a)
display.remove(c2a)
display.remove(c1b)
display.remove(c2b)
	for i = 1, #moving, 2 do
		moving[i], moving[i + 1] = moving[i] + dx, moving[i + 1] + dy
	end
	
	dline = display.newLine(unpack(moving))

	dline:append(moving[1], moving[2])

	dline.strokeWidth = 3

	local intersects = gjk.GJK(static, SN, moving, MN)

	if intersects then
		closest = nil

		dline:setStrokeColor(1, 0, 1)
		
		ss.text = ""
	else
		dline:setStrokeColor(0, 0, 1)

		display.remove(simplex)
		display.remove(s2)
local dd, ia, ja, ib, jb = gjk.DistanceAndPoints(static, SN, moving, MN)
		ss.text = dd and ("Length = %f, IA = %i, JA = %i, IB = %i, JB = %i"):format(dd, ia, ja, ib, jb) or "? :("
if dd then
c1a = display.newCircle(static[ia], static[ia + 1], 7)
		
		c1a:setFillColor(0, 1, 0)
c2a = display.newCircle(moving[ja], moving[ja + 1], 7)
		
		c2a:setFillColor(0, 0, 1)
if ib ~= ia then
c1b = display.newCircle(static[ib], static[ib + 1], 7)
		
		c1b:setFillColor(0, 1, 0)
end
if jb ~= ja then
c2b = display.newCircle(moving[jb], moving[jb + 1], 7)
		
		c2b:setFillColor(0, 0, 1)
end
end
--[[
		closest = display.newCircle(x, y, 7)
		
		closest:setFillColor(0, 1, 0)]]
	end
end

UpdateMoving(0, 0)

local start = system.getTimer()

local Keys = {}

Runtime:addEventListener("enterFrame", function(event)
    local now = event.time
    local delta = (now--[[event.time]] - start) * .0875

    start = now

    local dx, dy = 0, 0

    if Keys.left then dx = dx - 1 end
    if Keys.right then dx = dx + 1 end
    if Keys.up then dy = dy - 1 end
    if Keys.down then dy = dy + 1 end

    UpdateMoving(dx, dy)
end)

Runtime:addEventListener("key", function(event)
    Keys[event.keyName] = event.phase == "down"
end)



do return end

local function Perturbation()
	return math.random() * 1.19209290e-5 * (math.random() < .5 and 1 or -1)
end

local function Jostle (ax, ay)
	return ax + Perturbation(), ay + Perturbation()
end

-- test case from dyn4j
    
local vertices1 = {
    4, 11,
    5, 5,
    9, 9
}
    
local vertices2 = {
    4, 11,
    5, 5,
    9, 9
}

local count1 = #vertices1 / 2
local count2 = #vertices2 / 2

local a, b = {}, {}

for step = 1, 5000 do
    for i = 1, 2 * count1, 2 do
        a[i], a[i + 1] = Jostle(vertices1[i], vertices1[i + 1])
    end

    for i = 1, 2 * count2, 2 do
        b[i], b[i + 1] = Jostle(vertices2[i], vertices2[i + 1])
    end

    if not gjk.GJK(a, count1, b, count2) then
        print("Step: " .. step .. (" Found failing case:\n\t{%f, %f}, {%f, %f}, {%f, %f}\n\t{%f, %f}, {%f, %f}, {%f, %f}\n\n"):format(
            a[1], a[2], a[3], a[4], a[5], a[6],
            b[1], b[2], b[3], b[4], b[5], b[6]
        ))
    else
        print("Step: " .. step .. " Collision correctly detected")
    end
end

