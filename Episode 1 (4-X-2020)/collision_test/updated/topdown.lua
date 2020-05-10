--- Top-down experiment.

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

local CX, CY = display.contentCenterX, display.contentCenterY

local function TraceLines (x, y, verts, r, g, b)
	local xmin, ymin, xmax, ymax = 1 / 0, 1 / 0, -1 / 0, -1 / 0

	for i = 1, #verts, 2 do
		local x, y = verts[i], verts[i + 1]

		xmin, xmax = math.min(x, xmin), math.max(x, xmax)
		ymin, ymax = math.min(y, ymin), math.max(y, ymax)
	end

	local ax, ay = (xmin + xmax) / 2, (ymin + ymax) / 2
	local vout = {}

	for i = 1, #verts, 2 do
		vout[#vout + 1] = x + verts[i] - ax
		vout[#vout + 1] = y + verts[i + 1] - ay
	end

	vout[#vout + 1] = vout[1]
	vout[#vout + 1] = vout[2]

	local line = display.newLine(vout[1], vout[2], vout[3], vout[4])

	line:setStrokeColor(r, g, b)

	line.strokeWidth = 3

	for i = 5, #vout, 2 do
		line:append(vout[i], vout[i + 1])
	end

	return vout
end

local v1 = {
	.25 * CX, .25 * CY,
	.5 * CX, .25 * CY,
	.5 * CX, .5 * CY,
	.1 * CX, .4 * CY,
	.2 * CX, .3 * CY
}

local p1 = display.newPolygon(.3 * CX, .3 * CY, v1)
local pv1 = TraceLines(p1.x, p1.y, v1, 1, 0, 0)

local v2 = {
	.7 * CX, .7 * CY,
	.8 * CX, 1.1 * CY,
	.2 * CX, .8 * CY
}

local p2 = display.newPolygon(.65 * CX, .7 * CY, v2)
local pv2 = TraceLines(p2.x, p2.y, v2, 0, 1, 0)

local X, Y = 0, 0
local prev

local obj = display.newCircle(CX, CY, 15)

obj:setFillColor(1, 0, 0)

local text = display.newText("", CX, display.contentHeight - 200, native.systemFontBold, 25)
local polys = { pv1, pv2 }

local function CheckPolys (x, y)
	for _, v in ipairs(polys) do
		local hit = true

		for i = 1, #v - 2, 2 do
			local x1, y1, x2, y2 = v[i], v[i + 1], v[i + 2], v[i + 3]
			local dx, dy = x - x1, y - y1
			local wx, wy = x2 - x1, y2 - y1

			if dx * wy - dy * wx >= 0 then
				hit = false

				break
			end
		end

		if hit then
			return true
		end
	end
end

Runtime:addEventListener("enterFrame", function(event)
	prev = prev or event.time

	local delta = (event.time - prev) * (235 / 1000)

	prev = event.time

	obj.x, obj.y = obj.x + delta * X, obj.y + delta * Y

	if CheckPolys(obj.x, obj.y) then
		text.text = "INSIDE"
	else
		text.text = "OUTSIDE"
	end
end)

Runtime:addEventListener("key", function(event)
	local dx, dy = 0, 0

	if event.keyName == "up" then
		dy = -1
	elseif event.keyName == "down" then
		dy = 1
	elseif event.keyName == "left" then
		dx = -1
	elseif event.keyName == "right" then
		dx = 1
	else
		return
	end

	if event.phase == "up" then
		dx, dy = -dx, -dy
	end

	X, Y = X + dx, Y + dy
end)