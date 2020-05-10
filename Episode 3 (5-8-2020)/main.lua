--- Local coordinate system sample.

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

local rect1 = display.newRect(display.contentCenterX, display.contentCenterY, 250, 200)

rect1:setFillColor(1, 0, 0)

rect1.rotation = 30

local angle = math.rad(rect1.rotation)

local function Dot (vx, vy, wx, wy)
    return vx * wx + vy * wy
end

local function Project (vx, vy, wx, wy)
    local scale = Dot(vx, vy, wx, wy) / Dot(wx, wy, wx, wy)

    return scale * wx, scale * wy
end

local right_vx, right_vy = math.cos(angle), math.sin(angle)
local up_vx, up_vy = right_vy, -right_vx

local halfw, halfh = rect1.width / 2, rect1.height / 2

local x1, y1 = rect1.x, rect1.y
local x2, y2 = x1 + halfw, y1 + halfh

local rx, ry = x1 + right_vx * halfw, y1 + right_vy * halfw
local ux, uy = x1 + up_vx * halfh, y1 + up_vy * halfh

local circ1 = display.newCircle(rx, ry, 15)

circ1:setFillColor(0, 1, 0)
circ1:setStrokeColor(0, .7, 0)

circ1.strokeWidth = 3

local circ2 = display.newCircle(ux, uy, 15)

circ2:setFillColor(0, 0, 1)
circ2:setStrokeColor(0, 0, .7)

circ2.strokeWidth = 3

local x3, y3 = x1, y1 + 350

local xaxis = display.newLine(x1, y1, rx, ry)

xaxis:setStrokeColor(1, 1, 0)

xaxis.strokeWidth = 3

local yaxis = display.newLine(x1, y1, ux, uy)

yaxis:setStrokeColor(1, 0, 1)

yaxis.strokeWidth = 3

local X = 100

local basex, basey = ux + right_vx * X, uy + right_vy * X

local x4, y4 = x1 + X, y1 - halfh
local halfh2 = 50

local rect2 = display.newRect(basex + up_vx * halfh2, basey + up_vy * halfh2, 150, 2 * halfh2)

rect2:setFillColor(0, 1, 1)

local to_center_x, to_center_y = rect2.x - ux, rect2.y - uy

local proj_x, proj_y = Project(to_center_x, to_center_y, right_vx, right_vy)

local base2_x, base2_y = ux + proj_x, uy + proj_y

display.newCircle(basex,basey, 15):setFillColor(0)
display.newCircle(base2_x,base2_y, 5)

rect2.rotation = rect1.rotation

local halfw2 = rect2.width / 2
