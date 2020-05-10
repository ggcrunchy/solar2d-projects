--- Gilbert-Johnson-Keerthi algorithm, adapted from [kroitor](https://github.com/kroitor/gjk.c)'s
-- implementation as well as [dyn4j](https://github.com/dyn4j/dyn4j/blob/master/src/main/java/org/dyn4j/collision/narrowphase/Gjk.java).

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
local sqrt = math.sqrt

-- Exports --
local M = {}

--
--
--

------------------------------------------------------------------------------
-- Triple product expansion is used to calculate perpendicular normal vectors 
-- which kinda 'prefer' pointing towards the Origin in Minkowski space

local function TripleProduct (ax, ay, bx, by, cx, cy)
    local ac = ax * cx + ay * cy -- perform a.dot(c)
    local bc = bx * cx + by * cy -- perform b.dot(c)
    
    -- perform b * a.dot(c) - a * b.dot(c)
    return bx * ac - ax * bc, by * ac - ay * bc
end

------------------------------------------------------------------------------
-- This is to compute average center (roughly). It might be different from
-- Center of Gravity, especially for bodies with nonuniform density,
-- but this is ok as initial direction of simplex search in GJK.

local function AveragePoint (vertices, count)
    local ax, ay = 0, 0

    for i = 1, 2 * count, 2 do
        ax, ay = ax + vertices[i], ay + vertices[i + 1]
    end

    return ax / count, ay / count
end

------------------------------------------------------------------------------
-- Get furthest vertex along a certain direction

local function IndexOfFurthestPoint (vertices, count, dx, dy)
    local index, max_product = 1, dx * vertices[1] + dy * vertices[2]

    for i = 3, 2 * count, 2 do
        local product = dx * vertices[i] + dy * vertices[i + 1]

        if product > max_product then
            index, max_product = i, product
        end
    end

    return index
end

------------------------------------------------------------------------------
-- Minkowski sum support function for GJK

local function Support (vertices1, count1, vertices2, count2, dx, dy)
    -- get furthest point of first body along an arbitrary direction
    local i = IndexOfFurthestPoint(vertices1, count1, dx, dy)

    -- get furthest point of second body along the opposite direction
    local j = IndexOfFurthestPoint(vertices2, count2, -dx, -dy)

    -- subtract (Minkowski sum) the two points to see if bodies 'overlap'
    return vertices1[i] - vertices2[j], vertices1[i + 1] - vertices2[j + 1], i, j
end

local function GetInitialDirection (vertices1, count1, vertices2, count2)
	local p1x, p1y = AveragePoint(vertices1, count1) -- not a CoG but
    local p2x, p2y = AveragePoint(vertices2, count2) -- it's ok for GJK

    -- initial direction from the center of 1st body to the center of 2nd body
    local dx, dy = p1x - p2x, p1y - p2y

    -- if initial direction is zero – set it to any arbitrary axis (we choose X)
    if 1 + dx^2 + dy^2 == 1 then
        return 1, 0
    else
		return dx, dy
	end
end

------------------------------------------------------------------------------
-- The GJK yes/no test
function M.GJK (vertices1, count1, vertices2, count2)
	local dx, dy = GetInitialDirection(vertices1, count1, vertices2, count2)
    
    -- set the first support as initial point of the new simplex
    local s1x, s1y, s2x, s2y

    while true do
        local ax, ay = Support(vertices1, count1, vertices2, count2, dx, dy)
        
        if ax * dx + ay * dy <= 0 then
            return false -- no collision
        end
        
        local aox, aoy = -ax, -ay -- from point A to Origin is just negative A
        
        if s2x then
            local abx, aby = s2x - ax, s2y - ay -- from point A to B
            local acx, acy = s1x - ax, s1y - ay -- from point A to C
            local acperpx, acperpy = TripleProduct(abx, aby, acx, acy, acx, acy)
            
            if acperpx * aox + acperpy * aoy >= 0 then
                dx, dy = acperpx, acperpy -- new direction is normal to AC towards Origin
            else
                local abperpx, abperpy = TripleProduct(acx, acy, abx, aby, abx, aby)

                if abperpx * aox + abperpy * aoy < 0 then
                    return true -- collision
                end

                s1x, s1y = s2x, s2y -- swap first element (point C)
                dx, dy = abperpx, abperpy -- new direction is normal to AB towards Origin
            end

            s2x, s2y = ax, ay -- swap element in the middle (point B)

        -- simplex has 1 or 2 points (a point or line segment, not a triangle yet)
        elseif s1x then
			local abx, aby = s1x - ax, s1y - ay -- from point A to B

			s2x, s2y, dx, dy = ax, ay, TripleProduct(abx, aby, aox, aoy, abx, aby) -- normal to AB towards Origin

			if 1 + dx^2 + dy^2 == 1 then
				dx, dy = aby, -abx
			end

        else
            s1x, s1y, dx, dy = ax, ay, aox, aoy
            -- skip to next iteration
        end
    end
end

local function ClosestOnEdge (ax, ay, bx, by)
	-- project (O - A) onto (B - A)
	local dx, dy = bx - ax, by - ay
	local numer, denom = -(ax * dx + ay * dy), dx^2 + dy^2

	if numer < 0 or 1 + denom == 1 then
		return ax, ay
	elseif numer > denom then
		return bx, by
	else
		local t = numer / denom

		return ax + t * dx, ay + t * dy
	end
end

function M.DistanceAndPoints (vertices1, count1, vertices2, count2)
	local dx, dy = GetInitialDirection(vertices1, count1, vertices2, count2)
	local ax, ay, ia, ja = Support(vertices1, count1, vertices2, count2, dx, dy)
	local bx, by, ib, jb = Support(vertices1, count1, vertices2, count2, -dx, -dy)

	dx, dy = ClosestOnEdge(ax, ay, bx, by)

	for _ = 1, count1 * count2 do
		dx, dy = -dx, -dy

		if 1 + dx^2 + dy^2 == 1 then
			return false
		end

		local cx, cy, ic, jc = Support(vertices1, count1, vertices2, count2, dx, dy)
		local dc = cx * dx + cy * dy
		local da = ax * dx + ay * dy

		if dc - da < 1e-2 then
			return sqrt(dx^2 + dy^2), ia, ja, ib, jb
		end

		local p1x, p1y = ClosestOnEdge(ax, ay, cx, cy)
		local p2x, p2y = ClosestOnEdge(cx, cy, bx, by)

		if p1x^2 + p1y^2 < p2x^2 + p2y^2 then
			bx, by, ib, jb, dx, dy = cx, cy, ic, jc, p1x, p1y
		else
			ax, ay, ia, ja, dx, dy = cx, cy, ic, jc, p2x, p2y
		end
	end
	print("!")
end

return M