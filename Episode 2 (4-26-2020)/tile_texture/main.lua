--- Entry point.

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
local indexed = require("corona_utils.indexed")
local options = require("options")
local swept_arc = require("swept_arc")
local triangle = require("triangle")

--
--
--

local A = { x = 236, y = 100 }
local B = { x = 236, y = 164 }
local C = { x = 300, y = 164 }

local N = 20 -- n.b. should be even

local Knots, Normals, Verts = {}, {}, {}

local K = 16

local Builder1 = indexed.NewLatticeBuilder(N)

swept_arc.Bezier{
	into_knots = Knots, into_normals = Normals, into_vertices = Verts,
	a_point = A, b_point = B, c_point = C,
	da = -8.5, dc = 8.5, -- can divide by 2...
	layer_count = K, builder = Builder1, is_half_arc = options.IsHalfArc
}

local bi1, _ = Builder1:GetResult()

local m = display.newMesh{ indices = bi1, uvs = options.SetUVs(Normals, Knots), vertices = Verts, mode = "indexed" }

m.x, m.y = display.contentCenterX, display.contentCenterY

m.fill.effect = options.Effect

if options.IsWireframe then
	display.setDrawMode("wireframe")
end

--[[

	Analysis for knot

	A = 0 or 1
	B = 1 - A

	* Case #1:
	
	 A - - A
	| \     \__
	|  \       \
	|   \       ½
	|    \     / \
	|     \   /   \_
	|	   \ /      \
	|       B        \
	|      / \        B
	|     /   \_      |
	|    /      \_    |
	|   /         \_  |
	|  /            \ |
	| /              \|
    ½ - - - - - - - - ½


	C = ½ or ??

	* Case #2:
	
	 A - - A
	| \     \__
	|  \       \
	|   \       ½
	|    \     / \
	|     \   /   \_
	|	   \ /      \
	|       B        \
	|      / \        B
	|     /   \_      |
	|    /      \_    |
	|   /         \_  |
	|  /            \ |
	| /              \|
    C - - - - - - - - ½


	The lower-left triangles might go as per the summary in the triangle module.

	Ideally set up so that curve "sides" line up (or do start, direction)
]]

local NormalComponentCount = #Normals

local Left, Right, Bottom, UniqueEdgeIndicesCount = triangle.Indices(N)

local pprint = function() end
-- print

local Above = {}

local function DoTri (ledge, redge, bedge, index)
	local n, above = #redge, ledge

	assert(#ledge == n and n == #bedge)
	assert(ledge[1] == bedge[1])
	assert(ledge[n] == redge[1])
	assert(redge[n] == bedge[n])

	for i = 1, n - 1 do
		local ul = above[2] -- 1 = corner (not updated); 2 and up row above

		pprint("TRI1", bedge[i], ul, bedge[i + 1])

		local ll, lr = bedge[i + 1]

		for j = 2, n - i do
			local ur = above[j + 1]

			if j + i < n then -- not last in row?
				index = index + 1 -- new point (pos and uv should interpolate...)
				lr = index
			else
				lr = redge[i + 1]
			end

			pprint("QUAD1", ul, ur, ll, lr)

			ll, ul, Above[j] = lr, ur, lr -- put lower-right corner into "above" row for next pass
		end

		Above[n], above = lr, Above -- after first pass, use internal buffer as "above" row
	end
end

DoTri(Left, Right, Bottom, UniqueEdgeIndicesCount)

pprint("")

local EndOfPenultimateRow = NormalComponentCount - (N + 1) * 2

local Half = N / 2
local Offset = EndOfPenultimateRow + 2 * Half

local Builder2 = indexed.NewLatticeBuilder(Half)

Builder2:SetMaxIndex(UniqueEdgeIndicesCount)

local Knot2, Normal2, Vert2 = {}, {}, {}

local TopX, TopY = Verts[Offset + 1], Verts[Offset + 2]
local TopNX, TopNY = Normals[Offset + 1], Normals[Offset + 2]
local TopKnot = Knots[.5 * Offset + 1] -- TODO: is this right?

local LowerLeftX, LowerLeftY = Verts[EndOfPenultimateRow + 1], Verts[NormalComponentCount]
local LowerLeftKnot = .5

pprint("Lower Left", LowerLeftX, LowerLeftY)
pprint("Top", TopX, TopY)

triangle.LeftEdge{
	into_knots = Knot2, into_normals = Normal2, into_vertices = Vert2,
	lower_left_x = LowerLeftX, lower_left_y = LowerLeftY,
	lower_left_nx = 0, lower_left_ny = -1,
	lower_left_knot = LowerLeftKnot,
	top_x = TopX, top_y = TopY,
	top_nx = TopNX, top_ny = TopNY,
	top_knot = TopKnot,
	count = Half
}

triangle.RightEdge{
	from_knots = Knots, into_knots = Knot2,
	from_normals = Normals, into_normals = Normal2,
	from_vertices = Verts, into_vertices = Vert2,
	pivot = EndOfPenultimateRow + N, count = Half
}

local LowerRightX, LowerRightY = Vert2[#Vert2 - 1], Vert2[#Vert2]
local LowerRightNX, LowerRightNY = Normal2[#Normal2 - 1], Normal2[#Normal2]
local LowerRightKnot = Knot2[#Knot2]

pprint("Lower Right", LowerRightX, LowerRightY)

triangle.BottomEdge{
	into_knots = Knot2, into_normals = Normal2, into_vertices = Vert2,
	lower_left_x = LowerLeftX, lower_left_y = LowerLeftY,
	lower_right_x = LowerRightX, lower_right_y = LowerRightY,
	lower_left_nx = 0, lower_left_ny = -1,
	lower_right_nx = LowerRightNX, lower_right_ny = LowerRightNY,
	lower_left_knot = LowerLeftKnot,
	lower_right_knot = LowerRightKnot,
	count = Half
}

triangle.Populate(Builder2, Left, Right, Bottom, Knot2, Normal2, Vert2, { no_normalize = not options.IsHalfArc })

local mshift = 75
local mshift2 = 49

if options.IsSeparated then
	mshift = mshift + 1
end

local bi2, _ = Builder2:GetResult()
local m2 = display.newMesh{ indices = bi2, uvs = options.SetUVs(Normal2, Knot2), vertices = Vert2, mode = "indexed" }

m2.x, m2.y = display.contentCenterX, display.contentCenterY + mshift -- or mshift2 with dc / 2

m2.fill.effect = options.Effect

--m2:scale(5, 5)

local Knot3, Normal3, Vert3 = {}, {}, {}

for i = 1, (Half + 1) * 2 do -- copy for testing (apart from slight shift), but shared in real situation
	Normal3[i], Vert3[i] = Normal2[i], Vert2[i]
end

for i = 1, Half + 1 do
	Knot3[i] = Knot2[i]
end

-- "right": everything from nn backward...

triangle.RightEdge{
	from_knots = Knots, into_knots = Knot3,
	from_normals = Normals, into_normals = Normal3,
	from_vertices = Verts, into_vertices = Vert3,
	pivot = EndOfPenultimateRow + N, count = Half,
	backward = true
}

-- "bottom": slight variant on previous case...

local UpperLeftX, UpperLeftY = LowerLeftX, Verts[EndOfPenultimateRow + 2]

triangle.BottomEdge{
	into_knots = Knot3, into_normals = Normal3, into_vertices = Vert3,
	lower_left_x = LowerLeftX, lower_left_y = LowerLeftY,
	lower_right_x = UpperLeftX, lower_right_y = UpperLeftY,
	lower_left_knot = .5,
	lower_left_nx = 0, lower_left_ny = -1,
	lower_right_nx = Normals[EndOfPenultimateRow + 1], lower_right_ny = Normals[EndOfPenultimateRow + 2],
	lower_right_knot = 0,
	count = Half
}

local Builder3 = indexed.NewLatticeBuilder(Half)

Builder3:SetMaxIndex(UniqueEdgeIndicesCount)

triangle.Populate(Builder3, Left, Right, Bottom, Knot3, Normal3, Vert3, { no_normalize = not options.IsHalfArc })

local bi3, _ = Builder3:GetResult()
local m3 = display.newMesh{ indices = bi3, uvs = options.SetUVs(Normal3, Knot3), vertices = Vert3, mode = "indexed" }

m3.x, m3.y = display.contentCenterX - mshift, display.contentCenterY

m3.fill.effect = options.Effect

--m3:scale(5, 5)

local ii = 1

pprint("")

for r = 1, Half do
	pprint("TRI2", bi2[ii], bi2[ii + 1], bi2[ii + 2])

	ii = ii + 3

	for _ = r + 1, Half do
		pprint("QUAD2", bi2[ii], bi2[ii + 3], bi2[ii + 4], bi2[ii + 5])

		ii = ii + 6
	end
end

-- Goal:

-- ▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓
-- ▓▓▓▓     0           1     ▓▓▓▓:▓▓   ▓▓
-- ▓▓▓      :           :      ▓▓▓:▓     ▓
-- ▓▓  A    0     B     1    C  ▓▓:▓  D  ▓
-- ▓      ▓▓:▓▓       ▓▓:▓▓      ▓:▓     ▓
-- ▓     ▓▓▓:▓▓▓     ▓▓▓:▓▓▓     ▓:▓     ▓
-- ·1···1···+···1···0···+···0···0·+·0···0·
-- ▓     ▓▓▓:▓▓▓     ▓▓▓:▓▓▓     ▓:▓     ▓
-- ▓      ▓▓:▓▓       ▓▓:▓▓      ▓:▓     ▓
-- ▓        0           1        ▓:▓     ▓
-- ▓   E    :     F     :    G   ▓:▓  H  ▓
-- ▓        1           0        ▓:▓     ▓
-- ▓      ▓▓:▓▓       ▓▓:▓▓      ▓:▓     ▓
-- ▓     ▓▓▓:▓▓▓     ▓▓▓:▓▓▓     ▓:▓     ▓
-- ·0···0···+···0···1···+···1···1·+·1···1·
-- ▓     ▓▓▓:▓▓▓     ▓▓▓:▓▓▓     ▓:▓     ▓
-- ▓      ▓▓:▓▓       ▓▓:▓▓      ▓:▓     ▓
-- ▓▓  I    1     J     0    K  ▓▓:▓  L  ▓
-- ▓▓▓      :           :      ▓▓▓:▓     ▓
-- ▓▓▓▓     1           0     ▓▓▓▓:▓▓   ▓▓
-- ▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓
-- ·········+···········+·········+·······
-- ▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓
-- ▓▓       0           1       ▓▓:▓▓▓▓▓▓▓
-- ▓   M    :     N     :    O   ▓:▓▓▓▓▓▓▓
-- ▓▓       0           1       ▓▓:▓▓▓▓▓▓▓
-- ▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓▓▓:▓▓▓▓▓▓▓

-- This is the layout of the mesh, as it would be rendered into a texture. (It
-- might still be worth preserving should we dispense with that indirection, say
-- if meshes could be shared.) The "·" and ":" symbols indicate horizontal and
-- vertical seams, respectively, with "+" where these meet. Most empty spots are
-- denoted by "▓", although seams cover a few.

-- This is a 4x4 configuration, as divvied up by the aforementioned seams, with
-- one spot unused. The numbers along the seams are the u-values at each endpoint
-- of the edge found there: these are a bit arbitrary but chosen for some variety,
-- with the lion's share of continuity meant to be handled on the shader side.

-- Tiles "H" and "N" are rectangular and axis-aligned. Tiles "D", "L", "M", and "O"
-- that abut them are "caps", i.e. a rectangle topped by a half-disk.

-- Tiles "A", "C", "I", and "K" are curves, axis-aligned at the adjacent seams.

-- Tiles "B", "E", "G", and "J" are T-shaped. (TODO: two half-curve + quarter-diamond
-- parts, with a half-rectangle along one side; contra sed, no half-rectangle in my
-- notes, and in reality it would just run afoul of the same problems)

-- Tile "F" is a cross. (TODO: ditto)

-- Many edges and corners are shared.

-- Normals are computed assuming the objects are cylindrical or spherical, with
-- boundaries coincident with the xy-plane, z pointing up. (TODO: gaps)







local cubic = require("spline_ops.cubic")

local cx, cy = display.contentCenterX, display.contentCenterY - 100

local AA = { x = cx + 100, y = cy }
local DD = { x = cx - 100, y = cy }
local BB = { x = AA.x, y = cy - 125 }
local CC = { x = DD.x, y = cy - 125 }

for _ = 1, 3 do
	for i = 0, 10 do
		local x, y = cubic.GetPosition("bezier", AA, BB, CC, DD, i / 10)
		local c = display.newCircle(x, y, 5)

		c:setFillColor(1, 0, 0)
		c:setStrokeColor(0, 1, 0, .4)

		c.strokeWidth = 2
	end

	AA.x, BB.x, BB.y = AA.x - 20, BB.x - 20, BB.y + 30
	CC.x, DD.x, CC.y = CC.x + 20, DD.x + 20, CC.y + 30
end




local cap = require("cap")

local LayerCount = 4

local CapBuilder = indexed.NewLatticeBuilder(LayerCount)

local PP, NN, KK = {}, {}, {}

cap.Make{
	x1 = 0, y1 = 0, x2 = 300, y2 = 250,
	into_knots = KK, into_normals = NN, into_vertices = PP,
	count = 9, builder = CapBuilder,
	distance_delta = 60
}

local bb = CapBuilder:GetResult()


local m4 = display.newMesh{ indices = bb, uvs = options.SetUVs(NN, KK), vertices = PP, mode = "indexed" }

m4.x, m4.y = m3.x + 350, m3.y

m4.fill.effect = options.Effect