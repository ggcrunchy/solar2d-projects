--- Module that builds up geometry via triangles, possibly with curved edges.
--
-- TODO CLEAN THIS UP A BIT
--
--	These might go:
--	
--	
--			  B
--	         / \
--	        K   L
--	       / \ / \
--	      J   U   M
--	     / \ / \ / \
--		I   S   T   N
--	   / \ / \ / \ / \
--	  H   P   Q   R   O
--	 / \ / \ / \ / \ / \
--	A - D - E - F - G - C
--
--	Sides / corners will appear multiple times, so this must be accommodated
--	Will pre-generate points along sides to dovetail with situations where we reuse them
--	To a lesser extent, same is true as we build up this bit, but internal
--	By traversing diagonally, we can add one triangle plus n rects
--	This introduces at most one new point per rect (none on the last)
--	Could have another scratch buffer for previous side, e.g. P-S-U-L
--
--	Example:
--	
--		AHD, HIDP (P new), IJPS (S new), JKSU (U new), KBUL
--		DPE, PSEQ (Q new), SUQT (T new), ULTM
--		EQF, QTFR (R new), TMRN
--		FRG, RNGO
--		GOC

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
local assert = assert
local sqrt = math.sqrt

-- Exports --
local M = {}

--
--
--

local MustNormalize, NormalX, NormalY, NormalZ, NormalDX, NormalDY, NormalDZ

local function InterpolateNormals (t)
	local nx, ny = NormalX + t * NormalDX, NormalY + t * NormalDY

	if MustNormalize then -- else assume z = sqrt(1 - nx^2 - ny^2)
		local nz = NormalZ + t * NormalDZ
		local len = sqrt(nx^2 + ny^2 + nz^2)

		nx, ny = nx / len, ny / len
	end

	return nx, ny
end

local function SetNormals (nx1, ny1, nx2, ny2, no_normalize)
	local nz1, nz2 = sqrt(1 - nx1^2 - ny1^2), sqrt(1 - nx2^2 - ny2^2)

	NormalX, NormalDX = nx1, nx2 - nx1
	NormalY, NormalDY = ny1, ny2 - ny1
	NormalZ, NormalDZ = nz1, nz2 - nz1
	MustNormalize = not no_normalize and nx1 * nx2 + ny1 * ny2 >= 0 -- With 'no_normalize', we account for situations like flat triangles; normals in the
																	-- plane, pointing away from one another, are like this. (TODO: are obtuse angles too
																	-- broad? should be tighten this up to just dot products near -1, i.e. nearly-opposite
																	-- directions?) Normalization breaks down in these cases, since we never pick up a z-
																	-- component, so we use the lerp'd results as a compromise.
end

--- DOCME
function M.BottomEdge (params) -- TODO: are these always new?
	local llx, lly, ll_knot = params.lower_left_x, params.lower_left_y, params.lower_left_knot
	local dx, dy, dknot = params.lower_right_x - llx, params.lower_right_y - lly, params.lower_right_knot - ll_knot
	local into_knots, into_normals, into_vertices = params.into_knots, params.into_normals, params.into_vertices
	local offset, count = params.offset or #into_normals, params.count
	local koffset = .5 * offset -- n.b. assumes offset is multiple of 2

	SetNormals(params.lower_left_nx, params.lower_left_ny, params.lower_right_nx, params.lower_right_ny)

	for i = 1, count - 1 do
		local t = i / count

		into_normals[offset + 1], into_normals[offset + 2] = InterpolateNormals(t)
		offset, into_vertices[offset + 1], into_vertices[offset + 2] = offset + 2, llx + t * dx, lly + t * dy
		koffset, into_knots[koffset + 1] = koffset + 1, ll_knot + t * dknot
	end
end

--- DOCME
function M.LeftEdge (params)
	local llx, lly, ll_knot = params.lower_left_x, params.lower_left_y, params.lower_left_knot
	local dx, dy, dknot = params.top_x - llx, params.top_y - lly, params.top_knot - ll_knot

	SetNormals(params.lower_left_nx, params.lower_left_ny, params.top_nx, params.top_ny)

	-- TODO: might already have lower left and / or top
	-- in that case, adjust loop below and extract ll* / t* from preexisting verts / uvs (could always assume from = into?)

	local into_knots, into_normals, into_vertices = params.into_knots, params.into_normals, params.into_vertices
	local offset, count = params.offset or #into_normals, params.count
	local koffset = .5 * offset -- n.b. assumes offset is multiple of 2

	for i = 0, count do
		local t = i / count

		into_normals[offset + 1], into_normals[offset + 2] = InterpolateNormals(t)
		offset, into_vertices[offset + 1], into_vertices[offset + 2] = offset + 2, llx + t * dx, lly + t * dy
		koffset, into_knots[koffset + 1] = koffset + 1, ll_knot + t * dknot
	end
end

--- DOCME
function M.RightEdge (params) -- TODO: in practice, will this ever be called? (since everything already exists)
	local from_knots, from_normals, from_vertices = params.from_knots, params.from_normals, params.from_vertices
	local into_knots, into_normals, into_vertices = params.into_knots, params.into_normals, params.into_vertices
	local offset, j, step = params.offset or #into_normals, params.pivot, params.backward and -2 or 2
	local koffset = .5 * offset -- n.b. assumes offset is multiple of 2

	for _ = 1, params.count do
		j = j + step
		into_normals[offset + 1], into_vertices[offset + 1] = from_normals[j + 1], from_vertices[j + 1]
		into_normals[offset + 2], into_vertices[offset + 2] = from_normals[j + 2], from_vertices[j + 2]
		into_knots[koffset + 1] = from_knots[.5 * j + 1]
		offset, koffset = offset + 2, koffset + 1
	end
end

--- DOCME
function M.Indices (n) -- TODO: not as first / only shape...
	local median, left, right, bottom = n / 2 + 1, {}, {}, {}

	for i = 1, median do
		left[i] = i
	end

	for i = 1, median do
		right[i] = median + i - 1
	end

	bottom[#bottom + 1] = 1

	local last = right[#right]

	for i = 1, median - 2 do
		bottom[#bottom + 1] = last + i
	end

	bottom[#bottom + 1] = last

	return left, right, bottom, bottom[#bottom - 1]
end

--- DOCME
function M.Populate (B, ledge, redge, bedge, knots, normals, vertices, params)
	local n = #redge

	assert(#ledge == n and n == #bedge)

	B:SetLowerLeft(ledge[1])

	for i = 2, n do
		B:SetLowerRight(ledge[i])
		B:EmitBottomEdge()
	end

	local offset, no_normalize = (params and params.offset) or #normals, params.no_normalize
	local koffset = .5 * offset -- n.b. assumes offset is multiple of 2

	for i = 1, n - 1 do
		for _ = 2, i do
			B:Skip()
		end

		local bi, ri = bedge[i + 1], redge[i + 1]
		local boffset, roffset = 2 * (bi - 1), 2 * (ri - 1)

		B:SetLowerRight(bi)
		B:EmitTriangle()

		SetNormals(normals[boffset + 1], normals[boffset + 2], normals[roffset + 1], normals[roffset + 2], no_normalize)

		local knot1, x1, y1 = knots[bi], vertices[boffset + 1], vertices[boffset + 2]
		local nbins, dx, dy, dknot = n - i, vertices[roffset + 1] - x1, vertices[roffset + 2] - y1, knots[ri] - knot1

        if nbins > 1 then -- triangle always gets one, any remaining are rects
            local t, dt = 0, 1 / (nbins - 1)
        
            for _ = 2, nbins - 1 do -- rects that introduce a new index
                t = t + dt
				normals[offset + 1], normals[offset + 2] = InterpolateNormals(t)
                offset, vertices[offset + 1], vertices[offset + 2] = offset + 2, x1 + t * dx, y1 + t * dy
				koffset, knots[koffset + 1] = koffset + 1, knot1 + t * dknot

                B:EmitQuad()
            end

            B:SetLowerRight(ri) -- rect that only uses old indices
            B:EmitQuad()
        end
	end
end

return M