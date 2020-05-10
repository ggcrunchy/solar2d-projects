--- Module that generates geometry by sweeping a segment along an arc.

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
local cos = math.cos
local pi = math.pi
local sqrt = math.sqrt

-- Modules --
local bezier = require("spline_ops.bezier")

-- Exports --
local M = {}

--
--
--

local Ap, Bp, Cp = {}, {}, {}

local function PreparePrincipalControlPoints (params, index)
	local curve_offset, acomp, da = 0, params.a_component or "x"

	if index then
		local aoffset, into_vertices = 2 * (index - 1), params.into_vertices

		Ap.x, Ap.y, da = into_vertices[aoffset + 1], into_vertices[aoffset + 2], acomp == "x" and 1 or 2
		curve_offset = curve_offset + 1 -- skip index

		params.builder:SetLowerLeft(index) -- TODO: might be wrong?

		-- edge normals ignored, so leave as is
	else
		local ap = params.a_point

		Ap.x, Ap.y, da = ap.x, ap.y, params.da
	end

	local bp, cp = params.b_point, params.c_point

	Bp.x, Cp.x = bp.x, cp.x
	Bp.y, Cp.y = bp.y, cp.y

	return acomp, params.c_component or "y", da, params.dc, curve_offset
end

local function ComputeFromCurve (into_vertices, into_normals, into_knots, from_knot, to_knot, offset, t)
	local px, py, tx, ty = bezier.Bezier2_PosTan(Ap, Bp, Cp, t)

	into_vertices[offset + 1] = px
	into_vertices[offset + 2] = py

    local len = sqrt(tx^2 + ty^2 + 1e-8) -- n.b. guard against slight undercalculations

	into_normals[offset + 1] = ty / len
	into_normals[offset + 2] = -tx / len

	into_knots[.5 * offset + 1] = from_knot + t * (to_knot - from_knot) -- n.b. assumes offset is multiple of 2

	-- also consider recording the arc length (just doing Bezier2 with a stub in normals) and then doing another pass to arc length-parametrize everything
end

local function GetKnots (params)
	local from_knot1, from_knot2, to_knot1, to_knot2 = params.from_knot1, params.from_knot2, params.to_knot1, params.to_knot2

	assert(not from_knot1 == not from_knot2, "Either 'knot1' or 'knot2' specified for 'from' knot, but not both")
	assert(not to_knot1 == not to_knot2, "Either 't1' or 't2' specified for 'to' knot, but not both")

	from_knot1 = from_knot1 or params.from_knot or 0
	to_knot1 = to_knot1 or params.to_knot or 1 - from_knot1

	return from_knot1, to_knot1, from_knot2 and from_knot2 - from_knot1 or 0, to_knot2 and to_knot2 - to_knot1 or 0
end

local function AuxRowBeforeLayers (into_vertices, into_normals, into_knots, from_knot, to_knot, offset, t)
	ComputeFromCurve(into_vertices, into_normals, into_knots, from_knot, to_knot, offset, t)
end

local function RowBeforeLayers (into_vertices, into_normals, into_knots, from_knot, to_knot, offset, curve_offset, builder)
	local count, pos = builder:GetColumnCount(), 2 * curve_offset

	for i = curve_offset, count do
		local t = i / count

		AuxRowBeforeLayers(into_vertices, into_normals, into_knots, from_knot, to_knot, offset, t)

		if pos > 0 then
			builder:EmitBottomEdge()
		end

		offset, pos = offset + 2, pos + 2
	end

	return offset
end

local function UpdatePrincipalControlPoints (acomp, ccomp, da, dc, into_vertices, index)
	local c, a = Cp[ccomp] + dc

	if index then
		a = into_vertices[2 * (index - 1) + da]
    else
		a = Ap[acomp] + da
	end

	Ap[acomp], Bp[acomp] = a, a
	Cp[ccomp], Bp[ccomp] = c, c
end

--- DOCME
function M.Bezier (params)
	local base, from_indices = 0, params.from_indices
	local acomp, ccomp, da, dc, curve_offset = PreparePrincipalControlPoints(params, from_indices and from_indices[base + 1])	
	local layer_count, no_from_indices, from_knot1, to_knot1, dfrom_knot, dto_knot = params.layer_count, curve_offset == 0, GetKnots(params)
	local builder, into_vertices, into_normals, into_knots = params.builder, params.into_vertices, params.into_normals, params.into_knots
	local offset = RowBeforeLayers(into_vertices, into_normals, into_knots, from_knot1, to_knot1, params.offset or #into_normals, curve_offset, builder)
	local count, right, max_theta = builder:GetColumnCount(), params.right, params.is_half_arc and -.5 * pi or pi
-- ^^ TODO: probably want 'to' indices instead... then adapt c as we did with a
-- should maybe switch A, C -> from, to (maybe as *_x, *_y)
-- reduce redundancy / chance of error with component, e.g. say from = "x", versus deltas for both?
	if right then
		right[base + 1] = builder:GetMaxIndex() -- TODO: count = 0 or 1?
	end

	for layer = 1, layer_count do
		local index = from_indices and from_indices[base + layer + 1]

        assert(no_from_indices == not index, "Hole in 'from' indices, must either be empty or full")

        if index then
            builder:SetLowerLeft(index) -- TODO: might be wrong?
        end

		UpdatePrincipalControlPoints(acomp, ccomp, da, dc, into_vertices, index)

		local frac = layer / layer_count
		local cos_theta, from_knot, to_knot, pos = cos(max_theta * frac), from_knot1 + dfrom_knot * frac, to_knot1 + dto_knot * frac, 0

		for i = curve_offset, count do
			ComputeFromCurve(into_vertices, into_normals, into_knots, from_knot, to_knot, offset, i / count)

            into_normals[offset + 1] = cos_theta * into_normals[offset + 1]
            into_normals[offset + 2] = cos_theta * into_normals[offset + 2]
            -- implicit sin(theta) * (0, 0, 1)

            if i > 0 then -- TODO: corners, one side may conflict
                builder:EmitQuad()
            end

			offset, pos = offset + 2, pos + 2
		end

		if right then
			right[base + layer + 1] = builder:GetMaxIndex() -- TODO: n = 0 or 1?
		end
	end
end

return M