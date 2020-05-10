--- Module that generates cap-style geometry, e.g. the two ends of a capsule.

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
local sqrt = math.sqrt

-- Modules --
local cubic = require("spline_ops.cubic")

-- Exports --
local M = {}

--
--
--

local P, CP1, CP2, Q = {}, {}, {}, {}

--- DOCME
function M.Make (params)
    local from_indices, from_knot1, from_knot2 = params.from_indices, params.from_knot1, params.from_knot2

    assert(not from_knot1 == not from_knot2, "Must have neither or both!") -- TODO

    if not from_knot1 then
        from_knot1 = params.from_knot or 0
        from_knot2 = from_knot2 or from_knot1
    end

    local to_knot, nfrom = params.to_knot or 1 - from_knot1, #(from_indices or "")
    local count = params.count or nfrom - 1

    assert(count > 0, "Invalid count")

    local into_normals, into_knots, into_vertices = params.into_normals, params.into_knots, params.into_vertices
    local no_indices, dx, dy, ux, uy, dist = nfrom == 0

    if no_indices then
        local x1, y1, x2, y2 = params.x1, params.y1, params.x2, params.y2

        if params.is_base_vertical then
            P.x, Q.x, P.y, Q.y = x1, x1, y1, y2
            dx, dy, ux, uy = 0, y2 - y1, x2 > x1 and 1 or -1, 0
        else
            P.y, Q.y, P.x, Q.x = y1, y1, x1, x2
            dx, dy, ux, uy = x2 - x1, 0, 0, y2 > y1 and 1 or -1
        end

        dist = (x2 - x1) * ux + (y2 - y1) * uy
    else
        assert(nfrom > count, "Not enough indices to satisfy count")

--      P.x, P.y = vertices[from_indices[1]]
--      Q.x, Q.y = vertices[from_indices[count]]
--      other stuff from normals, ts (revise above)
--      d = (Q - P) / count (only used to find n) 
--      n = normalize(perp(d), sense) | sense = cw/ccw
    end

    local builder, ddist = params.builder, params.distance_delta
    local layer_count = builder:GetColumnCount()
    local base_count = 2 * layer_count

    dx, dy = dx / base_count, dy / base_count

    local offset, dknot = params.offset or #into_normals, (from_knot2 - from_knot1) / base_count
    local koffset, to_interior = .5 * offset, no_indices and 1 or 0
    local k0 = koffset

    for i = 1, layer_count do
        CP1.x, CP1.y = P.x + ux * dist, P.y + uy * dist
        CP2.x, CP2.y = Q.x + ux * dist, Q.y + uy * dist

        local frac = 1 - (i - 1) / layer_count
        local knot2 = from_knot1 + frac * (to_knot - from_knot1)

        for j = 1 - to_interior, count + to_interior - 1 do
            local t = j / count
            local px, py = cubic.GetPosition("bezier", P, CP1, CP2, Q, t)
            local tx, ty = cubic.GetTangent("bezier", P, CP1, CP2, Q, t)
            local scale = frac / sqrt(tx^2 + ty^2 + 1e-8)

            into_vertices[offset + 1] = px
            into_vertices[offset + 2] = py
            into_normals[offset + 1] = ty * scale
            into_normals[offset + 2] = -tx * scale

            local s = 2 * abs(.5 - t)

            into_knots[koffset + 1] = s * from_knot1 + (1 - s) * knot2

            offset, koffset = offset + 2, koffset + 1
        end

        if no_indices then
            P.x, P.y, Q.x, Q.y = P.x + dx, P.y + dy, Q.x - dx, Q.y - dy
        else
            --
        end

        dist, from_knot1, from_knot2 = dist - ddist, from_knot1 + dknot, from_knot2 - dknot
    end

    local base_index, mid_index = builder:GetMaxIndex()

    if no_indices then
        builder:SetUpperLeft(base_index + 1)

        into_vertices[offset + 1] = Q.x
        into_vertices[offset + 2] = Q.y
        into_normals[offset + 1] = 0
        into_normals[offset + 2] = 0
        into_knots[koffset + 1] = from_knot2

        mid_index = base_index + koffset + 1 - k0
    else
        builder:SetUpperLeft(from_indices[1])

        mid_index = from_indices[layer_count]
    end

    local stride = count + 2 * to_interior - 1 -- no indices: n bins = n - 1 points, plus both endpoints; else omit endpoints

    for i = 1, count do
        if i < count or no_indices then
            builder:SetLowerLeft(base_index + to_interior + i)
        else
            builder:SetLowerLeft(from_indices[count - i + 1])
        end

        for j = 1, layer_count - 1 do
            local next_index = base_index + j * stride + 1

            if i == 1 then
                if no_indices then
                    builder:SetUpperRight(next_index)
                else
                    builder:SetUpperRight(from_indices[i + 1])
                end
            end

            if i < count or no_indices then
                builder:SetLowerRight(next_index + i)
            else
                builder:SetLowerRight(from_indices[count - i])
            end

            builder:EmitQuad()
        end

        builder:SetAboveFlag()
        builder:SetUpperRight(mid_index)
        builder:EmitTriangle()
    end
end

-- An example with 4 layers and the arc split into 3 parts:

-- A     B     C     D     E     F     G     H     I
-- X-----X-----X-----X-----X-----X-----X-----X-----X
-- |     |     |     |    / \    |     |     |     |
-- |     |     |     |   /   \   |     |     |     |
--  \     \     \     \ /     \ /     /     /     /
--   \     \     \   P x-------x Q   /     /     /
--    \     \     \   /         \   /     /     /
--     \     \     \ /           \ /     /     /
--      \     \   N x-------------x O   /     /
--       \     \   /               \   /     /
--        \     \ /                 \ /     /
--         \   L x-------------------x M   /
--          \   /                     \   /
--           \ /                       \ /
--          J x-------------------------x K

-- #1: ABJL | BCLN | CDNP | DEP
-- #2: JLKM | LNMO | NPOQ | PEQ
-- #3: KMHI | MOHG | OQGF | QEF

return M