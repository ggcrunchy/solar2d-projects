--- Bits that change according to what we want to test.

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

-- The following may be changed to try different things:

-- This determines what we want to do with our knots and normals. Currently, the options are:
-- * "NormalShaded": The texture coordinates receive the normals, which are used to shade our objects with a given "light".
-- * "PackedNormalShaded": Our normals' components are converted from [-1, +1] floating-point numbers to [0, 255] integers.
-- These are packed together as a 16-bit value in texture coordinate y, then decoded in the vertex kernel. At this point,
-- everything is like "NormalShaded".
-- * "KnotToColor": Our [0, 1] knots are rendered to the red channel.
-- * "NormalToUV": Our texture coordinates again contain the normals, in [-1, +1]. After rescaling to [0, 1], these are
-- rendered in the red and green channels.
-- * "PackedNormalToUV": Like "NormalToUV", but our normals have gone through the process described in "PackedNormalShaded".
-- TODO: use the unit_exclusive machinery (see s3_utils.snippets) to pack (1024, 1024) values
-- TODO: convert (x, y) normals to (phi, theta) instead since our projection onto the z = 0 plane bunches up a lot of values, e.g. near-vertical ones
local DrawMode = "NormalShaded"

-- Is the arc part of our object half a cylinder (an extruded semicircle) or a full one (full circles)?
-- This determines the corresponding normals.
local IsHalfArc = true

-- Are the (curved) triangles slightly separated from the arc? (testing feature)
local IsSeparated = false

-- Draw objects in wireframe? (testing feature)
local IsWireframe = false

--
--
--

local Kernel = { category = "generator", name = "uv" }

local AddPosVarying1, AddPosVarying2 = "", ""

local SetUVs

if DrawMode == "NormalShaded" or DrawMode == "PackedNormalShaded" then

AddPosVarying1 = [[
	P_POSITION varying vec2 v_Pos;

]]

AddPosVarying2 = [[
		v_Pos = pos;

]]

Kernel.fragment = (AddPosVarying1 .. [[
	P_COLOR vec4 FragmentKernel (P_UV vec2 uv)
	{
		P_UV vec3 n = vec3(uv, 0.);

		n.z = sqrt(max(1. - dot(n, n), 0.));

		P_POSITION vec3 p = vec3(%.4f, %.4f + 128. * sin(1.7 * CoronaTotalTime), 512.);
		P_UV vec3 u = normalize(p - vec3(v_Pos, 0.));

		return vec4(vec3(.12, .1, .1) + .35 * max(0., dot(u, n)), 1.);
	}
]]):format(display.contentCenterX, display.contentCenterY)

	Kernel.isTimeDependent = true
elseif DrawMode == "NormalToUV" or DrawMode == "PackedNormalToUV" then
	Kernel.fragment = [[
		P_COLOR vec4 FragmentKernel (P_UV vec2 uv)
		{
			return vec4(clamp(.5 * uv + .5, 0., 1.), 0., 1.);
		}
	]]
else
	assert(DrawMode == "KnotToColor", "Unknown draw mode")

	Kernel.vertex = [[
		P_POSITION varying float v_Knot;

		P_POSITION vec2 VertexKernel (P_POSITION vec2 pos)
		{
			v_Knot = v_TexCoord.y;

			return pos;
		}
	]]

	Kernel.fragment = [[
		P_POSITION varying float v_Knot;

		P_COLOR vec4 FragmentKernel (P_UV vec2 _)
		{
			return vec4(v_Knot, 0., 0., 1.);
		}
	]]

	function SetUVs (_, knots)
		local new, j = {}, 1

		for _, v in ipairs(knots) do
			new[j], new[j + 1], j = 0, v, j + 2
		end

		return new
	end
end

local DecodeNormal = ""

if DrawMode == "PackedNormalShaded" or DrawMode == "PackedNormalToUV" then

DecodeNormal = [[
		v_TexCoord.y = mod(CoronaTexCoord.x, 256.);
		v_TexCoord.x = (CoronaTexCoord.x - v_TexCoord.y) / 256.;

		v_TexCoord = (2. / 255.) * v_TexCoord - 1.;

]]

	local function CrunchNormalComponent (comp)
		comp = .5 * comp + .5
		comp = math.max(0, math.min(comp * 255, 255))

		return math.floor(comp)
	end

	function SetUVs (normals)
		local new = {}

		for i = 1, #normals, 2 do
			new[i], new[i + 1] = CrunchNormalComponent(normals[i]) * 256 + CrunchNormalComponent(normals[i + 1]), 0
		end

		return new
	end
end

if #AddPosVarying1 + #AddPosVarying2 + #DecodeNormal > 0 then

Kernel.vertex = AddPosVarying1 .. [[
	P_POSITION vec2 VertexKernel (P_POSITION vec2 pos)
	{
]] .. AddPosVarying2 .. DecodeNormal .. [[
		return pos;
	}
]]

end
--[[
-- uncomment to see concatenated results; random-looking snippet formatting above was some attempt at pretty printing :D
print("VERTEX")
print(Kernel.vertex)
print("")
print("FRAGMENT")
print(Kernel.fragment)
]]

graphics.defineEffect(Kernel)

return {
	IsHalfArc = IsHalfArc, IsSeparated = IsSeparated, IsWireframe = IsWireframe,

	Effect = "generator.custom.uv",

	SetUVs = SetUVs or function(normals)
		return normals
	end
}