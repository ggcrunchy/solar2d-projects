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

local var_dump = require("tektite_core.var.dump")

--- Helper to print formatted argument.
-- @string s Format string.
-- @param ... Format arguments.
function printf (s, ...)
	print(s:format(...))
end

-- Install printf as the default var dump routine.
var_dump.SetDefaultOutf(printf)

--- Helper to dump generic variable.
-- @param var Variable to dump.
-- @param name As per @{tektite_core.var.dump.Print}.
-- @uint limit As per @{tektite_core.var.dump.Print}.
function vdump (var, name, limit)
	var_dump.Print(var, name and { name = name, limit = limit })
end

-- Monkey-patch.
local type = type

function display.isValid (object)
	return type(object) == "table" and object.parent ~= nil
end

--require("tests.node_pattern")
require("tests.shader_graph.core")