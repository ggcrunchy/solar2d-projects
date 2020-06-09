--- Test for Blend2D plugin.

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

local blend2d = require("plugin.blend2d")

local Example = 8

local Filename = "bl-getting-started-" .. Example .. ".bmp"

local img = blend2d.image.New(480, 480, "PRGB32")
local ctx = blend2d.context.New(img)

ctx:setCompOp("SRC_COPY")
ctx:fillAll()

local codec

if Example == 1 then
    local path = blend2d.path.New()

    path:moveTo(26, 31)
    path:cubicTo(642, 132, 587, -136, 25, 464)
    path:cubicTo(882, 404, 144, 267, 27, 31)

    ctx:setCompOp("SRC_OVER")
    ctx:setFillStyle(0xFFFFFFFF)
    ctx:fillPath(path)

    codec = blend2d.codec.New()

    codec:findByName("BMP")
elseif Example == 2 then
    -- Coordinates can be specified now or changed later.
    local linear = blend2d.gradient.New{ type = "LINEAR", y1 = 480 }

    -- Color stops can be added in any order.
    linear:addStop(0.0, 0xFFFFFFFF)
    linear:addStop(0.5, 0xFF5FAFDF)
    linear:addStop(1.0, 0xFF2F5FDF)

    -- `setFillStyle()` can be used for both colors and styles.
    ctx:setFillStyle(linear)
    ctx:setCompOp("SRC_OVER")
    ctx:fillRoundRect(40.0, 40.0, 400.0, 400.0, 45.5)

    codec = blend2d.codec.New()

    codec:findByName("BMP")
elseif Example == 3 then
    -- Read an image from file.
    local texture = blend2d.image.New()
    local ok, err = texture:readFromFile(system.pathForFile("Image1.jpg"))

    -- Basic error handling is necessary as we need some IO.
    if err then
        print(("Failed to load a texture (err=%u)\n"):format(err))

        return
    end

    -- Create a pattern and use it to fill a rounded-rect.
    local pattern = blend2d.pattern.New(texture)

    ctx:setCompOp("SRC_OVER")
    ctx:setFillStyle(pattern)
    ctx:fillRoundRect(40.0, 40.0, 400.0, 400.0, 45.5)
elseif Example == 4 then
    -- Read an image from file.
    local texture = blend2d.image.New()
    local ok, err = texture:readFromFile(system.pathForFile("Image1.jpg"))

    -- Basic error handling is necessary as we need some IO.
    if err then
        print(("Failed to load a texture (err=%u)\n"):format(err))

        return
    end

    -- Rotate by 45 degrees about a point at [240, 240].
    ctx:rotate(0.785398, 240.0, 240.0)

    -- Create a pattern.
    ctx:setCompOp("SRC_OVER")
    ctx:setFillStyle(blend2d.pattern.New(texture))
    ctx:fillRoundRect(50.0, 50.0, 380.0, 380.0, 80.5)
elseif Example == 5 then
    -- First shape filld by a radial gradient.
    local radial = blend2d.gradient.New{ type = "RADIAL", x0 = 180, y0 = 180, x1 = 180, y1 = 180, r0 = 180 }

    radial:addStop(0.0, 0xFFFFFFFF)
    radial:addStop(1.0, 0xFFFF6F3F)

    ctx:setCompOp("SRC_OVER")
    ctx:setFillStyle(radial)
    ctx:fillCircle(180, 180, 160);

    -- Second shape filled by a linear gradient.
    local linear = blend2d.gradient.New{ type = "LINEAR", x0 = 195, y0 = 195, x1 = 470, y1 = 470 }

    linear:addStop(0.0, 0xFFFFFFFF)
    linear:addStop(1.0, 0xFF3F9FFF)

    ctx:setCompOp("DIFFERENCE")
    ctx:setFillStyle(linear)
    ctx:fillRoundRect(195, 195, 270, 270, 25)
elseif Example == 6 then
    local linear = blend2d.gradient.New{ type = "LINEAR", y1 = 480 }

    linear:addStop(0.0, 0xFFFFFFFF)
    linear:addStop(1.0, 0xFF1F7FFF)

    local path = blend2d.path.New()

    path:moveTo(119, 49)
    path:cubicTo(259, 29, 99, 279, 275, 267)
    path:cubicTo(537, 245, 300, -170, 274, 430)

    ctx:setCompOp("SRC_OVER")
    ctx:setStrokeStyle(linear)
    ctx:setStrokeWidth(15)
    ctx:setStrokeStartCap("ROUND")
    ctx:setStrokeEndCap("BUTT")
    ctx:strokePath(path)
elseif Example == 7 then
    local face = blend2d.fontface.New()
    local ok, err = face:createFromFile(system.pathForFile("NotoSans-Regular.ttf"))

    -- We must handle a possible error returned by the loader.
    if err then
        print(("Failed to load a font-face (err=%u)\n"):format(err))

        return
    end

    local font = blend2d.font.New()
    font:createFromFace(face, 50.0)

    ctx:setFillStyle(0xFFFFFFFF)
    ctx:fillUtf8Text({ x = 60, y = 80 }, font, "Hello Blend2D!")

    ctx:rotate(0.785398)
    ctx:fillUtf8Text({ x = 250, y = 80 }, font, "Rotated Text")
elseif Example == 8 then
--[[
  ctx.setCompOp(BL_COMP_OP_SRC_COPY);
  ctx.fillAll();
  ctx.setFillStyle(BLRgba32(0xFFFFFFFF));

  BLFontFace face;
  BLResult err = face.createFromFile("NotoSans-Regular.ttf");
  if (err) {
    printf("Failed to load a font-face (err=%u)\n", err);
    return 1;
  }

  BLFont font;
  font.createFromFace(face, 20.0f);

  BLFontMetrics fm = font.metrics();
  BLTextMetrics tm;
  BLGlyphBuffer gb;

  BLPoint p(20, 190 + fm.ascent);
]]
    local text = "Hello Blend2D!\n" ..
                     "I'm a simple multiline text example\n" ..
                     "that uses BLGlyphBuffer and fillGlyphRun!"

    while true do
        local sep = text:find("\n")

        if sep then
            print(ii, text:sub(1, sep - 1))
        else
            print(ii, text)
        end
--[[
    gb.setUtf8Text(text, end ? (size_t)(end - text) : SIZE_MAX);
    font.shape(gb);
    font.getTextMetrics(gb, tm);

    p.x = (480.0 - (tm.boundingBox.x1 - tm.boundingBox.x0)) / 2.0;
    ctx.fillGlyphRun(p, font, gb.glyphRun());
    p.y += fm.ascent + fm.descent + fm.lineGap;
]]
        if not sep then
            break
        end

        text = text:sub(sep + 1)
    end
return
end

ctx["end"](ctx)

img:writeToFile(system.pathForFile(Filename, system.DocumentsDirectory), codec)

display.newImage(Filename, system.DocumentsDirectory, display.contentCenterX, display.contentCenterY)

--[[
Also https://github.com/blend2d/blend2d-samples/blob/master/getting-started/src/bl-capi-sample.c:

BLResult r;
  BLImageCore img;
  BLContextCore ctx;
  BLGradientCore gradient;

  r = blImageInitAs(&img, 256, 256, BL_FORMAT_PRGB32);
  if (r != BL_SUCCESS)
    return 1;

  r = blContextInitAs(&ctx, &img, NULL);
  if (r != BL_SUCCESS)
    return 1;

  BLLinearGradientValues values = { 0, 0, 256, 256 };
  r = blGradientInitAs(&gradient,
    BL_GRADIENT_TYPE_LINEAR, &values,
    BL_EXTEND_MODE_PAD, NULL, 0, NULL);
  if (r != BL_SUCCESS)
    return 1;

  blGradientAddStopRgba32(&gradient, 0.0, 0xFFFFFFFFu);
  blGradientAddStopRgba32(&gradient, 0.5, 0xFFFFAF00u);
  blGradientAddStopRgba32(&gradient, 1.0, 0xFFFF0000u);

  blContextSetFillStyleObject(&ctx, &gradient);
  blContextFillAll(&ctx);
  blGradientDestroy(&gradient);

  BLCircle circle;
  circle.cx = 128;
  circle.cy = 128;
  circle.r = 64;

  blContextSetCompOp(&ctx, BL_COMP_OP_EXCLUSION);
  blContextSetFillStyleRgba32(&ctx, 0xFF00FFFFu);
  blContextFillGeometry(&ctx, BL_GEOMETRY_TYPE_CIRCLE, &circle);

  blContextEnd(&ctx);

  // An example of querying a codec from Blend2D internal codecs.
  BLImageCodecCore codec;
  blImageCodecInit(&codec);
  blImageCodecFindByName(&codec, "BMP", SIZE_MAX, NULL);
  blImageWriteToFile(&img, "bl-capi-sample.bmp", &codec);
  blImageCodecDestroy(&codec);

  blImageDestroy(&img);


  Also also: 
    https://blend2d.com/#MoreSamples, implemented here:
    https://github.com/blend2d/blend2d-samples/tree/master/qt/src
]]