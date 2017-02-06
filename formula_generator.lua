
local square = true
local munit = true
local inverse = false

--- This setup loads in Google Sheets
-- local square = false
-- local munit = false
-- local inverse = true

--- This setup fails to load in Google Sheets
-- local square = false
-- local munit = true
-- local inverse = true

--- This setup crashes LibreOffice 4.2.3.3
-- square = true
-- munit = true
-- inverse = true

local values = function()
   local r1 = "A1"
   local r2 = square and "B2" or "B1"
   coroutine.yield("5")
   coroutine.yield(square and "{10;20|30;40}" or "{10;20}")
   coroutine.yield("[."..r1.."]")
   coroutine.yield("[."..r1..":."..r2.."]")
   coroutine.yield("INDIRECT(&quot;"..r1.."&quot;)")
   coroutine.yield("INDIRECT(&quot;"..r1..":"..r2.."&quot;)")
   coroutine.yield("INDIRECT({&quot;"..r1.."&quot;;&quot;"..r2.."&quot;})")
   coroutine.yield("INDIRECT({&quot;"..r1..":"..r2.."&quot;})")
end

local fn_types = function(z)
   return function()
      if z == "@" then
         coroutine.yield("@")
      end
      coroutine.yield("SQRT(@)") -- scalar-to-scalar
      coroutine.yield("SUM(@)") -- array-to-scalar
      coroutine.yield(munit and "MUNIT(@)" or "TRANSPOSE(@)") --scalar-to-array
      coroutine.yield(inverse and "MINVERSE(@)" or "TRANSPOSE(@)") -- array-to-array
   end
end

local function foreach(gen, f)
   local coro = coroutine.create(gen)
   while true do
      local ok, value = coroutine.resume(coro)
      if not value then
         break
      end
      f(value)
   end
end

local get_formula = coroutine.wrap(function()
   foreach(fn_types("@"), function(a)
      foreach(fn_types(a), function(b)
         foreach(fn_types("x"), function(c)
            foreach(values, function(value)
               local formula = (c):gsub("@", value)
               formula = (b):gsub("@", formula)
               formula = (a):gsub("@", formula)
               coroutine.yield(formula)
            end)
         end)
      end)
   end)
end)

local fd = io.open("full_test.fods", "r")
local out = io.open("full_formulas.fods", "w")
local formula
for line in fd:lines() do
   if line:match('table:formula="of:=%[.I') then
      formula = get_formula()
      line = line:gsub('table:formula="of:=%[.I%d+%]"', 'table:formula="of:='..formula..'"')
   elseif line:match('table:formula="of:=%[.L') then
      line = '     <table:table-cell table:number-matrix-columns-spanned="2" table:number-matrix-rows-spanned="1" table:formula="of:='..formula..'" office:value-type="float" office:value="0" calcext:value-type="float">'
   elseif line:match('FORMULA_TEXT') then
      line = '      <text:p>'..formula:gsub("&quot;", "\""):gsub("%[.([^]]+)%]", "%1")..'</text:p>'
   end
   out:write(line.."\n")
end
fd:close()
out:close()
