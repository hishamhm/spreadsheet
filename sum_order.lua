#!/usr/bin/lua


local function perm(f, ...)
   local args = {}
   for i = 1, select("#", ...) do
      args[i] = select(i, ...)
   end
   if #args == 0 then
      f()
   else
      for i = 1, #args do
         local other = {}
         for j = 1, #args do
            if j ~= i then
               table.insert(other, args[j])
            end
         end
         perm(function(...) f(args[i], ...) end, table.unpack(other))
      end
   end
end

local sum = function(a,b,c,d) print(a,b,c,d,a+b+c+d) end

-- MINVERSE({10,20;30,40})
perm(sum, -0.2, 0.1, 0.15, -0.05)
print()
