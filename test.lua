local v1 = 5 + foo(a, b, { 1.2, 453.12 })
local v2 = bar { 1, 2, 3 }
local v3 = { 1, 2, 3, 4 }
local v4 = xs[({ 1, 2, 3 })[1]]
local str = bad "aaaaa"
local sum = 1 + 2 + a
local result = a and b or 3
local function foo(a, b, c)
    local x = 1 .. "aaa"
    local y = 1 + 2
    return x / y + 2
end
for i = 1, 10 do
    print(i)
end
while true do
    print("a")
end
if true then
    print("a")
elseif false then
    print("b")
else
    print("c")
end
str = 1
foo()
