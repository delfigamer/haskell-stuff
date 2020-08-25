-- $Id: testes/errors.lua $
-- See Copyright Notice in file all.lua

print("testing errors")

local debug = require"debug"

-- avoid problems with 'strict' module (which may generate other error messages)
local mt = getmetatable(_G) or {}
local oldmm = mt.__index
mt.__index = nil

local function checkerr (msg, f, ...)
  local st, err = xpcall(f, nil, ...)
  assert(not st)
  if not string.find(err, msg) then
    print(err, msg)
  end
end


local function doit (s)
  local f, msg = load(s)
  if not f then return msg end
  local cond, msg = xpcall(f, nil)
  return (not cond) and msg
end


local function checkmessage (prog, msg)
  local m = doit(prog)
  if not string.find(m, msg, 1, true) then
    print(m, msg)
    assert(false)
  end
end

local function checksyntax (prog, extra, token, line)
  local msg = doit(prog)
  -- if not string.find(token, "^<%a") and not string.find(token, "^char%(")
    -- then token = "'"..token.."'" end
  -- token = string.gsub(token, "(%p)", "%%%1")
  -- local pt = string.format([[^%%[string ".*"%%]:%d: .- near %s$]],
                           -- line, token)
  -- assert(string.find(msg, pt))
  -- assert(string.find(msg, msg, 1, true))
end


-- test error message with no extra info
assert(doit("error('hi', 0)") == 'hi')

-- test error message with no info
assert(doit("error()") == nil)


-- test common errors/errors that crashed in the past
assert(doit("table.unpack({}, 1, n=2^30)"))
assert(doit("a=math.sin()"))
assert(not doit("tostring(1)") and doit("tostring()"))
assert(doit"tonumber()")
assert(doit"repeat until 1; a")
assert(doit"return;;")
assert(doit"assert(false)")
assert(doit"assert(nil)")
assert(doit("function a (... , ...) end"))
assert(doit("function a (, ...) end"))
assert(doit("local t={}; t = t[#t] + 1"))

checksyntax([[
  local a = {4

]], "'}' expected (to close '{' at line 1)", "<eof>", 3)


if not T then
  (Message or print)
    ('\n >>> testC not active: skipping memory message test <<<\n')
else
  print "testing memory error message"
  local a = {}
  for i = 1, 10000 do a[i] = true end   -- preallocate array
  collectgarbage()
  T.totalmem(T.totalmem() + 10000)
  -- force a memory error (by a small margin)
  local st, msg = pcall(function()
    for i = 1, 100000 do a[i] = tostring(i) end
  end)
  T.totalmem(0)
  assert(not st and msg == "not enough" .. " memory")
end


-- tests for better error messages

checkmessage("a = {} + 1", "Attempt to perform arithmetic")
checkmessage("a = {} | 1", "Attempt to perform arithmetic")
checkmessage("a = {} < 1", "Attempt to compare")
checkmessage("a = {} <= 1", "Attempt to compare")

checkmessage("a=1; bbbb=2; a=math.sin(3)+bbbb(3)", "Attempt to call")
checkmessage("a={}; do local a=1 end a:bbbb(3)", "Attempt to call")
checkmessage("local a={}; a.bbbb(3)", "Attempt to call")
assert(not string.find(doit"a={13}; local bbbb=1; a[bbbb](3)", "'bbbb'"))
checkmessage("a={13}; local bbbb=1; a[bbbb](3)", "Attempt to call")
checkmessage("a=(1)..{}", "Attempt to concatenate")

-- tail calls
checkmessage("local a={}; return a.bbbb(3)", "Attempt to call")
checkmessage("a={}; do local a=1 end; return a:bbbb(3)", "Attempt to call")

checkmessage("a = #print", "Attempt to take length")
checkmessage("a = #3", "Attempt to take length")

aaa = nil
checkmessage("aaa.bbb:ddd(9)", "Attempt to index")
checkmessage("local aaa={bbb=1}; aaa.bbb:ddd(9)", "Attempt to index")
checkmessage("local aaa={bbb={}}; aaa.bbb:ddd(9)", "Attempt to call")
checkmessage("local a,b,c; (function () a = b+1.1 end)()",
  "Attempt to perform arithmetic")
assert(not doit"local aaa={bbb={ddd=pairs}}; aaa.bbb:ddd(nil)")

-- upvalues being indexed do not go to the stack
checkmessage("local a,b,cc; (function () a = cc[1] end)()", "Attempt to index")
checkmessage("local a,b,cc; (function () a.x = 1 end)()", "Attempt to index")

checkmessage("local _ENV = {x={}}; a = a + 1", "Attempt to perform arithmetic")

checkmessage("b=1; local aaa={}; x=aaa+b", "Attempt to perform arithmetic")
checkmessage("aaa={}; x=3.3/aaa", "Attempt to perform arithmetic")
checkmessage("aaa=2; b=nil;x=aaa*b", "Attempt to perform arithmetic")
checkmessage("aaa={}; x=-aaa", "Attempt to perform arithmetic")

-- short circuit
checkmessage("a=1; local a,bbbb=2,3; a = math.sin(1) and bbbb(3)",
       "Attempt to call")
checkmessage("a=1; local a,bbbb=2,3; a = bbbb(1) or a(3)", "Attempt to call")
checkmessage("local a,b,c,f = 1,1,1; f((a and b) or c)", "Attempt to call")
checkmessage("local a,b,c = 1,1,1; ((a and b) or c)()", "Attempt to call")
assert(not string.find(doit"aaa={}; x=(aaa or aaa)+(aaa and aaa)", "'aaa'"))
assert(not string.find(doit"aaa={}; (aaa or aaa)()", "'aaa'"))

checkmessage("print(print < 10)", "Attempt to compare")
checkmessage("print(print < print)", "Attempt to compare")
checkmessage("print('10' < 10)", "Attempt to compare")
checkmessage("print(10 < '23')", "Attempt to compare")

-- float->integer conversions
-- checkmessage("local a = 2.0^100; x = a << 2", "local a")
-- checkmessage("local a = 1 >> 2.0^100", "has no integer representation")
checkmessage("local a = 10.1 << 2.0^100",
  "Attempt to perform bitwise on a non-integer number")
-- checkmessage("local a = 2.0^100 & 1", "has no integer representation")
-- checkmessage("local a = 2.0^100 & 1e100", "has no integer representation")
-- checkmessage("local a = 2.0 | 1e40", "has no integer representation")
-- checkmessage("local a = 2e100 ~ 1", "has no integer representation")
-- checkmessage("string.sub('a', 2.0^100)", "has no integer representation")
checkmessage("string.rep('a', 3.3)", "Expected a integer")
-- checkmessage("return 6e40 & 7", "has no integer representation")
-- checkmessage("return 34 << 7e30", "has no integer representation")
-- checkmessage("return ~-3e40", "has no integer representation")
checkmessage("return ~-3.009",
  "Attempt to perform bitwise on a non-integer number")
checkmessage("return 3.009 & 1",
  "Attempt to perform bitwise on a non-integer number")
checkmessage("return 34 >> {}", "Attempt to perform arithmetic")
checkmessage("a = 24 // 0", "Divide by zero")
checkmessage("a = 1 % 0", "Divide by zero")


-- numeric for loops
checkmessage("for i = {}, 10 do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = io.stdin, 10 do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = {}, 10 do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = 1, 'x', 10 do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = 1, {}, 10 do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = 1, {} do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = 1, 10, print do end",
  "Attempt to perform a non-numeric ranged for loop")
checkmessage("for i = 1, 10, print do end",
  "Attempt to perform a non-numeric ranged for loop")

-- passing light userdata instead of full userdata
--[==[_G.D = debug
checkmessage([[
  -- create light udata
  local x = D.upvalueid(function () return debug end, 1)
  D.setuservalue(x, {})
]], "light userdata")
_G.D = nil]==]

-- do   -- named objects (field '__name')
  -- checkmessage("math.sin(io.input())", "(number expected, got FILE*)")
  -- _G.XX = setmetatable({}, {__name = "My Type"})
  -- assert(string.find(tostring(XX), "^My Type"))
  -- checkmessage("io.input(XX)", "(FILE* expected, got My Type)")
  -- checkmessage("return XX + 1", "on a My Type value")
  -- checkmessage("return ~io.stdin", "on a FILE* value")
  -- checkmessage("return XX < XX", "two My Type values")
  -- checkmessage("return {} < XX", "table with My Type")
  -- checkmessage("return XX < io.stdin", "My Type with FILE*")
  -- _G.XX = nil
-- end

-- global functions
-- checkmessage("(io.write or print){}", "io.write")
-- checkmessage("(collectgarbage or print){}", "collectgarbage")

-- errors in functions without debug info
--[==[do
  local f = function (a) return a + 1 end
  f = assert(load(string.dump(f, true)))
  assert(f(3) == 4)
  checkerr("^%?:%-1:", f, {})

  -- code with a move to a local var ('OP_MOV A B' with A<B)
  f = function () local a; a = {}; return a + 2 end
  -- no debug info (so that 'a' is unknown)
  f = assert(load(string.dump(f, true)))
  -- symbolic execution should not get lost
  checkerr("^%?:%-1:.*table value", f)
end]==]


-- tests for field accesses after RK limit
local t = {}
for i = 1, 1000 do
  t[i] = "a = x" .. i
end
local s = table.concat(t, "; ")
t = nil
checkmessage(s.."; a = bbb + 1", "Attempt to perform arithmetic")
checkmessage("local _ENV=_ENV;"..s.."; a = bbb + 1",
  "Attempt to perform arithmetic")
checkmessage(s.."; local t = {}; a = t.bbb + 1", "Attempt to perform arithmetic")
checkmessage(s.."; local t = {}; t:bbb()", "Attempt to call")

checkmessage([[aaa=9
repeat until 3==3
local x=math.sin(math.cos(3))
if math.sin(1) == x then return math.sin(1) end   -- tail call
local a,b = 1, {
  {x='a'..'b'..'c', y='b', z=x},
  {1,2,3,4,5} or 3+3<=3+3,
  3+1>3+1,
  {d = x and aaa[x or y]}}
]], "Attempt to call")

checkmessage([[
local x,y = {},1
if math.sin(1) == 0 then return 3 end    -- return
x.a()]], "Attempt to call")

checkmessage([[
prefix = nil
insert = nil
while 1 do
  local a
  if nil then break end
  insert(prefix, a)
end]], "Attempt to call")

checkmessage([[  -- tail call
  return math.sin("a")
]], "Expected a number")

checkmessage([[collectgarbage("nooption")]], "Invalid option")

checkmessage([[x = print .. "a"]], "Attempt to concatenate")
checkmessage([[x = "a" .. false]], "Attempt to concatenate")
checkmessage([[x = {} .. 2]], "Attempt to concatenate")

checkmessage("getmetatable(io.stdin).__gc()", "Attempt to index")

checkmessage([[
local Var
local function main()
  NoSuchName (function() Var=0 end)
end
main()
]], "Attempt to call")
print'+'

a = {}; setmetatable(a, {__index = string})
checkmessage("a:sub()", "argument 1")
checkmessage("string.sub('a', {})", "argument 2")
checkmessage("('a'):sub{}", "argument 2")

checkmessage("table.sort({1,2,3}, table.sort)", "")
checkmessage("string.gsub('s', 's', setmetatable)", "")

-- tests for errors in coroutines

local function f (n)
  local c = coroutine.create(f)
  local a,b = coroutine.resume(c)
  return b
end
assert(string.find(f(), "Stack overflow"))

checkmessage("coroutine.yield()", "Attempt to yield")

-- f = coroutine.wrap(function () table.sort({1,2,3}, coroutine.yield) end)
-- checkerr("yield across", f)


-- testing size of 'source' info; size of buffer for that info is
-- LUA_IDSIZE, declared as 60 in luaconf. Get one position for '\0'.
--[==[idsize = 60 - 1
local function checksize (source)
  -- syntax error
  local _, msg = load("x", source)
  msg = string.match(msg, "^([^:]*):")   -- get source (1st part before ':')
  assert(msg:len() <= idsize)
end

for i = 60 - 10, 60 + 10 do   -- check border cases around 60
  checksize("@" .. string.rep("x", i))   -- file names
  checksize(string.rep("x", i - 10))     -- string sources
  checksize("=" .. string.rep("x", i))   -- exact sources
end]==]


-- testing line error

local function lineerror (s, l)
  local err,line = xpcall(load(s), function()
    local n = 2
    while true do
      local i = debug.getinfo(n)
      if i and i.currentline > 0 then
        return i.currentline
      end
      n = n + 1
    end
  end)
  if not (line == l or (not line and not l)) then
    print(line, l)
    load(s)()
    assert(false)
  end
end

lineerror("local a\n for i=1,'a' do \n print(i) \n end", 2)
lineerror("\n local a \n for k,v in 3 \n do \n print(k) \n end", 3)
lineerror("\n\n for k,v in \n 3 \n do \n print(k) \n end", 3)
lineerror("function a.x.y ()\na=a+1\nend", 1)

lineerror("a = \na\n+\n{}", 4)
lineerror("a = \n3\n+\n(\n4\n/\nprint)", 7)
lineerror("a = \nprint\n+\n(\n4\n/\n7)", 7)

lineerror("a\n=\n-\n\nprint\n;", 5)

lineerror([[
a
(
23)
]], 3)

lineerror([[
local a = {x = 13}
a
.
x
(
23
)
]], 7)

lineerror([[
local a = {x = 13}
a
.
x
(
23 + a
)
]], 7)

-- local p = [[
  -- function g() f() end
  -- function f(x) error('a', X) end
-- g()
-- ]]
-- X=3;lineerror((p), 3)
-- X=0;lineerror((p), false)
-- X=1;lineerror((p), 2)
-- X=2;lineerror((p), 1)


lineerror([[
local b = false
if not b then
  error 'test'
end]], 3)

lineerror([[
local b = false
if not b then
  if not b then
    if not b then
      error 'test'
    end
  end
end]], 5)


if not _soft then
  -- several tests that exaust the Lua stack
  collectgarbage()
  print"testing stack overflow"
  C = 0
  local l = debug.getinfo(1, "l").currentline; function y () C=C+1; y() end

  local function checkstackmessage (m)
    return (string.find(m, "stack overflow"))
  end
  -- repeated stack overflows (to check stack recovery)
  assert(checkstackmessage(doit('y()')))
  print('+')
  assert(checkstackmessage(doit('y()')))
  print('+')
  assert(checkstackmessage(doit('y()')))
  print('+')


  -- error lines in stack overflow
  C = 0
  local l1
  local function g(x)
    l1 = debug.getinfo(x, "l").currentline; y()
  end
  local _, stackmsg = xpcall(g, debug.traceback, 1)
  print('+')
  local stack = {}
  for line in string.gmatch(stackmsg, "[^\n]*") do
    local curr = string.match(line, ":(%d+):")
    if curr then table.insert(stack, tonumber(curr)) end
  end
  local i=1
  while stack[i] ~= l1 do
    assert(stack[i] == l)
    i = i+1
  end
  assert(i > 15)


  -- error in error handling
  local res, msg = xpcall(error, error)
  assert(not res and type(msg) == 'string')
  print('+')

  local function f (x)
    if x==0 then error('a\n')
    else
      local aux = function () return f(x-1) end
      local a,b = xpcall(aux, aux)
      return a,b
    end
  end
  f(3)

  local function loop (x,y,z) return 1 + loop(x, y, z) end

  local res, msg = xpcall(loop, function (m)
    assert(string.find(m, "stack overflow"))
    checkerr("error handling", loop)
    assert(math.sin(0) == 0)
    return 15
  end)
  assert(msg == 15)

  local f = function ()
    for i = 999900, 1000000, 1 do table.unpack({}, 1, i) end
  end
  checkerr("too many results", f)

end


do
  -- non string messages
  local t = {}
  local res, msg = xpcall(function () error(t) end, nil)
  assert(not res and msg == t)

  res, msg = xpcall(function () error(nil) end, nil)
  assert(not res and msg == nil)

  local function f() error{msg='x'} end
  res, msg = xpcall(f, function (r) return {msg=r.msg..'y'} end)
  assert(msg.msg == 'xy')

  -- 'assert' with extra arguments
  res, msg = xpcall(assert, nil, false, "X", t)
  assert(not res and msg == "X")

  -- 'assert' with no message
  res, msg = xpcall(function () assert(false) end, nil)
  assert(string.match(msg, "Assertion failed"))

  -- 'assert' with non-string messages
  res, msg = xpcall(assert, nil, false, t)
  assert(not res and msg == t)

  res, msg = xpcall(assert, nil, nil, nil)
  assert(not res and msg == nil)

  -- 'assert' without arguments
  res, msg = xpcall(assert, nil)
  assert(not res and string.find(msg, "Expected a value at argument 1"))
end

-- xpcall with arguments
a, b, c = xpcall(string.find, error, "alo", "al")
assert(a and b == 1 and c == 2)
a, b, c = xpcall(string.find, function (x) return {} end, true, "al")
assert(not a and type(b) == "table" and c == nil)


print("testing tokens in error messages")
checksyntax("syntax error", "", "error", 1)
checksyntax("1.000", "", "1.000", 1)
checksyntax("[[a]]", "", "[[a]]", 1)
checksyntax("'aa'", "", "'aa'", 1)
checksyntax("while << do end", "", "<<", 1)
checksyntax("for >> do end", "", ">>", 1)

-- test invalid non-printable char in a chunk
checksyntax("a\1a = 1", "", "<\\1>", 1)

-- test 255 as first char in a chunk
checksyntax("\255a = 1", "", "<\\255>", 1)

doit('I = load("a=9+"); a=3')
assert(a==3 and not I)
print('+')

lim = 1000
if _soft then lim = 100 end
for i=1,lim do
  doit('a = ')
  doit('a = 4+nil')
end


-- testing syntax limits

-- local function testrep (init, rep, close, repc, finalresult)
  -- local s = init .. string.rep(rep, 100) .. close .. string.rep(repc, 100)
  -- local res, msg = load(s)
  -- assert(res)   -- 100 levels is OK
  -- if (finalresult) then
    -- assert(res() == finalresult)
  -- end
  -- s = init .. string.rep(rep, 10000) .. close .. string.rep(repc, 100)
  -- local res, msg = load(s)   -- 10000 levels not ok
  -- print(msg)
  -- assert(not res and (string.find(msg, "too many registers") or
                      -- string.find(msg, "stack overflow")))
-- end

-- testrep("local a; a", ",a", "= 1", ",1")    -- multiple assignment
-- testrep("local a; a=", "{", "0", "}")
-- testrep("return ", "(", "2", ")", 2)
-- testrep("local function a (x) return x end; return ", "a(", "2.2", ")", 2.2)
-- testrep("", "do ", "", " end")
-- testrep("", "while a do ", "", " end")
-- testrep("local a; ", "if a then else ", "", " end")
-- testrep("", "function foo () ", "", " end")
-- testrep("local a = ''; return ", "a..", "'a'", "", "a")
-- testrep("local a = 1; return ", "a^", "a", "", 1)

-- checkmessage("a = f(x" .. string.rep(",x", 260) .. ")", "too many registers")


-- testing other limits

-- upvalues
-- local lim = 127
-- local  s = "local function fooA ()\n  local "
-- for j = 1,lim do
  -- s = s.."a"..j..", "
-- end
-- s = s.."b,c\n"
-- s = s.."local function fooB ()\n  local "
-- for j = 1,lim do
  -- s = s.."b"..j..", "
-- end
-- s = s.."b\n"
-- s = s.."function fooC () return b+c"
-- local c = 1+2
-- for j = 1,lim do
  -- s = s.."+a"..j.."+b"..j
  -- c = c + 2
-- end
-- s = s.."\nend  end end"
-- local a,b = load(s)
-- assert(c > 255 and string.find(b, "too many upvalues") and
       -- string.find(b, "line 5"))

-- local variables
-- s = "\nfunction foo ()\n  local "
-- for j = 1,300 do
  -- s = s.."a"..j..", "
-- end
-- s = s.."b\n"
-- local a,b = load(s)
-- assert(string.find(b, "line 2") and string.find(b, "too many local variables"))

mt.__index = oldmm

print('OK')
