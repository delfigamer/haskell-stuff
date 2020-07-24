-- bisect.lua
-- bisection method for solving non-linear equations

-- tolerance
local delta=1e-100

function bisect(f, a, b, fa, fb)
    local c = (a + b) / 2
    io.write(n, " c=", c, " a=", a, " b=", b, "\n")
    if c == a or c == b or math.abs(a - b) < delta then
        return c, b - a
    end
    n = n + 1
    local fc = f(c)
    if fa * fc < 0 then
        return bisect(f, a, c, fa, fc)
    else
        return bisect(f, c, b, fc, fb)
    end
end

-- find root of f in the inverval [a,b]. needs f(a)*f(b)<0
local function solve(f, a, b)
    n = 0
    local z, e = bisect(f, a, b, f(a), f(b))
    io.write(
        "after ", n, " steps, root is ", z, " with error ", e,
        ", f=", f(z), "\n")
end

-- our function
local function f(x)
    return x*x*x - x - 1
end

-- find zero in [1,2]
solve(f, math.exact(1), math.exact(2))
