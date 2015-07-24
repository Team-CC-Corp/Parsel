-- TABLE UTIL

local function concat(a, b)
    local concatted = {}

    for i,v in ipairs(a) do
        concatted[i] = v
    end

    for i,v in ipairs(b) do
        table.insert(concatted, v)
    end

    return concatted
end

local function tail(t)
    local tl = {}
    for i=2,#t do
        tl[i - 1] = t[i]
    end
    return tl
end

local function replicate(n, x)
    local t = {}
    for i=1, n do
        table.insert(t, x)
    end
    return t
end

-- ERROR UTIL

local function stackError(msg, level)
    if type(level) ~= "number" or level == 0 then
        level = 1
    end

    local errors = {}
    local lastMsg
    while lastMsg ~= msg do
        local ok
        ok, lastMsg = pcall(error, msg, level)
        level = level + 1
        if lastMsg ~= msg then
            if #errors == 0 then
                table.insert(errors, lastMsg)
            else
                table.insert(errors, " at: " .. lastMsg:sub(1, -3 -#msg))
            end
        end
    end
    if #errors >= 16 then
        errors[16] = "..."
        while table.remove(errors, 17) do end
    end
    error(table.concat(errors, "\n"), 0)
end
local function stackAssert(cond, msg, level)
    if type(level) ~= "number" or level == 0 then
        level = 1
    end

    if cond then
        return cond
    else
        return stackError(msg or "Assertion failed!", level + 1)
    end
end

-- PARSER TYPE

Parser = {}

function new(f)
    local function f1(s)
        local ok, a, cs = f(stackAssert(s, "Nil string"))
        stackAssert(cs, "Nil suffix")
        return ok, a, cs
    end
    return setmetatable({runParser=f1}, {__index=Parser})
end

function Parser:bind(f)
    return new(function(s)
        local ok, a, cs = self.runParser(s)
        if ok then
            return f(a).runParser(cs)
        else
            return ok, a, cs
        end
    end)
end

function Parser:discardBind(p)
    return self:bind(function() return p end)
end

function Parser:fmap(f)
    return self:bind(function(a)
        return from(f(a))
    end)
end

function Parser:expect(str)
    return new(function(s)
        local ok, a, cs = self.runParser(s)
        if not ok then
            return ok, {str}, cs
        else
            return ok, a, cs
        end
    end)
end

function Parser:try()
    return new(function(s)
        local ok, a, cs = self.runParser(s)
        if not ok then
            return ok, a, s
        else
            return ok, a, cs
        end
    end)
end

function Parser:lookahead()
    return new(function(s)
        local ok, a, cs = self.runParser(s)
        if not ok then
            return ok, a, cs
        else
            return ok, a, s
        end
    end)
end

function Parser:apply(s)
    stackAssert(s, "Nil apply string")
    return spaces:discardBind(self).runParser(s)
end

function Parser:otherwise(b)
    return new(function(s)
        local ok, a, cs = self.runParser(s)
        -- return if ok, or error if input was consumed
        if (not ok) and cs == s then
            local firstError = a
            ok, a, cs = b.runParser(s)
            if not ok and cs == s then
                return ok, concat(firstError, a), cs
            else
                return ok, a, cs
            end
        else
            return ok, a, cs
        end
    end)
end

function Parser:many()
    return self:many1():otherwise(from({}))
end

function Parser:many1()
    return self:bind(function(a)
        return self:many():bind(function(as)
            return from(concat({a}, as))
        end)
    end)
end

function Parser:skipMany()
    return self:many():discardBind(from(nil))
end

function Parser:sepBy(sep)
    return self:sepBy1(sep):otherwise(from({}))
end

function Parser:sepBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(self):many():bind(function(as)
            return from(concat({a}, as))
        end)
    end)
end

function Parser:chainl(op, x)
    return self:chainl1(op):otherwise(from(x))
end

function Parser:chainl1(op)
    local function rest(a)
        return op:bind(function(f)
            return self:bind(function(b)
                return rest(f(a, b))
            end)
        end):otherwise(from(a))
    end
    return self:bind(rest)
end

function Parser:chainr(op, x)
    return self:chainr1(op):otherwise(from(x))
end

function Parser:chainr1(op)
    local function rest(a)
        return op:bind(function(f)
            return self:bind(rest):bind(function(b)
                return from(f(a, b))
            end)
        end):otherwise(from(a))
    end
    return self:bind(rest)
end

function Parser:token()
    return self:bind(function(a)
        return spaces:discardBind(from(a))
    end)
end

function Parser:count(n)
    return sequence(replicate(n, self))
end

function Parser:between(start, stop)
    return start:discardBind(self:bind(function(a)
        return stop:discardBind(from(a))
    end))
end

-- CONSTRUCTORS

function fail(str)
    return zero:expect(str)
end

function from(a)
    return new(function(s) return true, a, s end)
end

-- CHAR

function satisfy(f)
    return item:bind(function(c)
        if f(c) then
            return from(c)
        else
            return zero
        end
    end):try()
end

function char(c)
    return satisfy(function(c1) return c == c1 end):expect(c)
end

function string(s)
    if s == "" then
        return from("")
    else
        return char(s:sub(1,1))
            :discardBind(string(s:sub(2))
            :discardBind(from(s)))
            :try()
            :expect(s)
    end
end

function oneOf(s)
    return satisfy(function(c)
        return s:find(c, 1, true) ~= nil
    end)
end

function noneOf(s)
    return satisfy(function(c)
        return s:find(c, 1, true) == nil
    end)
end

space = satisfy(function(c) return c:find"%s" ~= nil end)
spaces = space:many()

newLine = char"\n"
crlf = string"\r\n"
endOfLine = newLine:otherwise(crlf)

tab = char"\t"

upper = satisfy(function(c) return (c:find"%a" ~= nil) and c == c:upper() end)
lower = satisfy(function(c) return (c:find"%a" ~= nil) and c == c:lower() end)

-- OTHER

function symbol(s)
    return string(s):token()
end

function choice(list)
    local p = list[1]
    if not p then
        return fail("No choices")
    end

    for i=2, #list do
        p = p:otherwise(list[i])
    end

    return p
end

function sequence(list)
    if #list == 0 then
        return from({})
    else
        return list[1]:bind(function(a)
            return sequence(tail(list)):bind(function(as)
                return from(concat({a}, as))
            end)
        end)
    end
end

-- SIMPLE PARSERS

zero = new(function(s) return false, {"Error"}, s end)

item = new(function(s)
    if s == "" then
        return false, {"Unexpected EOF"}, s
    else
        return true, s:sub(1,1), s:sub(2)
    end
end)