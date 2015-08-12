-- local fh = fs.open("parsel.log", "w")

-- TABLE UTIL

local function concat(a, b)
    if not b then
        -- Allow currying
        return function(b)
            return concat(a, b)
        end
    end

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

local function foldl(f, accum, t)
    for i,v in ipairs(t) do
        accum = f(accum, v)
    end
    return accum
end

local function foldr(f, accum, t)
    for i=#t,1,-1 do
        accum = f(t[i], accum)
    end
    return accum
end

local function contains(t, x)
    for k,v in pairs(t) do
        if v == x then
            return true
        end
    end
    return false
end

-- FUNCTION UTIL

local function id(...)
    return ...
end

function thunk(f, name)
    local run = false
    local a
    return function()
        if not run then
            -- print((showStack("Running thunk: " .. name, 1):gsub("\n.*", "")))
            a = f()
            run = true
        end
        return a
    end
end

-- Algebraic Data Type util


--[[
Use this function to make ADTs.
For example, take this haskell data

    data Constant = Nil | I(Int) | S(String) | APlusB(A, B)

The same thing using this looks like:

    Constant = {
        Nil = cons(),
        I = cons(1),
        S = cons(1),
        APlusB = cons(2)
    }

To use it,

    x = Constant.I(123)
    x = Constant.APlusB(a)(b) -- IS CURRIED!!

    if x.cons() == Constant.Nil then
        print("x is nil")
    elseif x.cons() == Constant.I then
        print("x is num: ", x.get())
    elseif x.cons() == Constant.S then
        print("x is string: ", x.get())
    elseif x.cons() == Constant.APlusB then
        local a, b = x.get()
        print("x is A and B: ", a, " : ", b)
    end
]]
function cons(n)
    local f
    local function makeF(n, ...)
        if n <= 0 then
            local args = {...}
            return {
                cons = function()
                    return f
                end,
                get = function()
                    return unpack(args)
                end
            }
        else
            local args = {...}
            return function(...)
                local newArgs = {...}
                return makeF(n - #newArgs, unpack(concat(args, newArgs)))
            end
        end
    end
    f = makeF(n or 0)
    return f
end

-- CONSTANTS UTIL
-- Constants can't be declared before the methods they use
-- But for organizational purposes, this is often necessary

local constantInitializers = {}
local function constants(f)
    table.insert(constantInitializers, f)
end

local function runConstants()
    for f in function() return table.remove(constantInitializers, 1) end do
        f()
    end
end

-- ERROR UTIL

function getStack(msg, level)
    if type(level) ~= "number" or level == 0 then
        level = 1
    end
    level = level + 2

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
                table.insert(errors, (lastMsg:sub(1, -3 -#msg):gsub("^pcall: ", "")))
            end
        end
    end

    return errors
end

function showStack(msg, level)
    if type(level) ~= "number" or level == 0 then
        level = 1
    end
    local errors = getStack(msg, level + 1)

    if #errors >= 16 then
        errors[16] = "..."
        while table.remove(errors, 17) do end
    end

    return table.concat(errors, "\n at: ")
end

function stackError(msg, level)
    if type(level) ~= "number" or level == 0 then
        level = 1
    end

    error(showStack(msg, level + 1), 0)
end
function stackAssert(cond, msg, level)
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
    local stack = getStack("", 2)
    local function f1(s)
        local show = s:gsub("\n.*", "")
        fh.writeLine(stack[1] .. "stack depth: " .. #getStack("",1) .. ": " .. show)
        fh.flush()
        return f(s)
    end
    return setmetatable({runParser=f1}, {__index=Parser})
end

-- CHAR

function satisfy(f)
    local stack = showStack("Failed satisfy", 1)
    return anyChar:bind(function(c)
        if f(c) then
            return from(c)
        else
            return zero:expect(stack)
        end
    end):try()
end

function char(c)
    stackAssert(type(c) == "string" and #c == 1, "Expected character")
    return satisfy(function(c1) return c == c1 end):expect(c)
end

function string(str)
    stackAssert(type(str) == "string", "Expected string")
    return new(function(s)
        if s:sub(1, #str):find(str, 1, true) then
            return true, str, s:sub(#str + 1), false
        else
            return false, {str}, s, false
        end
    end)
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

constants(function()
    anyChar = new(function(s)
        if s == "" then
            return false, {"EOF"}, s, true
        else
            return true, s:sub(1,1), s:sub(2), false
        end
    end)
    
    space = satisfy(function(c) return c:find"%s" ~= nil end)
    spaces = space:many()

    newLine = char"\n"
    crlf = string"\r\n"
    endOfLine = newLine:otherwise(crlf)

    tab = char"\t"

    upper = satisfy(function(c) return (c:find"%a" ~= nil) and c == c:upper() end)
    lower = satisfy(function(c) return (c:find"%a" ~= nil) and c == c:lower() end)

    alphaNum = satisfy(function(c) return c:find"%w" ~= nil end)
    letter = satisfy(function(c) return c:find"%a" ~= nil end)
    digit = satisfy(function(c) return c:find"%d" ~= nil end)
    hexDigit = satisfy(function(c) return c:find"%x" ~= nil end)
    octalDigit = satisfy(function(c) return c:find"[0-7]" ~= nil end)
end)

-- COMBINATOR

function choice(list)
    if #list == 0 then
        return fail("No choices")
    end

    local x = new(function(s)
        local ok, a, cs, unexp
        for i,p in ipairs(list) do
            ok, a, cs, unexp = p.runParser(s)
            if ok or s ~= cs then
                return ok, a, cs, unexp
            end
        end
        return ok, a, cs, unexp
    end)
    return x
end

function Parser:count(n)
    return sequence(replicate(n, self))
end

function Parser:between(start, stop)
    return start:discardBind(self:bind(function(a)
        return stop:discardBind(from(a))
    end))
end

function Parser:option(a)
    return self:otherwise(from(a))
end

function Parser:optionMaybe()
    return self:otherwise(from(nil))
end

function Parser:optional()
    return self:discardBind(from(nil)):otherwise(from(nil))
end

function Parser:skipMany1()
    return self:many1():discardBind(from(nil))
end

function Parser:many1()
    return self:bind(function(a)
        return self:many():fmap(concat({a}))
    end)
end

function Parser:sepBy(sep)
    return self:sepBy1(sep):otherwise(from({}))
end

function Parser:sepBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(self):many():fmap(concat({a}))
    end)
end

function Parser:endBy(sep)
    return self:endBy1(sep):otherwise(from({}))
end

function Parser:endBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(from(a))
    end):many1()
end

function Parser:sepEndBy(sep)
    return self:sepEndBy1(sep):otherwise(from({}))
end

function Parser:sepEndBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(self:sepEndBy(sep)):fmap(concat({a})):otherwise(from({a}))
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
            return self:bind(rest):fmap(function(b)
                return f(a, b)
            end)
        end):otherwise(from(a))
    end
    return self:bind(rest)
end

constants(function()
    eof = anyChar:notFollowedBy():expect"end of input"
end)

function Parser:notFollowedBy()
    return self:bind(function(c)
        return unexpected(tostring(c))
    end)
    :otherwise(from(nil))
    :try()
end

function Parser:manyTill(ending)
    local function scan()
        return ending:discardBind(from({})):otherwise(self:bind(function(a)
            return scan():fmap(concat({a}))
        end))
    end
    return scan()
end

function Parser:lookahead()
    return new(function(s)
        local ok, a, cs, unexp = self.runParser(s)
        if not ok then
            return ok, a, cs, unexp
        else
            return ok, a, s, unexp
        end
    end)
end

-- PRIMITIVE

function Parser:apply(s)
    stackAssert(s, "Nil apply string")
    return spaces:discardBind(self):bind(function(a)
        return eof:discardBind(from(a))
    end).runParser(s)
end

function Parser:parse(s, sourceName)
    if type(sourceName) ~= "string" then
        sourceName = "string"
    end

    local ok, a, cs, unexp = self:apply(s)

    if not ok then
        local consumedInput = s:sub(1, -(#cs + 1))
        local _, linesConsumed = consumedInput:gsub("\n", "")
        local lineNumber = linesConsumed + 1

        local near = cs:gsub("%s*(%S+)(.*)", "%1")
        if near == "" then near = "End of input" end
        local errMsg = (unexp and "Unexpected: " or "Expected: ")
            .. table.concat(a, ", ")
            .. "\n  at: " .. sourceName .. ":" .. lineNumber
            .. "\n  near: " .. near

        return false, errMsg
    else
        return ok, a
    end
end

function Parser:bind(f)
    return new(function(s)
        local ok, a, cs, unexp = self.runParser(s)
        if ok then
            return f(a).runParser(cs)
        else
            return ok, a, cs, unexp
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

function fail(str)
    return zero:expect(str)
end

function unexpected(str)
    return new(function(s)
        return false, {str}, s, true
    end)
end

function from(a)
    return new(function(s) return true, a, s, false end)
end

-- Use to eliminate unfortunate recursive thunks, since Lua isn't lazy
function fromThunk(f)
    return new(function(s)
        return f().runParser(s)
    end)
end

function Parser:expect(str)
    return new(function(s)
        local ok, a, cs, unexp = self.runParser(s)
        if not ok then
            return ok, {str}, cs, false
        else
            return ok, a, cs, unexp
        end
    end)
end

function Parser:try()
    return new(function(s)
        local ok, a, cs, unexp = self.runParser(s)
        if not ok then
            return ok, a, s, unexp
        else
            return ok, a, cs, unexp
        end
    end)
end

function Parser:otherwise(b)
    return new(function(s)
        local ok, a, cs, unexp = self.runParser(s)
        -- return if ok, or error if input was consumed
        if (not ok) and cs == s then
            local firstError = a
            ok, a, cs, unexp = b.runParser(s)
            if not ok and cs == s then
                return ok, concat(firstError, a), cs, unexp
            else
                return ok, a, cs, unexp
            end
        else
            return ok, a, cs, unexp
        end
    end)
end

constants(function()
    zero = new(function(s) return false, {"Error"}, s, false end)
end)

function Parser:many()
    return new(function(s)
        local result = {}

        while true do
            local ok, a, cs, unexp = self.runParser(s)

            if ok then
                table.insert(result, a)
            elseif s == cs then -- no input consumed. all good
                return true, result, cs, unexp
            else -- input consumed. report the error
                return ok, a, cs, unexp
            end

            s = cs
        end
    end)
end

function Parser:skipMany()
    return self:many():discardBind(from(nil))
end

-- EXPRESSION

Assoc = {
    None = cons(),
    Left = cons(),
    Right= cons()
}

Operator = {
    Infix = cons(2), -- parser, assoc
    Prefix = cons(1), -- parser
    Postfix = cons(1), -- parser
}

function Parser:buildExpressionParser(operators)
    local function splitOp(op, opTypesList)
        local rassocOp, lassocOp, nassocOp, prefixOp, postfixOp = unpack(opTypesList)

        if op.cons() == Operator.Infix then
            local parser, assoc = op.get()
            if assoc == Assoc.None then
                nassocOp = concat({parser}, nassocOp)
            elseif assoc == Assoc.Left then
                lassocOp = concat({parser}, lassocOp)
            elseif assoc == Assoc.Right then
                rassocOp = concat({parser}, rassocOp)
            end
        elseif op.cons() == Operator.Prefix then
            prefixOp = concat({op.get()}, prefixOp)
        elseif op.cons() == Operator.Postfix then
            postfixOp = concat({op.get()}, postfixOp)
        end

        return {rassocOp, lassocOp, nassocOp, prefixOp, postfixOp}
    end

    local function ambigious(assoc, op)
        return op:discardBind(fail("ambigious use of an operator")):try()
    end

    local function makeParser(term, ops)
        local rassoc, lassoc, nassoc, prefix, postfix = unpack(foldr(splitOp, {{}, {}, {}, {}, {}}, ops))
        local rassocOp = choice(rassoc)
        local lassocOp = choice(lassoc)
        local nassocOp = choice(nassoc)
        local prefixOp = choice(prefix)
        local postfixOp = choice(postfix)

        local ambigiousRight = ambigious(Assoc.Right, rassocOp)
        local ambigiousLeft = ambigious(Assoc.Left, lassocOp)
        local ambigiousNon = ambigious(Assoc.None, nassocOp)

        local termP = prefixOp:many():bind(function(pres)
            local fpre = foldr(function(f, accum) return function(x) return f(accum(x)) end end, id, pres)

            return term:bind(function(a)
                return postfixOp:many():bind(function(posts)
                    local fpost = foldl(function(accum, f) return function(x) return accum(f(x)) end end, id, posts)

                    return from(fpost(fpre(a)))
                end)
            end)
        end)

        local rassocP, rassocP1
        function rassocP(x)
            return rassocOp:bind(function(f)
                return termP:bind(rassocP1):bind(function(y)
                    return from(f(x, y))
                end)
            end)
            :otherwise(ambigiousLeft)
            :otherwise(ambigiousNon)
        end

        function rassocP1(x)
            return rassocP(x):otherwise(from(x))
        end

        local lassocP, lassocP1
        function lassocP(x)
            return lassocOp:bind(function(f)
                return termP:bind(function(y)
                    return lassocP1(f(x, y))
                end)
            end)
            :otherwise(ambigiousRight)
            :otherwise(ambigiousNon)
        end

        function lassocP1(x)
            return lassocP(x):otherwise(from(x))
        end

        local function nassocP(x)
            return nassocOp:bind(function(f)
                return termP:bind(function(y)
                    return ambigiousRight
                        :otherwise(ambigiousLeft)
                        :otherwise(ambigiousNon)
                        :otherwise(from(f(x, y)))
                end)
            end)
        end

        return termP:bind(function(x)
            return rassocP(x)
                :otherwise(lassocP(x))
                :otherwise(nassocP(x))
                :otherwise(from(x))
                :expect"operator"
        end)
    end
    return foldl(makeParser, self, operators)
end

-- TOKEN
--[[
LanguageDef {
    comment        :: Parser,

    nestedComments :: Bool,

    identStart     :: Parser,

    identLetter    :: Parser,

    opStart        :: Parser,

    opLetter       :: Parser,

    reservedNames  :: [String],

    reservedOpNames:: [String],

    caseSensitive  :: Bool
}
]]

function makeTokenParser(languageDef)
    local tokenParser = {}

    ----------------------------------------------------
    -- Brackets
    ----------------------------------------------------

    function tokenParser:parens(p)
        return p:between(self:symbol"(", self:symbol")")
    end

    function tokenParser:braces(p)
        return p:between(self:symbol"{", self:symbol"}")
    end

    function tokenParser:angles(p)
        return p:between(self:symbol"<", self:symbol">")
    end

    function tokenParser:brackets(p)
        return p:between(self:symbol"[", self:symbol"]")
    end

    ----------------------------------------------------
    -- Comma and semi sep
    ----------------------------------------------------

    function tokenParser:commaSep(p)
        return p:sepBy(tokenParser.comma)
    end

    function tokenParser:semiSep(p)
        return p:sepBy(tokenParser.semi)
    end

    function tokenParser:commaSep1(p)
        return p:sepBy1(tokenParser.comma)
    end

    function tokenParser:semiSep1(p)
        return p:sepBy1(tokenParser.semi)
    end

    constants(function()
        tokenParser.semi = tokenParser:symbol";"
        tokenParser.comma = tokenParser:symbol","
        tokenParser.dot = tokenParser:symbol"."
        tokenParser.colon = tokenParser:symbol":"
    end)

    ----------------------------------------------------
    -- Chars and strings
    ----------------------------------------------------

    -- Escape
    local escMap = {
        ["a"] = "\a",
        ["b"] = "\b",
        ["f"] = "\f",
        ["n"] = "\n",
        ["r"] = "\r",
        ["t"] = "\t",
        ["v"] = "\v"
    }

    local escapeCode = anyChar:fmap(function(c)
        return escMap[c] or c
    end):expect"escape code"

    -- Chars

    local function charLetter(quote)
        return satisfy(function(c) return c ~= quote and c ~= "\\" and c:byte() > 26 end)
    end

    local charEscape = char("\\"):discardBind(escapeCode)

    local function characterChar(quote)
        return charLetter(quote):otherwise(charEscape):expect"character"
    end

    function tokenParser:charLiteral(quote)
        return self:lexeme(characterChar(quote)
                            :between(char(quote), char(quote):expect"end of character")
                            :expect"character literal")
    end

    -- Strings

    function tokenParser:stringLiteral(quote)
        return self:lexeme(characterChar(quote)
                            :many()
                            :between(char(quote), char(quote):expect"end of string")
                            :expect"string literal")
    end

    ----------------------------------------------------
    -- Numbers
    ----------------------------------------------------

    constants(function()
        local function number(base, baseDigit)
            return baseDigit:many1():fmap(function(digits)
                return foldl(function(x, d) return base * x + tonumber(d, base) end, 0, digits)
            end)
        end

        local sign = char"-":discardBind(from(function(x) return -x end))
                        :otherwise(from(id))

        local decimal = number(10, digit)
        local hexadecimal = oneOf"xX":discardBind(number(16, hexDigit))
        local octal = oneOf"oO":discardBind(number(8, octalDigit))

        local exponent = oneOf"eE":discardBind(sign:bind(function(f)
            return decimal:expect"exponent":fmap(function(e)
                return 10^f(e)
            end)
        end)):expect"exponent"

        local fraction = char".":discardBind(digit:many1():expect"fraction":fmap(function(digits)
            local function op(d, f)
                return (f + d) / 10
            end
            return foldr(op, 0, digits)
        end)):expect"fraction"

        local function fractExponent(n)
            return fraction:bind(function(fract)
                return exponent:option(1):fmap(function(expo)
                    return (n + fract) * expo
                end)
            end)
            :otherwise(exponent:fmap(function(expo)
                return n * expo
            end))
        end

        local zeroNumber = char"0":discardBind(
            hexadecimal
            :otherwise(octal)
            :otherwise(decimal)
            :otherwise(from(0))
        )

        local floating = decimal:bind(fractExponent)
        local nat = zeroNumber:otherwise(decimal)
        local int = tokenParser:lexeme(sign):bind(function(f)
            return nat:fmap(f)
        end)

        tokenParser.float = tokenParser:lexeme(floating):expect"floating"
        tokenParser.integer = tokenParser:lexeme(int):expect"integer"
        tokenParser.natural = tokenParser:lexeme(nat):expect"natural"
        tokenParser.decimal = decimal
        tokenParser.hexadecimal = hexadecimal
        tokenParser.octal = octal
    end)

    ----------------------------------------------------
    -- Operator
    ----------------------------------------------------

    function tokenParser:reservedOp(name)
        return self:lexeme(
            string(name)
            :discardBind(
                languageDef.opLetter
                :notFollowedBy()
                :expect("end of " .. name)
            )
            :try()
        )
    end

    constants(function()
        local oper = languageDef.opStart:bind(function(c)
            return languageDef.opLetter:many():fmap(function(cs)
                return c .. cs
            end)
        end):expect"operator"

        tokenParser.operator = tokenParser:lexeme(
            oper:bind(function(name)
                if contains(languageDef.reservedOpNames, name) then
                    return unexpected("reserved operator " .. name)
                else
                    return from(name)
                end
            end)
            :try()
        )
    end)

    ----------------------------------------------------
    -- Identifiers & Reserved words
    ----------------------------------------------------
    local function caseString(name)
        local function caseChar(c)
            if c:find("%a") then
                return char(c:lower()):otherwise(char(c:upper()))
            else
                return char(c)
            end
        end

        local function walk(s)
            if s == "" then
                return from(nil)
            else
                local c = s:sub(1,1)
                return caseChar(c):expect(name):discardBind(walk(s:sub(2)))
            end
        end

        if languageDef.caseSensitive then
            return string(name)
        else
            return walk(name):discardBind(from(name))
        end
    end

    function tokenParser:reserved(name)
        return self:lexeme(
            caseString(name)
            :discardBind(
                languageDef.identLetter
                :notFollowedBy()
                :expect("end of " .. name)
            )
            :try()
        )
    end

    constants(function()
        local ident = languageDef.identStart:bind(function(c)
            return languageDef.identLetter
                :many()
                :fmap(concat({c}))
                :fmap(table.concat)
                :expect"identifier"
        end)

        tokenParser.identifier = tokenParser:lexeme(
            ident:bind(function(name)
                if contains(languageDef.reservedNames, name) then
                    return unexpected("reserved word " .. name)
                else
                    return from(name)
                end
            end)
            :try()
        )
    end)

    ----------------------------------------------------
    -- White space & symbols
    ----------------------------------------------------

    function tokenParser:symbol(name)
        return self:lexeme(string(name))
    end

    function tokenParser:lexeme(p)
        return p:bind(function(a)
            return self.whiteSpace:discardBind(from(a))
        end)
    end

    do -- do this outside constants() because lots of constants depend on whitespace
        local whiteSpace = satisfy(function(c) return c:find"%s" ~= nil end):skipMany1()

        tokenParser.whiteSpace = whiteSpace:otherwise(languageDef.comment):skipMany()
    end

    runConstants()

    return tokenParser
end

-- OTHER

function sequence(list)
    if #list == 0 then
        return from({})
    else
        return list[1]:bind(function(a)
            return sequence(tail(list)):fmap(concat({a}))
        end)
    end
end

-- INIT CONSTANTS

runConstants()