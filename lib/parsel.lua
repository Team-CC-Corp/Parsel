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

-- FUNCTION UTIL

local function id(...)
    return ...
end

-- CONSTANTS UTIL
-- Constants can't be declared before the methods they use
-- But for organizational purposes, this is often necessary

local constantInitializers = {}
local function constants(f)
    table.insert(constantInitializers, f)
end

local function runConstants()
    for f in function() return table.remove(constantInitializers) end do
        f()
    end
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
        local ok, a, cs, unexp = f(stackAssert(s, "Nil string"))
        stackAssert(cs, "Nil suffix")
        return ok, a, cs, unexp
    end
    return setmetatable({runParser=f1}, {__index=Parser})
end

-- CHAR

function satisfy(f)
    return anyChar:bind(function(c)
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
    local p = list[1]
    if not p then
        return fail("No choices")
    end

    for i=2, #list do
        p = p:otherwise(list[i])
    end

    return p
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
    return spaces:discardBind(self).runParser(s)
end

function Parser:parse(s, sourceName)
    if type(sourceName) ~= "string" then
        sourceName = "string"
    end

    local ok, a, cs = self:apply(s)

    if not ok then
        local consumedInput = s:sub(1, -(#cs + 1))
        local _, linesConsumed = consumedInput:gsub("\n", "")
        local lineNumber = linesConsumed + 1

        local errMsg = "Expected: " .. table.concat(a, ", ")
            .. "\n  at: " .. sourceName .. ":" .. lineNumber
            .. " near " .. cs:gsub("%s*(%S+)(.*)", "%1")

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
    return self:many1():otherwise(from({}))
end

function Parser:skipMany()
    return self:many():discardBind(from(nil))
end

-- EXPRESSION

Assoc = {
    None = "non",
    Left = "left",
    Right= "right"
}

function infixOperator(p, assoc)
    return {
        type = "infix",
        assoc = assoc,
        parser = p
    }
end

function prefixOperator(p)
    return {
        type="prefix",
        parser = p
    }
end

function postfixOperator(p)
    return {
        type="postfix",
        parser = p
    }
end

function Parser:buildExpressionParser(operators)
    local function splitOp(op, opTypesList)
        local rassocOp, lassocOp, nassocOp, prefixOp, postfixOp = unpack(opTypesList)

        if op.type == "infix" then
            if op.assoc == Assoc.None then
                nassocOp = concat({op.parser}, nassocOp)
            elseif op.assoc == Assoc.Left then
                lassocOp = concat({op.parser}, lassocOp)
            elseif op.assoc == Assoc.Right then
                rassocOp = concat({op.parser}, rassocOp)
            end
        elseif op.type == "prefix" then
            prefixOp = concat({op.parser}, prefixOp)
        elseif op.type == "postfix" then
            postfixOp = concat({op.parser}, postfixOp)
        end

        return {rassocOp, lassocOp, nassocOp, prefixOp, postfixOp}
    end

    local function ambigious(assoc, op)
        return op:discardBind(fail("ambigious use of a " .. assoc .. " associative operator")):try()
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

        local postfixP = postfixOp:otherwise(from(id))
        local prefixP = prefixOp:otherwise(from(id))

        local termP = prefixP:bind(function(pre)
            return term:bind(function(a)
                return postfixP:bind(function(post)
                    return from(post(pre(a)))
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
    commentStart   :: String,

    commentEnd     :: String,

    commentLine    :: String,

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
        ["a"] = "\a"
        ["b"] = "\b"
        ["f"] = "\f"
        ["n"] = "\n"
        ["r"] = "\r"
        ["t"] = "\t"
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
            return baseDigit:fmap(function(digits)
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

    runConstants()

    return tokenParser
end

-- OTHER

function symbol(s)
    return string(s):lexeme()
end

function sequence(list)
    if #list == 0 then
        return from({})
    else
        return list[1]:bind(function(a)
            return sequence(tail(list)):fmap(concat({a}))
        end)
    end
end

function Parser:lexeme()
    return self:bind(function(a)
        return spaces:discardBind(from(a))
    end)
end

-- INIT CONSTANTS

runConstants()