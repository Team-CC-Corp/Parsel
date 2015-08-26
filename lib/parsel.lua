-- DEBUG UTIL

-- local fh = fs.open("parsel.log", "w")
local debugPrint = false
local function debug(s)
    if fh then
        fh.writeLine(s)
        fh.flush()
    end
    if debugPrint then
        print(s)
    end
end

-- NIL

Nil = {}

-- TABLE UTIL

--[[ concat(table, table)

Concatenate two tables
]]
function concat(a, b)
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

--[[ tail(table)

Returns a copy of t without its first element
]]
function tail(t)
    local tl = {}
    for i=2,#t do
        tl[i - 1] = t[i]
    end
    return tl
end

--[[ replicate(number, any)

Returns a table of length n, populated with x
]]
function replicate(n, x)
    local t = {}
    for i=1, n do
        table.insert(t, x)
    end
    return t
end

--[[ foldl(function, any, table)

See: Haskell's foldl function
]]
function foldl(f, accum, t)
    for i,v in ipairs(t) do
        accum = f(accum, v)
    end
    return accum
end

--[[ foldr(function, any, table)

See: Haskell's foldr function
]]
function foldr(f, accum, t)
    for i=#t,1,-1 do
        accum = f(t[i], accum)
    end
    return accum
end

--[[ contains(table, any)

Returns true if t contains x
]]
function contains(t, x)
    for k,v in pairs(t) do
        if v == x then
            return true
        end
    end
    return false
end

-- FUNCTION UTIL

--[[ id(...)

Returns exactly what was passed.
Used as the base continuation in the apply function.
]]
function id(...)
    return ...
end

--[[ thunk(f, name)

Returns a function that memoizes f's result
]]
function thunk(f, name)
    local run = false
    local ret
    return function()
        if not run then
            -- print((showStack("Running thunk: " .. name, 1):gsub("\n.*", "")))
            ret = {f()}
            run = true
        end
        return unpack(ret)
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
                get = function(n)
                    return unpack(args, n or 1)
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

-- Stream
do
    local streamMt = {
        __eq = function(lhs, rhs)
            return lhs.text == rhs.text and lhs.cursor == rhs.cursor
        end
    }
    function offsetStream(s, offset)
        return setmetatable({text=s.text, cursor=s.cursor+offset}, streamMt)
    end

    function newStream(str)
        return setmetatable({text=str, cursor=1}, streamMt)
    end
end

--[[ Result type

This type is returned by a parser.
If a parser failed, it will return a Fail
with a list of Messages, the index of failure
and the index of unconsumed input.
]]

Result = {
    Fail = cons(3), -- [Message], csOfFail, cs
    Success = cons(2) -- a, cs
}

--[[ Message type

This type represents error messages.
If an unexpected token is encountered, an Unexpected is returned.
If something in particular was expected, an Expected is returned.
If there is a general error message, a Message is returned.
]]
Message = {
    Unexpected = cons(1), -- String
    Expected = cons(1), -- String
    Message = cons(1) -- String
}

function resultUnexpected(str, csOfFail, cs)
    return Result.Fail({Message.Unexpected(str)}, csOfFail, cs)
end

function resultExpected(str, csOfFail, cs)
    return Result.Fail({Message.Expected(str)}, csOfFail, cs)
end

function resultMessage(str, csOfFail, cs)
    return Result.Fail({Message.Message(str)}, csOfFail, cs)
end

-- PARSER TYPE

Parser = {}

--[[ new((string, Result->Result) -> Result)

Returns a new Parser instance with the string parser f.
]]
function new(f)
    -- local stack = getStack("", 2)
    -- local _f = f
    -- f = function(s, cont)
    --     local show = s:gsub("\n.*", "")
    --     debug(stack[1] .. "stack depth: " .. #getStack("",1) .. ": " .. show)
    --     return _f(s, cont)
    -- end
    return setmetatable({runParser=f}, {__index=Parser})
end

-- CHAR

--[[ satisfy(char->boolean)

Returns a parser that parses any character that makes f return true
]]
function satisfy(f)
    return anyChar:bind(function(c)
        if f(c) then
            return from(c)
        else
            return unexpected(c)
        end
    end):try()
end

--[[ char(char)

Returns a parser that succeeds on the character c
]]
function char(c)
    stackAssert(type(c) == "string" and #c == 1, "Expected character")
    return satisfy(function(c1) return c == c1 end):expect(c)
end

--[[ string(string)

Returns a parser that succeeds on the exact string str
]]
function string(str)
    stackAssert(type(str) == "string", "Expected string")
    return new(function(s, cont)
        if s.text:sub(s.cursor, s.cursor + #str - 1) == str then
            return cont(Result.Success(str, offsetStream(s, #str)))
        else
            return cont(resultExpected(str, s, s))
        end
    end)
end

--[[ oneOf(string)

Returns a parser that succeeds on any of the characters in s
]]
function oneOf(s)
    return satisfy(function(c)
        return s:find(c, 1, true) ~= nil
    end)
end

--[[ oneOf(string)

Returns a parser that succeeds on none of the characters in s
]]
function noneOf(s)
    return satisfy(function(c)
        return s:find(c, 1, true) == nil
    end)
end

constants(function()
    -- Parses any character
    anyChar = new(function(s, cont)
        if s.cursor > #s.text then
            return cont(resultUnexpected("EOF", s, s))
        else
            return cont(Result.Success(s.text:sub(s.cursor,s.cursor), offsetStream(s, 1)))
        end
    end)
    
    -- Parses any space character
    space = satisfy(function(c) return c:find"%s" ~= nil end)
    -- Parses any number of space characters
    spaces = space:many()

    -- Succeeds only at the end of input
    eof = anyChar:notFollowedBy():expect"end of input"

    newLine = char"\n"
    crlf = string"\r\n"
    -- Parses the end of the line
    endOfLine = newLine:otherwise(crlf):otherwise(eof)

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

--[[ choice([Parser])

Returns a parser that tries each parser in the list
until one succeeds
]]
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

--[[ count(number)

Returns a parser that parses self n times
]]
function Parser:count(n)
    return sequence(replicate(n, self))
end

--[[ between(Parser, Parser)

Returns a parser that parsers start, self, then stop.
]]
function Parser:between(start, stop)
    return start:discardBind(self:bind(function(a)
        return stop:discardBind(from(a))
    end))
end

--[[ option(a)

Returns a parser that attempts to parse self,
otherwise returns from(a)
]]
function Parser:option(a)
    return self:otherwise(from(a))
end


--[[ optionMaybe()

Same as self:option(Nil)
]]
function Parser:optionMaybe()
    return self:otherwise(from(Nil))
end

--[[ optional()

Same as self:optionMaybe():discardBind(Nil)
]]
function Parser:optional()
    return self:discardBind(from(Nil)):otherwise(from(Nil))
end

--[[ skipMany1()

Returns a parser that parses self one or more times
then discards the results
]]
function Parser:skipMany1()
    return self:many1():discardBind(from(Nil))
end

--[[ many1()

Returns a parser that parses self one or more times
]]
function Parser:many1()
    return self:bind(function(a)
        return self:many():fmap(concat({a}))
    end)
end

--[[ sepBy(Parser)

Returns a parser that parses self delimited by sep zero or more times
]]
function Parser:sepBy(sep)
    return self:sepBy1(sep):otherwise(from({}))
end

--[[ sepBy1(Parser)

Returns a parser that parses self delimited by sep one or more times
]]
function Parser:sepBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(self):many():fmap(concat({a}))
    end)
end

--[[ endBy(Parser)

Returns a parser that parses self delimited by sep and ending with sep zero or more times
]]
function Parser:endBy(sep)
    return self:endBy1(sep):otherwise(from({}))
end

--[[ endBy1(Parser)

Returns a parser that parses self delimited by sep and ending with sep one or more times
]]
function Parser:endBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(from(a))
    end):many1()
end

--[[ sepEndBy(Parser)

Returns a parser that parses self delimited by sep and ending with an optional sep zero or more times
]]
function Parser:sepEndBy(sep)
    return self:sepEndBy1(sep):otherwise(from({}))
end

--[[ sepEndBy1(Parser)

Returns a parser that parses self delimited by sep and ending with an optional sep one or more times
]]
function Parser:sepEndBy1(sep)
    return self:bind(function(a)
        return sep:discardBind(self:sepEndBy(sep)):fmap(concat({a})):otherwise(from({a}))
    end)
end

--[[ chainl(Parser, any)

Parses zero or more occurrences of self, separated by op.
Returns a value obtained by a left associative application
of all functions returned by op to the values returned by self.
If there are zero occurrences of self, the value x is returned.
]]
function Parser:chainl(op, x)
    return self:chainl1(op):otherwise(from(x))
end

--[[ chainl1(Parser)

Same as chainl, except at least one occurecne of self is required.
]]
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

--[[ chainr(Parser, any)

Parses zero or more occurrences of self, separated by op.
Returns a value obtained by a right associative application
of all functions returned by op to the values returned by self.
If there are zero occurrences of self, the value x is returned.
]]
function Parser:chainr(op, x)
    return self:chainr1(op):otherwise(from(x))
end

--[[ chainl=r1(Parser)

Same as chainr, except at least one occurecne of self is required.
]]
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

--[[ notFollowedBy()

Fails if self succeeds, otherwise returns Nil
]]
function Parser:notFollowedBy()
    return self:bind(function(c)
        return unexpected(tostring(c))
    end)
    :otherwise(from(Nil))
    :try()
end

--[[ manyTill(Parser)

Parses self many times until ending succeeds
]]
function Parser:manyTill(ending)
    local function scan()
        return ending:discardBind(from({})):otherwise(self:bind(function(a)
            return scan():fmap(concat({a}))
        end))
    end
    return scan()
end

--[[ lookahead()

Parses self. Ordinary failure behavior, but on success it rolls back the input
]]
function Parser:lookahead()
    return new(function(s, cont)
        return self.runParser(s, function(result)
            if result.cons() ~= Result.Success then
                return cont(result)
            else
                return cont(Result.Success(result.get(), s))
            end
        end)
    end)
end

-- PRIMITIVE

--[[ bind(any->Parser)

Parses self. Passes the result to f, then parses the returned parser
]]
function Parser:bind(f)
    return new(function(s, cont)
        return self.runParser(s, function(result)
            if result.cons() == Result.Success then
                return f(result.get()).runParser(result.get(2), cont)
            else
                return cont(result)
            end
        end)
    end)
end

--[[ discardBind(Parser)

Parses self, then parses p
]]
function Parser:discardBind(p)
    return self:bind(function() return p end)
end

--[[ fmap(any->any)

Parses self, but returns f(a) instead of a
]]
function Parser:fmap(f)
    return self:bind(function(a)
        return from(f(a))
    end)
end

--[[ fail(string)

Fails with the message str
]]
function fail(str)
    return new(function(s, cont)
        return cont(resultMessage(str, s, s))
    end)
end

--[[ unexpected(string)

Fails with an unexpected error message
]]
function unexpected(str)
    return new(function(s, cont)
        return cont(resultUnexpected(str, s, s))
    end)
end

--[[ from(any)

Returns a parser that always succeeds, consumes no input, and returns a
]]
function from(a)
    return new(function(s, cont)
        return cont(Result.Success(a, s))
    end)
end

-- Use to eliminate unfortunate recursive thunks, since Lua isn't lazy
function fromThunk(f)
    return new(function(s, cont)
        return f().runParser(s, cont)
    end)
end

--[[ expect(string)

Parses self, but replaces any Expected error messages with str
]]
function Parser:expect(str)
    return new(function(s, cont)
        return self.runParser(s, function(result)
            if result.cons() == Result.Fail then
                local messages = {}
                for i,e in ipairs(result.get()) do
                    if e.cons() ~= Message.Expected then
                        table.insert(messages, e)
                    end
                end
                table.insert(messages, Message.Expected(str))
                return cont(Result.Fail(messages, result.get(2), result.get(3)))
            else
                return cont(result)
            end
        end)
    end)
end

--[[ try()

Parses self. If it fails, roll the input back
]]
function Parser:try()
    return new(function(s, cont)
        return self.runParser(s, function(result)
            if result.cons() == Result.Fail then
                return cont(Result.Fail(result.get(), s, s))
            else
                return cont(result)
            end
        end)
    end)
end

--[[ otherwise(Parser)

Parses self. If it fails without consuming input, parses b
]]
function Parser:otherwise(b)
    return new(function(s, cont)
        return self.runParser(s, function(result)
            if (result.cons() == Result.Fail) and result.get(3) == s then
                return b.runParser(s, function(bresult)
                    if bresult.cons() == Result.Fail and bresult.get(3) == s then
                        return cont(Result.Fail(concat(result.get(), (bresult.get())), s, s))
                    else
                        return cont(bresult)
                    end
                end)
            else
                return cont(result)
            end
        end)
    end)
end

constants(function()
    zero = new(function(s, cont)
        return cont(resultMessage("Error", s, s))
    end)
end)

--[[ many()

Parses self zero or more times
]]
function Parser:many()
    return self:many1():otherwise(from({}))
end

--[[ skipMany()

Parses self zero or more times, discarding the results
]]
function Parser:skipMany()
    return self:many():discardBind(from(Nil))
end

--[[ trace(any->void)

Parses self, the passes the result to f.
f defaults to print(tostring(a))
]]
function Parser:trace(f)
    f = f or function(a)
        print(tostring(a))
    end
    return self:bind(function(a)
        f(a)
        return from(a)
    end)
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

--[=[ buildExpressionParser([[Operator]])

Creates an expression parser, where self is the terms.
Operators is a list of lists of operators.
Each top level list represents prescedence.
The lists within each prescendenc level contain operators with the same prescedence.
]=]
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
                return from(Nil)
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

    function tokenParser:apply(p, s)
        stackAssert(s, "Nil apply string")
        return self.whiteSpace:discardBind(p):bind(function(a)
            return eof:discardBind(from(a))
        end).runParser(s, id)
    end

    function tokenParser:parse(p, s, sourceName)
        if type(sourceName) ~= "string" then
            sourceName = "string"
        end

        local result = self:apply(p, newStream(s))

        if result.cons() == Result.Fail then
            local errors, csOfFail, cs = result.get()

            local consumedInput = s:sub(1, csOfFail.cursor - 1)
            local _, linesConsumed = consumedInput:gsub("\n", "")
            local lineNumber = linesConsumed + 1

            local near = s:sub(csOfFail.cursor):gsub("%s*(%S+)(.*)", "%1")
            if near == "" then near = "End of input" end

            local messages = {}
            for i,e in ipairs(errors) do
                if e.cons() == Message.Unexpected then
                    messages[i] = "Unexpected: " .. e.get()
                elseif e.cons() == Message.Expected then
                    messages[i] = "Expected: " .. e.get()
                else
                    messages[i] = e.get()
                end
            end

            local errMsg = table.concat(messages, "\n")
                .. "\n  at: " .. sourceName .. ":" .. lineNumber
                .. "\n  near: " .. near

            return false, errMsg
        else
            return true, (result.get())
        end
    end

    runConstants()

    return tokenParser
end

-- OTHER

--[[ sequence([Parser])

Takes a list of parsers and returns a parser of a list
]]
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