local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")

local languageDef = {
    commentStart = nil, -- the library doesn't support Lua's multiline comments. They are an outlier
    commentEnd = nil,
    commentLine = "--",
    nestedComments = nil,
    identStart = parsel.letter:otherwise(parsel.char"_"),
    identLetter = parsel.alphaNum:otherwise(parsel.char"_"),
    opStart = parsel.oneOf"<>+-/=*^%#.",
    opLetter = parsel.oneOf"<>+-/=*^%#.",
    reservedNames = {
        "and",       "break",     "do",        "else",      "elseif",
        "end",       "false",     "for",       "function",  "if",
        "in",        "local",     "nil",       "not",       "or",
        "repeat",    "return",    "then",      "true",      "until",     "while",
    },
    reservedOpNames = {
        "+",     "-",     "*",     "/",
        "%",     "^",     "#",     "..",
        "==",    "~=",    "<=",    ">=",
        "<",     ">",     "="
    },
    caseSensitive = true,
}

local tokens = parsel.makeTokenParser(languageDef)

-- Forward declaring parsers in order
local chunk,
    block,
    var,
    stat,
    namelist,
    namelist1,
    varlist,
    varlist1,
    explist,
    explist1,
    exp,
    stringExp,
    prefixexp,
    suffixexp,
    tableconstructor,
    --fieldlist,
    field,
    fieldsep,
    prefixcall,
    functioncall,
    suffixcall,
    method,
    index,
    fargs,
    func,
    funcbody,
    funcname,
    parlist,
    parlist1

--------------------------------------------------
-- Data types
--------------------------------------------------

local Var = {
    Name = parsel.cons(1),
    IndexedPrefix = parsel.cons(2)
}

local Stat = {
    Do              = parsel.cons(1),   -- block
    Assignment      = parsel.cons(2),   -- varlist1, explist1
    While           = parsel.cons(2),   -- exp, block
    Repeat          = parsel.cons(2),   -- block, exp
    If              = parsel.cons(4),   -- exp, block, {ElseIf}, [block]
    Return          = parsel.cons(1),   -- [explist1]
    Break           = parsel.cons(),
    For             = parsel.cons(5),   -- Name, exp, exp, [exp], block
    ForIn           = parsel.cons(3),   -- namelist1, explist1, block
    FunctionCall    = parsel.cons(1),   -- functioncall
    Local           = parsel.cons(2),   -- namelist1, [explist1]
    Function        = parsel.cons(2),   -- funcname, funcbody
    LocalFunction   = parsel.cons(2),   -- Name, funcbody
}

local ElseIf = {
    ElseIf = parsel.cons(2) -- exp, block
}

local PrefixExp = {
    FunctionCall    = parsel.cons(2),   -- functioncall, suffixexp
    Name            = parsel.cons(2),   -- Name, suffixexp
    Expression      = parsel.cons(2)   -- exp, suffixexp
}

local Field = {
    Expression  = parsel.cons(1),   -- exp
    Named       = parsel.cons(2),   -- Name, exp
    Indexed     = parsel.cons(2)    -- exp, exp
}

local FunctionCall = {
    Function = parsel.cons(3), -- prefixcall, fargs, suffixcall
    Method = parsel.cons(3), -- prefixcall, method, suffixcall

    Prefix = {
        Name = parsel.cons(2), -- Name, suffixexp
        Expression = parsel.cons(2) -- exp, suffixexp
    },

    Suffix = {
        Function = parsel.cons(2), -- suffixexp, fargs, suffixcall
        Method = parsel.cons(2) -- suffixexp, method, suffixcall
    }
}

local Method = {
    Method = parsel.cons(2) -- Name, fargs
}

local Index = {
    Name = parsel.cons(1),
    Expression = parsel.cons(1)
}

local FunctionBody = {
    FunctionBody = parsel.cons(2) -- parlist, block
}

local FunctionName = {
    Function = parsel.cons(1),  -- names
    Method = parsel.cons(2)     -- names, methodname
}

local Parameter = {
    Variable = parsel.cons(1), -- name
    Vararg = parsel.cons()
}

--------------------------------------------------
-- Chunk, block
--------------------------------------------------

function chunk()
    return stat():bind(function(statement)
        return tokens:symbol";":discardBind(statement)
    end):many()
end

block = chunk

--------------------------------------------------
-- Var
--------------------------------------------------

function var()
    return tokens.identifier:bind(Var.Name)
    :otherwise(prefixexp():bind(function(prefix)
        return index():fmap(Var.IndexedPrefix(prefix))
    end))
end

--------------------------------------------------
-- Stat
--------------------------------------------------

do
    -- Do
    local function doStat()
        return tokens:reserved"do":bind(function()
            return block():fmap(Stat.Do):bind(function(d)
                return tokens:reserved"end":discardBind(from(d))
            end)
        end)
    end

    -- Assignment
    local function assignmentStat()
        return varlist1():bind(function(vars)
            return tokens:reservedOp"=":discardBind(explist1():fmap(Stat.Assignment(vars)))
        end):try()
    end

    -- While
    local function whileStat()
        return tokens:reserved"while":discardBind(exp():bind(function(expression)
            return tokens:reserved"do":discardBind(block():fmap(Stat.While(expression)):bind(function(wh)
                return tokens:reserved"end":discardBind(from(wh))
            end))
        end))
    end

    -- Repeat
    local function repeatStat()
        return tokens:reserved"repeat":discardBind(block():bind(function(bl)
            return tokens:reserved"until":discardBind(exp():fmap(function(expression)
                return Stat.Repeat(bl, expression)
            end))
        end))
    end

    -- If
    local function elseIf()
        return tokens:reserved"elseif":discardBind(exp():bind(function(expression)
            return tokens:reserved"then":discardBind(block():fmap(ElseIf.ElseIf(expression)))
        end))
    end

    local function ifStat()
        return parsel.sequence({
            tokens:reserved"if",    -- 1
            exp(),                  -- 2
            tokens:reserved"then",  -- 3
            block(),                -- 4
            elseIf:many(),          -- 5
            tokens:reserved"else":discardBind(block()):optionMaybe(), -- 6
            tokens:reserved"end"    -- 7
        }):fmap(function(list)
            return Stati.If(list[2], list[4], list[5], list[6])
        end)
    end

    -- Return
    local function returnStat()
        return tokens:reserved"return":discardBind(explist1():try():optionMaybe()):fmap(Stat.Return)
    end

    -- Break
    local function breakStat()
        return tokens:reserved"break":discardBind(parsel.from(Stat.Break))
    end

    -- For
    local function forStat()
        return parsel.sequence({
            tokens:reserved"for",   -- 1
            tokens.identifier,      -- 2
            tokens:reservedOp"=",   -- 3
            exp(),                  -- 4
            tokens.comma,           -- 5
            exp(),                  -- 6
            tokens.comma:discardBind(exp()):try():option(Expression.Number(1)), -- 7
            tokens:reserved"do",    -- 8
            block(),                -- 9
            tokens:reserved"end"    -- 10
        }):try():fmap(function(list)
            return Stat.For(list[2], list[4], list[6], list[7], list[9])
        end)
    end

    -- ForIn
    local function forInStat()
        return parsel.sequence({
            tokens:reserved"for",   -- 1
            namelist1(),             -- 2
            tokens:reserved"in",    -- 3
            explist1(),              -- 4
            tokens:reserved"do",    -- 5
            block(),                -- 6
            tokens:reserved"end"    -- 7
        }):fmap(function(list)
            return Stat.ForIn(list[2], list[4], list[6])
        end)
    end

    -- FunctionCall
    local function functionCallStat()
        return functioncall()
    end

    -- Local
    local function localStat()
        return tokens:reserved"local":discardBind(namelist1():bind(function(names)
            return reservedOp"=":discardBind(explist1()):try():optionMaybe():fmap(Stat.Local(names))
        end)):try()
    end

    -- Function
    local function functionStat()
        return tokens:reserved"function":discardBind(funcname():bind(function(name)
            return funcbody():fmap(Stat.Function(name))
        end))
    end

    -- LocalFunction
    local function localFunctionStat()
        return parsel.sequence({
            tokens:reserved"local",     -- 1
            tokens:reserved"function",  -- 2
            tokens.identifier,          -- 3
            funcbody()                  -- 4
        }):fmap(function(list)
            return Stat.LocalFunction(list[3], list[4])
        end)
    end

    function stat()
        return parsel.choice({
            doStat(),
            assignmentStat(),
            whileStat(),
            repeatStat(),
            ifStat(),
            returnStat(),
            breakStat(),
            forStat(),
            forInStat(),
            functionCallStat(),
            localStat(),
            functionStat(),
            localFunctionStat()
        })
    end
end

--------------------------------------------------
-- Namelist
--------------------------------------------------

function namelist()
    return namelist1():otherwise(from({}))
end

function namelist1()
    return tokens:commaSep1(tokens.identifier):try()
end

--------------------------------------------------
-- Varlist
--------------------------------------------------

function varlist()
    return varlist1():otherwise(from({}))
end

function varlist1()
    return tokens:commaSep1(var()):try()
end

--------------------------------------------------
-- Explist
--------------------------------------------------

function explist()
    return explist1():otherwise(from({}))
end

function explist1()
    return tokens:commaSep1(exp()):try()
end

--------------------------------------------------
-- Exp
--------------------------------------------------

-- TODO

--------------------------------------------------
-- StringExp
--------------------------------------------------

-- TODO

--------------------------------------------------
-- Prefixexp
--------------------------------------------------

function prefixexp()
    return parsel.choice({
        functioncall():try():fmap(PrefixExp.FunctionCall),
        tokens.identifier:fmap(PrefixExp.Name),
        tokens:parens(exp()):fmap(PrefixExp.Expression)
    }):bind(function(a)
        return suffixexp():fmap(a)
    end)
end

function suffixexp()
    return index():many()
end

--------------------------------------------------
-- Tableconstructor
--------------------------------------------------

function tableconstructor()
    return tokens:braces(field():sepBy(fieldsep()))
end

--------------------------------------------------
-- Field
--------------------------------------------------

function field()
    return tokens:brackets(exp()):bind(function(expression)
        return tokens:reserved"=":discardBind(exp():fmap(Field.Indexed(expression)))
    end)
    :otherwise(tokens.identifier:bind(function(name)
        return tokens:reserved"=":discardBind(exp():fmap(Field.Named(name)))
    end):try())
    :otherwise(exp():fmap(Field.Expression))
end

--------------------------------------------------
-- Fieldsep
--------------------------------------------------

function fieldsep()
    return tokens.comma:otherwise(tokens.semi)
end

--------------------------------------------------
-- Prefixcall
--------------------------------------------------

function prefixcall()
    return parsel.choice({
        tokens.identifier:fmap(FunctionCall.Prefix.Name),
        tokens:parens(exp()):fmap(FunctionCall.Prefix.Expression)
    }):bind(function(a)
        return suffixexp():fmap(a)
    end)
end

--------------------------------------------------
-- Functioncall
--------------------------------------------------

function functioncall()
    return prefixcall():bind(function(pre)
        return fargs():fmap(FunctionCall.Function(pre))
                :otherwise(method():fmap(FunctionCall.Method(pre)))
                :bind(function(f)
                    return suffixcall():fmap(f)
                end)
    end)
end

--------------------------------------------------
-- Suffixcall
--------------------------------------------------

function suffixcall()
    return suffixexp():bind(function(suf)
        return fargs():fmap(FunctionCall.Suffix.Function(suf))
                :otherwise(method():fmap(FunctionCall.Suffix.Method(suf)))
                :many()
    end)
end

--------------------------------------------------
-- Method
--------------------------------------------------

function method()
    return tokens.colon:discardBind(tokens.identifier:bind(function(name)
        return fargs():fmap(Method.Method(name))
    end))
end

--------------------------------------------------
-- Index
--------------------------------------------------

function index()
    return tokens.dot:discardBind(tokens.identifier:fmap(Index.Name))
            :otherwise(tokens:brackets(exp()):fmap(Index.Expression))
end

--------------------------------------------------
-- Fargs
--------------------------------------------------

function fargs()
    return parsel.choice({
        tokens:parens(explist1()),
        tableconstructor():bind(function(tbl) return parsel.from({tbl}) end),
        stringExp():bind(function(str) return parsel.from({str}) end)
    })
end

--------------------------------------------------
-- Func
--------------------------------------------------

function func()
    return tokens:reserved"function":discardBind(funcbody())
end

--------------------------------------------------
-- Funcbody
--------------------------------------------------

function funcbody()
    return parsel.sequence({
        tokens:parens(parlist), -- 1
        block(),                -- 2
        tokens:reserved"end"    -- 3
    }):fmap(function(list)
        return FunctionBody.FunctionBody(list[1], list[2])
    end)
end

--------------------------------------------------
-- Funcname
--------------------------------------------------

function funcname()
    return tokens.identifier:sepBy1(tokens.dot):bind(function(names)
        return tokens.colon:discardBind(tokens.identifier)
                :fmap(FunctionName.Method(names))
                :otherwise(from(FunctionName.Function(names)))
    end)
end

--------------------------------------------------
-- Parlist
--------------------------------------------------

function parlist()
    return parlist1():otherwise(from({}))
end

function parlist1()
    local parameter = tokens.identifier:fmap(Parameter.Variable)
                        :otherwise(
                            tokens:symbol"...":discardBind(
                                tokens.comma:notFollowedBy()
                            ):discardBind(from(Parameter.Vararg))
                        )
    return tokens:commaSep1(parameter)
end

local Lua = chunk()

-- File

assert((...), "Usage: test.lua path")
local path = shell.resolve((...))
assert(fs.exists(path) and not fs.isDir(path), "File not found")

local fh = fs.open(path, "r")
local s = fh.readAll()
fh.close()

local ok, val = Lua:parse(s, fs.getName(path))

if not ok then
    printError(val)
else
    print("Parse successful")
end