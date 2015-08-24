local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")

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
    longStringStart,
    longString,
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

local tokens

local comment = parsel.fromThunk(parsel.thunk(function()
    return tokens:symbol"--":discardBind(
        longStringStart:lookahead():try():discardBind(longString)
        :otherwise(parsel.anyChar:manyTill(parsel.endOfLine):fmap(table.concat))
    )
end, "comment"))

local languageDef = {
    comment = comment,
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

tokens = parsel.makeTokenParser(languageDef)

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

local Expression = {
    Prefix = parsel.cons(1),
    Nil = parsel.cons(),
    Boolean = parsel.cons(1),
    Number = parsel.cons(1),
    String = parsel.cons(1),
    Function = parsel.cons(1),
    TableConstructor = parsel.cons(1),
    Dots = parsel.cons(),
    BinOp = parsel.cons(1),
    UnaryOp = parsel.cons(1)
}

local BinOp = {
    Plus = parsel.cons(2),
    Minus = parsel.cons(2),
    Multiply = parsel.cons(2),
    Divide = parsel.cons(2),
    Mod = parsel.cons(2),
    Pow = parsel.cons(2),
    Concat = parsel.cons(2),
    Equals = parsel.cons(2),
    NotEquals = parsel.cons(2),
    LE = parsel.cons(2),
    GE = parsel.cons(2),
    LT = parsel.cons(2),
    GT = parsel.cons(2),
    And = parsel.cons(2),
    Or = parsel.cons(2)
}

local UnaryOp = {
    Negate = parsel.cons(1),
    Length = parsel.cons(1),
    Not = parsel.cons(1)
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
    return stat:bind(function(statement)
        return tokens:symbol";":optionMaybe():discardBind(parsel.from(statement))
    end):many()
end

block = chunk

--------------------------------------------------
-- Var
--------------------------------------------------

function var()
    return tokens.identifier:fmap(Var.Name)
    :otherwise(prefixexp:bind(function(prefix)
        return index:fmap(Var.IndexedPrefix(prefix))
    end))
end

--------------------------------------------------
-- Stat
--------------------------------------------------

do
    -- Do
    local function doStat()
        return tokens:reserved"do":bind(function()
            return block:fmap(Stat.Do):bind(function(d)
                return tokens:reserved"end":discardBind(parsel.from(d))
            end)
        end)
    end

    -- Assignment
    local function assignmentStat()
        return varlist1:bind(function(vars)
            return tokens:reservedOp"=":discardBind(explist1:fmap(Stat.Assignment(vars)))
        end):try()
    end

    -- While
    local function whileStat()
        return tokens:reserved"while":discardBind(exp:bind(function(expression)
            return tokens:reserved"do":discardBind(block:fmap(Stat.While(expression)):bind(function(wh)
                return tokens:reserved"end":discardBind(parsel.from(wh))
            end))
        end))
    end

    -- Repeat
    local function repeatStat()
        return tokens:reserved"repeat":discardBind(block:bind(function(bl)
            return tokens:reserved"until":discardBind(exp:fmap(function(expression)
                return Stat.Repeat(bl, expression)
            end))
        end))
    end

    -- If
    local function elseIf()
        return tokens:reserved"elseif":discardBind(exp:bind(function(expression)
            return tokens:reserved"then":discardBind(block:fmap(ElseIf.ElseIf(expression)))
        end))
    end

    local function ifStat()
        return parsel.sequence({
            tokens:reserved"if",    -- 1
            exp,                    -- 2
            tokens:reserved"then",  -- 3
            block,                  -- 4
            elseIf():many(),          -- 5
            tokens:reserved"else":discardBind(block):optionMaybe(), -- 6
            tokens:reserved"end"    -- 7
        }):fmap(function(list)
            return Stat.If(list[2], list[4], list[5], list[6])
        end)
    end

    -- Return
    local function returnStat()
        return tokens:reserved"return":discardBind(explist):fmap(Stat.Return)
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
            exp,                    -- 4
            tokens.comma,           -- 5
            exp,                    -- 6
            tokens.comma:discardBind(exp):try():option(Expression.Number(1)), -- 7
            tokens:reserved"do",    -- 8
            block,                  -- 9
            tokens:reserved"end"    -- 10
        }):try():fmap(function(list)
            return Stat.For(list[2], list[4], list[6], list[7], list[9])
        end)
    end

    -- ForIn
    local function forInStat()
        return parsel.sequence({
            tokens:reserved"for",   -- 1
            namelist1,              -- 2
            tokens:reserved"in",    -- 3
            explist1,               -- 4
            tokens:reserved"do",    -- 5
            block,                  -- 6
            tokens:reserved"end"    -- 7
        }):fmap(function(list)
            return Stat.ForIn(list[2], list[4], list[6])
        end)
    end

    -- FunctionCall
    local function functionCallStat()
        return functioncall
    end

    -- Local
    local function localStat()
        return tokens:reserved"local":discardBind(
            namelist1:bind(function(names)
                return tokens:reservedOp"=":discardBind(explist1):optionMaybe():fmap(Stat.Local(names))
            end)
            :otherwise(parsel.sequence({
                tokens:reserved"function",  -- 1
                tokens.identifier,          -- 2
                funcbody                    -- 3
            }):fmap(function(list)
                return Stat.LocalFunction(list[2], list[3])
            end))
        )
    end

    -- Function
    local function functionStat()
        return tokens:reserved"function":discardBind(funcname:bind(function(name)
            return funcbody:fmap(Stat.Function(name))
        end))
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
            functionStat()
        })
    end
end

--------------------------------------------------
-- Namelist
--------------------------------------------------

function namelist()
    return namelist1:otherwise(parsel.from({}))
end

function namelist1()
    return tokens:commaSep1(tokens.identifier)
end

--------------------------------------------------
-- Varlist
--------------------------------------------------

function varlist()
    return varlist1:otherwise(parsel.from({}))
end

function varlist1()
    return tokens:commaSep1(var)
end

--------------------------------------------------
-- Explist
--------------------------------------------------

function explist()
    return explist1:otherwise(parsel.from({}))
end

function explist1()
    return tokens:commaSep1(exp)
end

--------------------------------------------------
-- Exp
--------------------------------------------------

do
    local function term()
        return parsel.choice({
            prefixexp,
            tokens:reserved"nil":discardBind(parsel.from(Expression.Nil)),
            tokens:reserved"true":discardBind(parsel.from(Expression.Boolean(true))),
            tokens:reserved"false":discardBind(parsel.from(Expression.Boolean(false))),
            tokens.float:try():otherwise(tokens.integer):fmap(Expression.Number),
            stringExp,
            func,
            tableconstructor,
            tokens:symbol"...":discardBind(parsel.from(Expression.Dots)),
        })
    end

    local ops = {
        {
            parsel.Operator.Infix(tokens:reservedOp"^":discardBind(parsel.from(BinOp.Pow)), parsel.Assoc.Right)
        },
        {
            parsel.Operator.Prefix(tokens:reserved"not":discardBind(parsel.from(UnaryOp.Not))),
            parsel.Operator.Prefix(tokens:reservedOp"#":discardBind(parsel.from(UnaryOp.Length))),
            parsel.Operator.Prefix(tokens:reservedOp"-":discardBind(parsel.from(UnaryOp.Negate))),
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"*":discardBind(parsel.from(BinOp.Multiply)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"/":discardBind(parsel.from(BinOp.Divide)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"%":discardBind(parsel.from(BinOp.Mod)), parsel.Assoc.Left)
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"+":discardBind(parsel.from(BinOp.Plus)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"-":discardBind(parsel.from(BinOp.Minus)), parsel.Assoc.Left)
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"..":discardBind(parsel.from(BinOp.Concat)), parsel.Assoc.Right)
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"<":discardBind(parsel.from(BinOp.LT)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp">":discardBind(parsel.from(BinOp.GT)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"<=":discardBind(parsel.from(BinOp.LE)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp">=":discardBind(parsel.from(BinOp.GE)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"~=":discardBind(parsel.from(BinOp.NotEquals)), parsel.Assoc.Left),
            parsel.Operator.Infix(tokens:reservedOp"==":discardBind(parsel.from(BinOp.Equals)), parsel.Assoc.Left)
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"and":discardBind(parsel.from(BinOp.And)), parsel.Assoc.Left)
        },
        {
            parsel.Operator.Infix(tokens:reservedOp"or":discardBind(parsel.from(BinOp.Or)), parsel.Assoc.Left)
        }
    }

    function exp()
        return term():expect"expression":buildExpressionParser(ops)
    end
end

--------------------------------------------------
-- StringExp
--------------------------------------------------

function longStringStart()
    return parsel.char"="
            :many()
            :between(parsel.char"[", parsel.char"[")
            :fmap(function(equals)
                return #equals
            end)
end

function longString()
    return longStringStart:bind(function(equals)
            return parsel.endOfLine
                    :optionMaybe()
                    :discardBind(
                        parsel.anyChar
                        :manyTill(
                            parsel.string(("="):rep(equals)):between(parsel.char"]", parsel.char"]"):try()
                        ):fmap(table.concat)
                    )
        end)
end

function stringExp()
    return tokens:stringLiteral"\""
            :otherwise(tokens:lexeme(longString))
            :fmap(Expression.String)
end

--------------------------------------------------
-- Prefixexp
--------------------------------------------------

function prefixexp()
    return parsel.choice({
        functioncall:try():fmap(PrefixExp.FunctionCall),
        tokens.identifier:fmap(PrefixExp.Name),
        tokens:parens(exp):fmap(PrefixExp.Expression)
    }):bind(function(a)
        return suffixexp:fmap(a)
    end)
end

function suffixexp()
    return index:many()
end

--------------------------------------------------
-- Tableconstructor
--------------------------------------------------

function tableconstructor()
    return tokens:braces(field:sepEndBy(fieldsep)):fmap(Expression.TableConstructor)
end

--------------------------------------------------
-- Field
--------------------------------------------------

function field()
    return tokens:brackets(exp):bind(function(expression)
        return tokens:symbol"=":discardBind(exp:fmap(Field.Indexed(expression)))
    end)
    :otherwise(
        tokens.identifier:bind(function(name)
            return tokens:symbol"=":discardBind(parsel.from(name))
        end):try():bind(function(name)
            return exp:fmap(Field.Named(name))
        end)
    )
    :otherwise(exp:fmap(Field.Expression))
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
        tokens:parens(exp):fmap(FunctionCall.Prefix.Expression)
    }):bind(function(a)
        return suffixexp:fmap(a)
    end)
end

--------------------------------------------------
-- Functioncall
--------------------------------------------------

function functioncall()
    return prefixcall:bind(function(pre)
        return fargs:fmap(FunctionCall.Function(pre))
                :otherwise(method:fmap(FunctionCall.Method(pre)))
                :bind(function(f)
                    return suffixcall:fmap(f)
                end)
    end)
end

--------------------------------------------------
-- Suffixcall
--------------------------------------------------

function suffixcall()
    return suffixexp:bind(function(suf)
        return fargs:fmap(FunctionCall.Suffix.Function(suf))
                :otherwise(method:fmap(FunctionCall.Suffix.Method(suf)))
                :many()
    end)
end

--------------------------------------------------
-- Method
--------------------------------------------------

function method()
    return tokens.colon:discardBind(tokens.identifier:bind(function(name)
        return fargs:fmap(Method.Method(name))
    end))
end

--------------------------------------------------
-- Index
--------------------------------------------------

function index()
    return tokens.dot:discardBind(tokens.identifier:fmap(Index.Name))
            :otherwise(tokens:brackets(exp):fmap(Index.Expression))
end

--------------------------------------------------
-- Fargs
--------------------------------------------------

function fargs()
    return parsel.choice({
        tokens:parens(explist),
        tableconstructor:bind(function(tbl) return parsel.from({tbl}) end),
        stringExp:bind(function(str) return parsel.from({str}) end)
    })
end

--------------------------------------------------
-- Func
--------------------------------------------------

function func()
    return tokens:reserved"function":discardBind(funcbody)
end

--------------------------------------------------
-- Funcbody
--------------------------------------------------

function funcbody()
    return tokens:parens(parlist):bind(function(parameters)
        return block:bind(function(bl)
            return tokens:reserved"end":discardBind(parsel.from(FunctionBody.FunctionBody(parameters, bl)))
        end)
    end)
end

--------------------------------------------------
-- Funcname
--------------------------------------------------

function funcname()
    return tokens.identifier:sepBy1(tokens.dot):bind(function(names)
        return tokens.colon:discardBind(tokens.identifier)
                :fmap(FunctionName.Method(names))
                :otherwise(parsel.from(FunctionName.Function(names)))
    end)
end

--------------------------------------------------
-- Parlist
--------------------------------------------------

function parlist()
    return parlist1:otherwise(parsel.from({}))
end

function parlist1()
    local parameter = tokens.identifier:fmap(Parameter.Variable)
                        :otherwise(
                            tokens:symbol"...":discardBind(
                                tokens.comma:notFollowedBy()
                            ):discardBind(parsel.from(Parameter.Vararg))
                        )
    return tokens:commaSep1(parameter)
end

--------------------------------------------------
-- Thunking
--------------------------------------------------

chunk               = parsel.fromThunk(parsel.thunk(chunk, "chunk"))
block               = parsel.fromThunk(parsel.thunk(block, "block"))
var                 = parsel.fromThunk(parsel.thunk(var, "var"))
stat                = parsel.fromThunk(parsel.thunk(stat, "stat"))
namelist            = parsel.fromThunk(parsel.thunk(namelist, "namelist"))
namelist1           = parsel.fromThunk(parsel.thunk(namelist1, "namelist1"))
varlist             = parsel.fromThunk(parsel.thunk(varlist, "varlist"))
varlist1            = parsel.fromThunk(parsel.thunk(varlist1, "varlist1"))
explist             = parsel.fromThunk(parsel.thunk(explist, "explist"))
explist1            = parsel.fromThunk(parsel.thunk(explist1, "explist1"))
exp                 = parsel.fromThunk(parsel.thunk(exp, "exp"))
longStringStart     = parsel.fromThunk(parsel.thunk(longStringStart, "longStringStart"))
longString          = parsel.fromThunk(parsel.thunk(longString, "longString"))
stringExp           = parsel.fromThunk(parsel.thunk(stringExp, "stringExp"))
prefixexp           = parsel.fromThunk(parsel.thunk(prefixexp, "prefixexp"))
suffixexp           = parsel.fromThunk(parsel.thunk(suffixexp, "suffixexp"))
tableconstructor    = parsel.fromThunk(parsel.thunk(tableconstructor, "tableconstructor"))
--fieldlist           = parsel.fromThunk(parsel.thunk(fieldlist, "fieldlist"))
field               = parsel.fromThunk(parsel.thunk(field, "field"))
fieldsep            = parsel.fromThunk(parsel.thunk(fieldsep, "fieldsep"))
prefixcall          = parsel.fromThunk(parsel.thunk(prefixcall, "prefixcall"))
functioncall        = parsel.fromThunk(parsel.thunk(functioncall, "functioncall"))
suffixcall          = parsel.fromThunk(parsel.thunk(suffixcall, "suffixcall"))
method              = parsel.fromThunk(parsel.thunk(method, "method"))
index               = parsel.fromThunk(parsel.thunk(index, "index"))
fargs               = parsel.fromThunk(parsel.thunk(fargs, "fargs"))
func                = parsel.fromThunk(parsel.thunk(func, "func"))
funcbody            = parsel.fromThunk(parsel.thunk(funcbody, "funcbody"))
funcname            = parsel.fromThunk(parsel.thunk(funcname, "funcname"))
parlist             = parsel.fromThunk(parsel.thunk(parlist, "parlist"))
parlist1            = parsel.fromThunk(parsel.thunk(parlist1, "parlist1"))

--------------------------------------------------
-- Lua
--------------------------------------------------

local Lua = chunk

--------------------------------------------------
-- File
--------------------------------------------------

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