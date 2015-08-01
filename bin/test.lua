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
    varlist,
    explist,
    exp,
    prefixexp,
    suffixexp,
    tableconstructor,
    fieldlist,
    field,
    fieldsep,
    functioncall,
    suffixcall,
    index,
    args,
    func,
    funcbody,
    funcname,
    parlist

--------------------------------------------------
-- Data types
--------------------------------------------------

local Var = {
    Name = parsel.cons(1),
    IndexedPrefix = parsel.cons(2)
}

local Stat = {
    Do              = parsel.cons(1),   -- block
    Assignment      = parsel.cons(2),   -- varlist, explist
    While           = parsel.cons(2),   -- exp, block
    Repeat          = parsel.cons(2),   -- block, exp
    If              = parsel.cons(4),   -- exp, block, {ElseIf}, [block]
    Return          = parsel.cons(1),   -- [explist]
    Break           = parsel.cons(),
    For             = parsel.cons(5),   -- Name, exp, exp, [exp], block
    ForIn           = parsel.cons(3),   -- namelist, explist, block
    FunctionCall    = parsel.cons(1),   -- functioncall
    Local           = parsel.cons(2),   -- namelist, [explist]
    Function        = parsel.cons(2),   -- funcname, funcbody
    LocalFunction   = parsel.cons(2),   -- Name, funcbody
}

local ElseIf = {
    ElseIf = parsel.cons(2) -- exp, block
}

--------------------------------------------------
-- Chunk, block
--------------------------------------------------

function chunk()
    return stat:bind(function(statement)
        return tokens:symbol";":discardBind(statement)
    end):many()
end

block = chunk

--------------------------------------------------
-- Var
--------------------------------------------------

function var()
    return tokens.identifier:bind(Var.Name)
    :otherwise(prefixexp:bind(function(prefix)
        return index:fmap(Var.IndexedPrefix(prefix))
    end))
end

--------------------------------------------------
-- Stat
--------------------------------------------------

do
    -- Do
    local doStat = tokens:reserved"do":discardBind(block():fmap(Stat.Do):bind(function(d)
        return tokens:reserved"end":discardBind(from(d))
    end))

    -- Assignment
    local assignmentStat = varlist:bind(function(vars)
        return tokens:reservedOp"=":discardBind(explist:fmap(Stat.Assignment(vars)))
    end):try()

    -- While
    local whileStat = tokens:reserved"while":discardBind(exp():bind(function(expression)
        return tokens:reserved"do":discardBind(block():fmap(Stat.While(expression)):bind(function(wh)
            return tokens:reserved"end":discardBind(from(wh))
        end))
    end))

    -- Repeat
    local repeatStat = tokens:reserved"repeat":discardBind(block():bind(function(bl)
        return tokens:reserved"until":discardBind(exp():fmap(function(expression)
            return Stat.Repeat(bl, expression)
        end))
    end))

    -- If
    local elseIf = tokens:reserved"elseif":discardBind(exp():bind(function(expression)
        return tokens:reserved"then":discardBind(block():fmap(ElseIf.ElseIf(expression)))
    end))

    local ifStat = parsel.sequence({
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

    -- Return
    local returnStat = tokens:reserved"return":discardBind(explist:try():optionMaybe()):fmap(Stat.Return)

    -- Break
    local breakStat = tokens:reserved"break":discardBind(parsel.from(Stat.Break))

    -- For
    local forStat = parsel.sequence({
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

    -- ForIn
    local forInStat = parsel.sequence({
        tokens:reserved"for",   -- 1
        namelist,               -- 2
        tokens:reserved"in",    -- 3
        explist,                -- 4
        tokens:reserved"do",    -- 5
        block(),                -- 6
        tokens:reserved"end"    -- 7
    }):fmap(function(list)
        return Stat.ForIn(list[2], list[4], list[6])
    end)
end

local Lua = chunk()