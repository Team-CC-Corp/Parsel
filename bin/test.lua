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
local Lua = chunk()