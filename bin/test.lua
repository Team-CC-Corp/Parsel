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

local Var = {
    Name = parsel.cons(1),
    IndexedPrefix = parsel.cons(2)
}

function chunk()
    return stat:bind(function(statement)
        return tokens:symbol";":discardBind(statement)
    end):many()
end

block = chunk

function var()
    return tokens.identifier:bind(function(name)
        return Var.Name(name)
    end)
    :otherwise(prefixexp:bind(function(prefix)
        return index:fmap(Var.IndexedPrefix(prefix))
    end))
end
local Lua = chunk()