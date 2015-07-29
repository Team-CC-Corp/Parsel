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