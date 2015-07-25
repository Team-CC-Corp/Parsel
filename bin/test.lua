local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")
do
    local a = parsel.symbol"a"
    local b = parsel.symbol"b"
    local test = parsel.symbol"test"
    local abtest = a:otherwise(b):otherwise(test)
    local p = abtest:many():bind(function(a)
        return abtest:sepEndBy1(parsel.symbol","):between(parsel.symbol"{", parsel.symbol"}"):bind(function(a_inner)
            return parsel.from({outer=a,inner=a_inner})
        end)
    end)
    local ok, val = p:parse((...))
    if not ok then
        printError(val)
    else
        print(textutils.serialize(val))
    end
end


do
    local function fplus(a, b)
        return {op="add", a=a, b=b}
    end
    local plus = parsel.infixOperator(parsel.symbol"+":discardBind(parsel.from(fplus)), parsel.Assoc.Left)
    local highestPrecedence = {plus}
    local t = {highestPrecedence}

    local exp = parsel.digit:buildExpressionParser(t)
    local ok, val = exp:parse("1+2+4+1")
    if not ok then
        printError(val)
    else
        print(textutils.serialize(val))
    end
end