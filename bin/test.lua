local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")
local a = parsel.symbol"a"
local b = parsel.symbol"b"
local test = parsel.symbol"test"
local abtest = a:otherwise(b):otherwise(test)
local p = abtest:many():bind(function(a)
    return abtest:between(parsel.symbol"{", parsel.symbol"}"):bind(function(a_inner)
        return parsel.from({outer=a,inner=a_inner})
    end)
end)
local ok, val = p:parse((...))
if not ok then
    printError(val)
else
    print(textutils.serialize(val))
end