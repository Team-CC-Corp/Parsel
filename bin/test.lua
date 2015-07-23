local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")
local ok, val, cs = parsel.char"a":otherwise(parsel.char"b"):otherwise(parsel.string"test"):many1():apply(...)
print(ok,":",textutils.serialize(val),":",cs)