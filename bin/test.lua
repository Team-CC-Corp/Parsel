local parsel = grin.getPackageAPI("Team-CC-Corp/Parsel", "parsel")
local ok, val, cs = parsel.char"a":many():apply("aaa")
print(ok,":",val[1],":",cs)