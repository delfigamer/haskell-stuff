@@cls
@@ghc ^
    --make ^
    -O2 -msse2 ^
    -Werror -Wall ^
    -outputdir "Build" ^
    -tmpdir "Temp" ^
    -iSource ^
    -o testlua.exe ^
    Source\Test\Main.hs ^
    %*
@@ghc ^
    --make ^
    -O2 -msse2 ^
    -Werror -Wall ^
    -outputdir "Build" ^
    -tmpdir "Temp" ^
    -iSource ^
    -o lua.exe ^
    Source\Main.hs ^
    %*
