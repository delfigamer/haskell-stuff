@@cls
@@ghc ^
    --make ^
    -O2 -msse2 ^
    -Werror -Wall ^
    -outputdir "Build" ^
    -tmpdir "Temp" ^
    -iSource ^
    -o testlua.exe ^
    Source\Main.hs ^
    %*
