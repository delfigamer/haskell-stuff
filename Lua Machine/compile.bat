@@cls
@@ghc ^
    --make -c ^
    -O2 -msse2 ^
    -Werror -Wall ^
    -outputdir "Build" ^
    -tmpdir "Temp" ^
    -iSource ^
    Source\Main.hs ^
    %*
