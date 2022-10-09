echo "Creating and building tests file"
cat test.asm graphic.asm > test_full.asm
spasm -E -T test_full.asm bin/TEST.8xp
echo Building tetrice_dat.asm
spasm -E -T tetrice_dat.asm bin/CETRISDT.8xp
tools/convbin -j 8x -k 8xv -i bin/CETRISDT.8xp -o bin/CETrisDT.8xv -n CETrisDT
echo Creating and building CETRIS
cat tetrice.asm graphic.asm > cetris.asm
spasm -E -T cetris.asm bin/CETRIS.8xp
echo Building debug info
spasm -E -L cetris.asm

