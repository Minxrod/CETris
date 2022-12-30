echo "Creating and building tests file"
cat test.asm graphic.asm > test_full.asm
spasm -E -T test_full.asm bin/TEST.8xp
echo Building tetrice_dat.asm
# version information
cat tetrice_dat.asm > cetrisdt.asm
echo -e "versionString:\n .db \""${1:-$(date -u +%H:%M)}"\",0\nversionStringEnd:\nversionStringSize = versionStringEnd-versionString" >> cetrisdt.asm
spasm -E -T cetrisdt.asm bin/CETRISDT.8xp
spasm -E -L cetrisdt.asm
tools/convbin -j 8x -k 8xv -i bin/CETRISDT.8xp -o bin/CETrisDT.8xv -n CETrisDT
echo Creating and building CETRIS
cat tetrice.asm graphic.asm > cetris.asm
if [ $2 ]; then
	spasm -E -T -DMETA=$1 cetris.asm bin/CETRIS.8xp
else
	spasm -E -T cetris.asm bin/CETRIS.8xp
fi

spasm -E -L cetris.asm
spasm -E -T cetris.asm #generates .lst file

ls -l bin/CETRIS.8xp bin/CETrisDT.8xv # for filesizes
