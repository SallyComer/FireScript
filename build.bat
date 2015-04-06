@echo off
echo "Building firescript interpreter executeable"
mkdir scratch
cp *.hs scratch
cd scratch
ghc -o firescript Main.hs -O
mv firescript.exe ..
cd ..
rm -r ./scratch/*
rmdir scratch
@echo on