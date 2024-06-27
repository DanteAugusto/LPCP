# run all files in problemas folder
alex Lexer.x
ghc ccure.hs

for file in $(ls problemas/*.ccr); do
    echo "---------------------------------------"
    echo "--> Running $file <--"
    ./ccure $file
    echo "---------------------------------------"
done