# run all files in problemas folder
alex Lexer.x
ghc Lexer.hs
ghc State.hs
ghc parsec.hs
for file in $(ls problemas/*.ccr); do
    echo "---------------------------------------"
    echo "--> Running $file <--"
    ./parsec $file
    echo "---------------------------------------"
done