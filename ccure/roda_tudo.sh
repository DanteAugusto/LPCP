# run all files in problemas folder

for file in $(ls problemas/*.ccr); do
    echo "---------------------------------------"
    echo "--> Running $file <--"
    ./parsec $file
    echo "---------------------------------------"
done