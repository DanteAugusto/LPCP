# run all files in problemas folder

for file in $(ls problemas/*.ccr); do
    echo "Running $file"
    ./parsec $file
done