mkdir -p build

cd build

# Delete old version
if [ -f "./4ku-mini" ]; then
    rm ./4ku-mini
fi

# Minify the code
python3 ../minifier/minify.py ../src/main.cpp > ../src/main-mini.cpp

# Copy the source file
cp ../src/main-mini.cpp ../src/copy.cpp

# Compress the source copy
lzma ../src/copy.cpp

# Create build script
cat ../src/launcher.sh ../src/copy.cpp.lzma > ./4ku-mini

# Delete the source copy
rm ../src/copy.cpp.lzma

# Make script executable
chmod +x ./4ku-mini

# Print 4ku-mini file size
ls -l ./4ku-mini
