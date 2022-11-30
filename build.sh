mkdir -p build

cd build

rm ./4ku-executable
rm ./4ku-executable-mini
rm ./4ku-normal
rm ./4ku-compressed
rm ./4ku-normal-mini
rm ./4ku-compressed-mini


######################## Minify Code ########################

python3 ../minifier/minify.py ../src/main.cpp > ../src/main-mini.cpp

######################## Minify Code ########################


c++ ../src/main.cpp -O3 -Wall -Wextra -Wconversion -Wno-misleading-indentation -pthread -o 4ku-executable
c++ ../src/main-mini.cpp -O3 -Wall -Wextra -Wconversion -Wno-misleading-indentation -pthread -o 4ku-executable-mini


######################## Normal Version ########################

# Create build script
cat ../src/launcher.sh ../src/main.cpp > ./4ku-normal

# Make script executable
chmod +x ./4ku-normal

######################## Normal Version ########################



######################## Normal Mini Version ########################

# Create build script
cat ../src/launcher.sh ../src/main-mini.cpp > ./4ku-normal-mini

# Make script executable
chmod +x ./4ku-normal-mini

######################## Normal Mini Version ########################



######################## Compressed Version ########################

# Copy the source file
cp ../src/main.cpp ../src/copy.cpp

# Compress the source copy
lzma ../src/copy.cpp

# Create build script
cat ../src/launcher-compressed.sh ../src/copy.cpp.lzma > ./4ku-compressed

# Delete the source copy
rm ../src/copy.cpp.lzma

# Make script executable
chmod +x ./4ku-compressed

######################## Compressed Version ########################



######################## Compressed Mini Version ########################

# Copy the source file
cp ../src/main-mini.cpp ../src/copy.cpp

# Compress the source copy
lzma ../src/copy.cpp

# Create build script
cat ../src/launcher-compressed.sh ../src/copy.cpp.lzma > ./4ku-compressed-mini

# Delete the source copy
rm ../src/copy.cpp.lzma

# Make script executable
chmod +x ./4ku-compressed-mini

######################## Compressed Mini Version ########################
