mkdir -p build

cd build

rm ./4ku2-normal
rm ./4ku2-compressed


######################## Normal Version ########################

# Create build script
cat ../src/launcher.sh ../src/main.cpp > ./4ku2-normal

# Make script executable
chmod +x ./4ku2-normal

######################## Normal Version ########################


######################## Compressed Version ########################

# Copy the source file
cp ../src/main.cpp ../src/copy.cpp

# Compress the source copy
lzma ../src/copy.cpp

# Create build script
cat ../src/launcher-compressed.sh ../src/copy.cpp.lzma > ./4ku2-compressed

# Delete the source copy
rm ../src/copy.cpp.lzma

# Make script executable
chmod +x ./4ku2-compressed

######################## Compressed Version ########################
