mkdir -p build

cd build

# Delete old version
if [ -f "./4ku-mini" ]; then
    rm ./4ku-mini
fi

# Minify the code
python3 ../minifier/minify.py ../src/main.cpp > ../src/main-mini.cpp

# Compress the source copy
echo Finding best compression parameters...
SMALLEST=1000000
LAUNCHER_SIZE=$(stat -c%s "../src/launcher.sh")
for MF in hc3 hc4 bt2 bt3 bt4
do
	for NICE in {4..273}
	do
		lzma -f -k --lzma1=preset=9,lc=0,lp=0,pb=0,mf=$MF,nice=$NICE ../src/main-mini.cpp
		FILESIZE=$(stat -c%s "../src/main-mini.cpp.lzma")
		if [ "$FILESIZE" -lt "$SMALLEST" ]; then
			echo mf=$MF nice=$NICE size=$(($LAUNCHER_SIZE + $FILESIZE))
			cp -f ../src/main-mini.cpp.lzma ../src/main-mini-smallest.cpp.lzma
			SMALLEST=$FILESIZE
		fi
	done
done

# Create build script
cat ../src/launcher.sh ../src/main-mini-smallest.cpp.lzma > ./4ku-mini

# Delete compressed sources
rm -f ../src/main-mini.cpp.lzma
rm -f ../src/main-mini-smallest.cpp.lzma

# Make script executable
chmod +x ./4ku-mini

# Print 4ku-mini file size
ls -l ./4ku-mini
