all: llvm-project build/bin/llvm-config

a1caa302970de86b15d360212b526be8f1d59641.zip:
	wget https://github.com/llvm/llvm-project/archive/a1caa302970de86b15d360212b526be8f1d59641.zip

cmake:
	mkdir -p cmake
	cd cmake && wget https://github.com/Kitware/CMake/releases/download/v3.21.1/cmake-3.21.1-linux-x86_64.sh && sh cmake-3.21.1-linux-x86_64.sh

llvm-project: a1caa302970de86b15d360212b526be8f1d59641.zip cmake
	unzip a1caa302970de86b15d360212b526be8f1d59641.zip
	mv llvm-project-a1caa302970de86b15d360212b526be8f1d59641 llvm-project

build/bin/llvm-config: llvm-project
	mkdir -p build
	cd build && ../cmake/cmake-3.21.1-linux-x86_64/bin/cmake ../llvm-project/llvm
	cd build && ../cmake/cmake-3.21.1-linux-x86_64/bin/cmake --build . -- -j 20

clean:
	rm -rf build
