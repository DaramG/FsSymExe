LLVM_CONFIG?=../../../llvm/build/bin/llvm-config

LDFLAGS+=$(shell $(LLVM_CONFIG) --ldflags)
COMMON_FLAGS=-Wall -Wextra -g
CXXFLAGS+=$(COMMON_FLAGS) $(shell $(LLVM_CONFIG) --cxxflags) -fno-rtti
CPPFLAGS+=$(shell $(LLVM_CONFIG) --cppflags) -I$(SRC_DIR)
LLVMLIBS=$(shell $(LLVM_CONFIG) --libs irreader bitwriter passes core support)

all: libllvm.dll

libllvm.o: llvm.cpp
	$(CXX) -fPIC -c $(CPPFLAGS) $(CXXFLAGS) $(INCLUDE) $^ -o $@

libllvm.dll: libllvm.o
	$(CXX) -fPIC -shared -o $@ $^ $(CXXFLAGS) $(LDFLAGS) $(LLVMLIBS)

clean:
	rm -f libllvm.dll libllvm.o
