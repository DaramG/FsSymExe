OUTDIR = $(shell pwd)/bin

all: FsSymExe

FsSymExe:
	make -C ./llvm
	dotnet build -c Release -o $(OUTDIR)

clean:
	dotnet clean
	rm -rf $(OUTDIR)
