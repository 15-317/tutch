
all:
	@echo "Tutch:"
	@echo "Run 'make smlnj' or Linux/Unix/OSX."
	@echo "Run 'make win+smlnj' on Windows."

SMLNJ=`which sml`
DEST=`pwd`/bin

# SML/NJ
.PHONY : smlnj
smlnj:
	cd src; sml export-smlnj.sml
	mv src/tutch-heapimg.amd64-linux $(DEST)
	bin/mknjexec-unixey $(SMLNJ) $(DEST) tutch-heapimg.amd64-linux tutch

# Windows + SML/NJ 
.PHONY : win+smlnj
win+smlnj:
	cd src; sml export-smlnj.sml
	mv src/tutch-heapimg.x86-win32 $(DEST)
	bin/mknjexec-win $(SMLNJ) $(DEST) tutch-heapimg.x86-win32 tutch
