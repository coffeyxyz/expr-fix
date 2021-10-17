all:
	csc -o xpr-fix -d0 src/*.scm

debug:
	csc -o xpr-fix src/*.scm
