all: ../functiongrapher/favicon.ico

../functiongrapher/favicon.ico: favicon.ico
	cp favicon.ico ../functiongrapher/

favicon.ico: MakeIcon
	./MakeIcon > favicon.ico

MakeIcon: MakeIcon.hs
	ghc -O3 --make MakeIcon
	touch MakeIcon
