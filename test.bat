rd test /S/Q
md test
cd binding-core
cabal sdist
move dist\*.tar.gz ..\test
cd ..\binding-gtk
cabal sdist
move dist\*.tar.gz ..\test
cd ..\binding-wx
cabal sdist
move dist\*.tar.gz ..\test
cd ..\test
7z x *.tar.gz
7z x *.tar
cd binding-core*
cabal configure --enable-tests
cabal build
echo * testing binding-core ... >..\log.txt
cabal test >>..\log.txt
cabal install
cd ..\binding-gtk*
cabal configure --enable-tests
cabal build
echo * testing binding-gtk ... >>..\log.txt
cabal test >>..\log.txt
cd ..\binding-wx*
cabal configure --enable-tests
cabal build
echo * testing binding-wx ... >>..\log.txt
cabal test >>..\log.txt
cd ..
