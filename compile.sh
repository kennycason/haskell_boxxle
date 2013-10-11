#!/bin/bash

rm bin/Boxxle

ghc -threaded -O0 -o bin/Boxxle *.hs 

rm -f *.hi
rm -f *.o

bin/Boxxle
