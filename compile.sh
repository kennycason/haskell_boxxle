#!/bin/bash

rm bin/Boxxle

ghc -threaded -o bin/Boxxle *.hs 

rm -f *.hi
rm -f *.o

bin/Boxxle
