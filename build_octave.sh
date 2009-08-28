
rm -rf build/release
mkdir -p build/release
rm -f FFI/Octave/libCInterface.so

# Don't optimise your Haskell -O0 is needed
# if you use type polymorphism in function calls
# and you do!
ghc -O0 -ibuild/release -odir build/release -hidir build/release --make  FFI/Octave/OptionInterface.hs
 
ghc -O0 -ibuild/release -odir build/release -hidir build/release -no-hs-main --make -optl '-shared' FFI/Octave/CInterface.c ./build/release/FFI/Octave/OptionInterface_stub.o ./build/release/FFI/Octave/OptionInterface.o ./build/release/Maths/Prime.o ./build/release/Normal/Interface.o ./build/release/Normal/Acklam.o ./build/release/Normal/Framework.o ./build/release/Normal/BoxMuller.o ./build/release/FrameworkInterface.o ./build/release/MonteCarlo/Interface.o ./build/release/MonteCarlo/DataStructures.o ./build/release/MonteCarlo/Framework.o ./build/release/MonteCarlo/Lookback.o ./build/release/MonteCarlo/European.o ./build/release/Random/Interface.o ./build/release/Random/Ranq1.o ./build/release/Random/Halton.o ./build/release/Random/Framework.o -o FFI/Octave/libCInterface.so -package mtl -package array

cd FFI/Octave
rm -f *.oct 
mkoctfile -v hs_init.cpp -lCInterface -L.
mkoctfile -v hs_exit.cpp -lCInterface -L.
mkoctfile -v price_option.cpp -lCInterface -L. 

export LD_LIBRARY_PATH=.
octave -q OctaveTest.m



