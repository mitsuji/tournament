# simple tournament model by (procedural, functional) * (C++, Haskell)

## run

$ stack runhaskell app/Functional.hs

$ stack runhaskell app/Procedural.h


## build

$ stack build


## install

$ stack install


## execute

$ tournament-func-exe

$ tournament-proc-exe


## build c++ version

$ g++ -o tournament-cxx-func-exe -std=c++98 app_cxx/Functional.cxx

$ clang++ -o tournament-cxx-func-exe -std=c++98 app_cxx/Functional.cxx

$ g++ -o tournament-cxx-proc-exe -std=c++98 app_cxx/Procedural.cxx

$ clang++ -o tournament-cxx-proc-exe -std=c++98 app_cxx/Procedural.cxx


## execute c++ version

$ ./tournament-cxx-func-exe

$ ./tournament-cxx-proc-exe