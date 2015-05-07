%% compile with `mmc --make hello`

:- module hello.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

%main(IOState_in, IOState_out) :-
%    io.write_string("Hello, World!\n", IOState_in, IOState_out). 

main(!IO) :-
    io.write_string("Hello, ", !IO),
    io.write_string("world!", !IO),
    io.nl(!IO).
