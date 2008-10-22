:- module writeread_person.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module protobuf_runtime.
:- import_module person.

:- import_module bitmap.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module maybe.
:- import_module list.
:- import_module stream.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = [File] ->
        io.open_binary_output(File, OpenRes, !IO),
        ( OpenRes = ok(Out) ->
            write_person(Out, !IO),
            close_binary_output(Out, !IO),
            io.open_binary_input(File, OpenRes2, !IO),
            ( OpenRes2 = ok(In) ->
                read_person(In, Person, !IO),
                io.write(Person, !IO)
            ;
                throw(OpenRes2)
            )
        ;
            throw(OpenRes)
        )
    ;
        throw("expecting one arg")
    ),
    io.nl(!IO).

:- pred write_person(S::in, io::di, io::uo) is det
    <= ( stream.writer(S, byte, io) ).

write_person(Stream, !IO) :-
    Person = person(
        1, "Peter Pan", gender_male, 1.8, yes("pp@neverland.org"), no,
            [
                person(2, "Peter Pan Jnr.", gender_male, 0.5, no,
                    yes(marital_status_single), [])
            ]),
    put(pb_writer(Stream), pb_message(Person), !IO).

:- pred read_person(S::in, person::out, io::di, io::uo) is det
    <= ( stream.reader(S, byte, io, E) ).

read_person(Stream, Person, !IO) :-
    get(pb_reader(Stream, 10000), GetRes, !IO),
    ( GetRes = ok(pb_message(Person))
    ; GetRes = error(_),
        throw(GetRes)
    ; GetRes = eof,
        throw(GetRes)
    ).
