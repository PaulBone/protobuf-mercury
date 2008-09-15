:- module read_defaults.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module protobuf_runtime.
:- import_module defaults.

:- import_module exception.
:- import_module list.
:- import_module pprint.
:- import_module stream.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( Args = [File] ->
        io.open_binary_input(File, OpenRes, !IO),
        ( OpenRes = ok(Stream) ->
            get(pb_reader(Stream, 100000), GetRes, !IO),
            ( GetRes = ok(pb_message(Message)),
                F1 = defaults_message_f1_or_default(Message),
                F2 = defaults_message_f2_or_default(Message),
                F3 = defaults_message_f3_or_default(Message),
                F4 = defaults_message_f4_or_default(Message),
                io.write(F1, !IO),
                io.nl(!IO),
                io.write(F2, !IO),
                io.nl(!IO),
                io.write(F3, !IO),
                io.nl(!IO),
                io.write(F4, !IO),
                io.nl(!IO)
            ; GetRes = error(_:pb_read_error(io.error)),
                throw(GetRes)
            ; GetRes = eof,
                throw(GetRes)
            )
        ;
            throw(OpenRes)
        )
    ;
        throw("expecting one arg")
    ),
    io.nl(!IO).
