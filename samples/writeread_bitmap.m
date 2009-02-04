:- module writeread_bitmap.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module protobuf_runtime.
:- import_module person.

:- import_module exception.
:- import_module maybe.
:- import_module list.

main(!IO) :-
    Person0 = person(
        1, "Peter Pan", gender_male, 1.8, yes("pp@neverland.org"), no,
            [
                person(2, "Peter Pan Jnr.", gender_male, 0.5, no,
                    yes(marital_status_single), [])
            ]),
    
    Bitmap = message_to_bitmap(Person0),

    bitmap_to_message(Bitmap, Res),
    (
	Res = ok(Person),
	( Person0 = Person ->
	    io.write_string("Peter Pan is the same Person\n", !IO)
	;
	    throw("Peter Pan is an imposter!\n")
	)
    ;
	Res = error(Error),
	throw(Error)
    ).
