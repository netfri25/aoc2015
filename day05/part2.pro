main :-
    file_lines("./input.txt", Words),
    bench(include(nice, Words, NiceWords), Tms),
    length(NiceWords, Len),
    format("~d (~4fms)", [Len, Tms]), nl,
    halt.

nice(Word) :-
    repeats_with_sep(Word),
    repeated_pair(Word),
    !.

repeats_with_sep(Word) :-
    string_chars(Word, Chars),
    Seq = [X, _, X],
    append([_, Seq, _], Chars),
    !.

repeated_pair(Word) :-
    string_chars(Word, Chars),
    length(Pair, 2),
    append([_, Pair, _, Pair, _], Chars),
    !.

file_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
       stream_lines(In, Lines),
       close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).

bench(G, Tms) :-
   T0 is cputime,
   G,
   T1 is cputime,
   T is T1 - T0,
   Tms is T * 1000.
