


main :-
    file_lines("./input.txt", Words),
    bench(include(nice, Words, NiceWords), Tms),
    length(NiceWords, Len),
    format("~d (~4fms)", [Len, Tms]), nl,
    halt.

nice(Word) :-
    three_vowels(Word),
    double_letter(Word),
    \+ banned_strings(Word),
    !.

three_vowels(Word) :-
    string_chars(Word, Chars),
    string_chars("aeiou", Vowels),
    length(Vs, 3),
    foldl([V, In, Out]>>(member(V, Vowels), selectchk(V, In, Out)), Vs, Chars, _),
    !.

double_letter(Word) :-
    string_chars(Word, Chars),
    Chars = [_|Tail],
    append(Init, [_], Chars),
    % `foldl or (==)`  is the same as  `not (foldl and (\=))`
    \+ maplist(\=, Init, Tail),
    !.

banned_strings(Word) :-
    string_chars(Word, Chars),
    maplist(string_chars, ["ab", "cd", "pq", "xy"], BannedCharsList),
    member(BannedChars, BannedCharsList),
    append([_, BannedChars, _], Chars),
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
