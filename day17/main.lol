CAN HAS FILE?
CAN HAS STRING?

O HAI IM List
    I HAS A len ITZ A NUMBR

    HOW IZ I push YR number
        ME HAS A SRS ME'Z len ITZ number
        ME'Z len R SUM OF ME'Z len AN 1
    IF U SAY SO

    HOW IZ I get YR index
        FOUND YR ME'Z SRS index
    IF U SAY SO

    HOW IZ I to_string
        I HAS A output ITZ "["
        IM IN YR loop UPPIN YR i TIL BOTH SAEM i AN ME'Z len
            output R SMOOSH output AN " " AN ME IZ get YR i MKAY
        IM OUTTA YR loop
        FOUND YR SMOOSH output AN " ]"
    IF U SAY SO
KTHX

HOW IZ I read_input YR path
    I HAS A file ITZ FILE IZ OPEN YR path MKAY
    NOT file, O RLY?, YA RLY
        INVISIBLE SMOOSH "no file `" AN path AN "` ::("
        FOUND YR NOOB
    OIC

    I HAS A content ITZ FILE IZ GETZ YR file MKAY
    O HAI IM result IM LIEK List
        I HAS A len ITZ 0
    KTHX

    IM IN YR number_parsing_loop TIL BOTH SAEM 0 AN STRING IZ LEN YR content MKAY
        I HAS A index ITZ I IZ newline_index YR content MKAY
        I HAS A number_str ITZ STRING IZ SLICE YR content AN YR 0 AN YR index MKAY
        result IZ push YR MAEK number_str A NUMBR
        content R STRING IZ SLICE YR content AN YR SUM OF index AN 1 AN YR STRING IZ LEN YR content MKAY MKAY
    IM OUTTA YR number_parsing_loop

    FOUND YR result
IF U SAY SO

HOW IZ I part1 YR slice AN YR index AN YR target
    BOTH SAEM target AN 0, O RLY?, YA RLY, FOUND YR 1, OIC

    EITHER OF  BOTH SAEM target AN SMALLR OF 0 AN target  AN  BOTH SAEM index AN BIGGR OF index AN slice'Z len
    O RLY?, YA RLY, FOUND YR 0, OIC

    I HAS A item ITZ slice IZ get YR index MKAY
    I HAS A new_index ITZ SUM OF index AN 1
    I HAS A new_target ITZ DIFF OF target AN item
    I HAS A lhs ITZ I IZ part1 YR slice AN YR new_index AN YR target MKAY
    I HAS A rhs ITZ I IZ part1 YR slice AN YR new_index AN YR new_target MKAY
    FOUND YR SUM OF lhs AN rhs
IF U SAY SO

HOW IZ I part2 YR slice AN YR index AN YR target AN YR to_use
    BOTH SAEM target AN 0, O RLY?, YA RLY, FOUND YR 1, OIC

    ANY OF  BOTH SAEM target AN SMALLR OF 0 AN target  AN  BOTH SAEM index AN BIGGR OF index AN slice'Z len  AN  BOTH SAEM to_use AN 0  MKAY
    O RLY?, YA RLY, FOUND YR 0, OIC

    I HAS A item ITZ slice IZ get YR index MKAY
    I HAS A new_index ITZ SUM OF index AN 1
    I HAS A new_target ITZ DIFF OF target AN item
    I HAS A new_to_use ITZ DIFF OF to_use AN 1
    I HAS A lhs ITZ I IZ part2 YR slice AN YR new_index AN YR target AN YR to_use MKAY
    I HAS A rhs ITZ I IZ part2 YR slice AN YR new_index AN YR new_target AN YR new_to_use MKAY
    FOUND YR SUM OF lhs AN rhs
IF U SAY SO

HOW IZ I min_containers YR slice AN YR index AN YR target
    BOTH SAEM target AN 0, O RLY?, YA RLY, FOUND YR 0, OIC

    EITHER OF  BOTH SAEM target AN SMALLR OF 0 AN target  AN  BOTH SAEM index AN BIGGR OF index AN slice'Z len
    O RLY?, YA RLY, FOUND YR slice'Z len, OIC BTW biggest possible answer

    I HAS A item ITZ slice IZ get YR index MKAY
    I HAS A new_index ITZ SUM OF index AN 1
    I HAS A new_target ITZ DIFF OF target AN item
    I HAS A lhs ITZ I IZ min_containers YR slice AN YR new_index AN YR target MKAY
    I HAS A rhs ITZ SUM OF 1 AN I IZ min_containers YR slice AN YR new_index AN YR new_target MKAY
    FOUND YR SMALLR OF lhs AN rhs
IF U SAY SO

HOW IZ I main
    I HAS A input ITZ I IZ read_input YR "input.txt" MKAY
    I HAS A target ITZ 150
    I HAS A sol1 ITZ I IZ part1 YR input AN YR 0 AN YR target MKAY
    VISIBLE SMOOSH "part 1:: " AN sol1
    I HAS A to_use ITZ I IZ min_containers YR input AN YR 0 AN YR target MKAY
    VISIBLE SMOOSH "to use:: " AN to_use
    I HAS A sol2 ITZ I IZ part2 YR input AN YR 0 AN YR target AN YR to_use MKAY
    VISIBLE SMOOSH "part 2:: " AN sol2
IF U SAY SO

HOW IZ I newline_index YR str
    I HAS A index ITZ 0
    IM IN YR loop UPPIN YR index TIL EITHER OF BOTH SAEM index AN STRING IZ LEN YR str MKAY AN BOTH SAEM ":)" AN STRING IZ AT YR str AN YR index MKAY
    IM OUTTA YR loop
    FOUND YR index
IF U SAY SO

I IZ main MKAY
