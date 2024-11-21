let read_lines path =
    let ic = open_in path in
    let rec read_all_lines () =
        try
            let line = input_line ic in
            line :: read_all_lines ()
        with End_of_file ->
            close_in ic;
            []
    in
    read_all_lines ()

type action = On | Off | Toggle

type pos = {
    x : int;
    y : int;
}

let parse_pos (input : string) : pos =
    Scanf.sscanf input "%d,%d" (fun x y -> { x; y })

type range = {
    src : pos;
    dst : pos;
}

type inst = {
    action : action;
    range : range;
}

let parse_inst (input : string) : inst =
    match String.split_on_char ' ' input with
    | "toggle"        :: src :: "through" :: dst :: [] -> { action = Toggle; range = { src = parse_pos src; dst = parse_pos dst } }
    | "turn" :: "on"  :: src :: "through" :: dst :: [] -> { action = On;     range = { src = parse_pos src; dst = parse_pos dst } }
    | "turn" :: "off" :: src :: "through" :: dst :: [] -> { action = Off;    range = { src = parse_pos src; dst = parse_pos dst } }
    | _ -> raise (Invalid_argument input)

let in_range ({ x : int; y : int } : pos) ({ src : pos; dst : pos } : range) : bool =
    let lx = min src.x dst.x in
    let ly = min src.y dst.y in
    let hx = max src.x dst.x in
    let hy = max src.y dst.y in
    lx <= x && x <= hx && ly <= y && y <= hy

let part1 (insts : inst list) : int =
    let rec find_state (pos : pos) (insts : inst list) : bool =
        match insts with
        | [] -> false
        | { action; range } :: rest ->
            if not (in_range pos range) then
                find_state pos rest
            else
                if action == Toggle then
                    not (find_state pos rest)
                else
                    action == On
    in

    let insts = List.rev insts in

    let count = ref 0 in
    for x = 0 to 999 do
        for y = 0 to 999 do
            if find_state { x; y } insts then
                incr count
        done
    done;
    !count

let part2 (insts : inst list) : int =
    let brightness = function
        | On -> 1
        | Off -> -1
        | Toggle -> 2
    in

    let rec find_bright (pos : pos) (insts : inst list) (current : int) : int =
        match insts with
        | [] -> current
        | { action; range } :: rest ->
            if not (in_range pos range) then
                find_bright pos rest current
            else
                let bright = brightness action in
                max 0 (current + bright) |> find_bright pos rest
    in

    let total = ref 0 in
    for x = 0 to 999 do
        for y = 0 to 999 do
            let bright = find_bright { x; y } insts 0 in
            total := !total + bright
        done
    done;
    !total


let () =
    let insts = read_lines "./input.txt" |> List.map parse_inst in
    Printf.printf "part 1: %d\n" (part1 insts);
    flush stdout;
    Printf.printf "part 2: %d\n" (part2 insts);
