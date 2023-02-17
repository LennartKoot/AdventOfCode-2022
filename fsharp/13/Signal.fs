module DistressSignal.Signal

open FParsec

type Packet = PNumber of int
            | PList of Packet list

type Signal = Signal of (Packet * Packet) list

let private str s = pstring s
let private listBetweenString sOpen sClose sSep pElement f =
    between (str sOpen) (str sClose)
        (sepBy pElement (str sSep)) |>> f

let private ppacket, private ppacketRef = createParserForwardedToRef<Packet, unit>()

let private pnumber = pint32<unit> |>> PNumber
let private plist = listBetweenString "[" "]" "," ppacket PList

ppacketRef.Value <- choice [ pnumber ; plist ]

let private ppair = tuple2 (ppacket .>> newline) (ppacket .>> spaces)
let private psignal = many ppair

let parse s =
    match run psignal s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error

let parsePackets s = 
    let pAllPackets = many (ppacket .>> spaces)
    match run pAllPackets s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
