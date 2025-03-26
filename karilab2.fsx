let punkt = 12, 13, 14 
let x, y, z = punkt
let pobierzX (x, y, z) = x 
let pobierz x y z = x 
let pobierzY (x, y, z) = y 
let pobierzZ (x, y, z) = z 
let wczytajPare() =
    let x = int (System.Console.ReadLine())
    let y = int (System.Console.ReadLine())
    (x, y)

type Punkt3D = {
    X: int
    Y: int
    Z: int
}
let punkt3D = {X=1; Y=2; Z = 3}
punkt3D.X
    
type Wynik = 
 |   Sukces of int
 |   Blad of string
let podziel a b =
    if b<> 0 then
        Sukces (a/b)
    else
        Blad "dzielenie przez zero"
let wynik = podziel 10 2
match wynik with
| Sukces x -> printfn "Wynik: %d" x
| Blad s -> printfn "Blad: %s" s

let pokazWynik wynik =
    match wynik with
    | Sukces x -> printfn "Wynik: %d" x
    | Blad s -> printfn "Blad: %s" s
pokazWynik (podziel 10 2)

type RRKwadratowego =
| Brak
| Jedno of float
| Dwa of float * float
let pierwiastkiRownania a b c =
    let delta = b*b - 4.0*a*c
    if delta <0.0 then
        Brak
    elif delta = 0.0 then
        Jedno( -b / (2.0*a))
    else 
        let x1 = (-b + sqrt delta)/(2.0*a)
        let x2 = (-b - sqrt delta)/(2.0*a)
        Dwa(x1, x2)
let rozwiazanie = pierwiastkiRownania 1 4 2
printfn "Pierwiastki: %A" rozwiazanie

let wyn =  pierwiastkiRownania 1 4 2
match wyn with
| Brak -> printfn "Brak wyniku"
| Jedno x -> printfn "Jedno %f" x
| Dwa y z -> printfn "Dwa: %f %f" y z

let pokaz wyn =  pierwiastkiRownania 1 4 2
    match wyn with
    | Brak -> printfn "Brak wyniku"
    | Jedno x -> printfn "Jedno %f" x
    | Dwa y z -> printfn "Dwa: %f %f" y z
pokaz(pierwiastkiRownania)