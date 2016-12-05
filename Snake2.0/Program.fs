module Snake

open System.Diagnostics

[<System.Runtime.InteropServices.DllImport("User32.dll")>]
extern int16 GetAsyncKeyState(int vk);

//Virtual Keys
type VK = Enter=0x0D | Esc=0x1B | Left=0x25 | Up=0x26 | Right=0x27 | Down=0x28
let isKeyPressed (key:VK) =
    GetAsyncKeyState(int key) > int16 0

type Point = int*int
type Snake = list<Point>
type Cookies = list<Point>
type Direction = int*int

type GameState = {
    Snake:Snake;
    Cookies:Cookies;
    Direction:Direction;
    Points:int;
    Frames:int
}

[<Literal>]
let W = 35
[<Literal>]
let H = 20
[<Literal>]
let Fps = 3
[<Literal>]
let CookieChance = 10
[<Literal>]
let CookiePoints = 100
[<Literal>]
let PointsToWin = 5000

let (defaultSnake:Snake) = [ (3,1); (2,1); (1,1) ]
let (defaultCookies:Cookies) = [ (20,5) ]
let (defaultDirection:Direction) = (1, 0)

let DefaultGameState : GameState = {
    Snake = defaultSnake;
    Cookies = defaultCookies;
    Direction = defaultDirection;
    Points = 0;
    Frames = 0
}

let printGameOver() = 
    System.Console.WriteLine("\n" + 
        String.replicate (W/2-4) "=" + "GAMEOVER" + 
        String.replicate (W/2-4) "="  + "\nPress Enter to play again\n")

let printPaused() = 
    System.Console.WriteLine("\n" + 
        String.replicate (W/2-3) "=" + "PAUSED" + 
        String.replicate (W/2-3) "=" + "\nPress Esc to resume\n")

let rand = new System.Random()

let tupleAdd (t1:(int*int)) (t2:(int*int)) : (int*int)=
    let (a',b') = t1
    let (c',d') = t2
    (a'+c', b'+d')

let tupleSub (t1:(int*int)) (t2:(int*int)) : (int*int) =
    let (a',b') = t1
    let (c',d') = t2
    (c'-a', d'-b')

let tupleEq (t1:(int*int)) (t2:(int*int)) : bool =
    let (a',b') = t1
    let (c',d') = t2
    a'=c' && b'=d'

let drawGameoverScreen() =
    printGameOver()
    while not(isKeyPressed(VK.Enter)) do
        System.Threading.Thread.Sleep(200)
    done

let drawPauseScreen() =
    printPaused()
    while not(isKeyPressed(VK.Esc)) do
        System.Threading.Thread.Sleep(200)
    done

let gameDraw (gs:GameState) : GameState =
    let getConsoleUpdateString head =
        let (headx, heady) = head
        let rec setConsoleChar acc i j hx hy =
                match i, j with
                | _, H -> (acc+(String.replicate (W+1) "+") + "\n")
                | W, _ -> setConsoleChar (acc+"+\n") 0 (j+1) hx hy
                | 0, 0 | 0, _ | _, 0 -> setConsoleChar (acc+"+") (i+1) j hx hy
                | _ when i=hx && j=hy -> setConsoleChar (acc+"H") (i+1) j hx hy
                | _ when List.contains (i, j) gs.Snake -> setConsoleChar (acc+"o") (i+1) j  hx hy
                | _ when List.contains (i, j) gs.Cookies -> setConsoleChar (acc+"c") (i+1) j  hx hy
                | _ -> setConsoleChar (acc+" ") (i+1) j  hx hy
        setConsoleChar "" 0 0 headx heady

    let stringBuffer = getConsoleUpdateString gs.Snake.Head
    let nf = 1 + gs.Frames
    System.Console.Clear()
    System.Console.Write(stringBuffer)
    System.Console.WriteLine("Points: " + gs.Points.ToString() + "\n")
    System.Console.WriteLine("Frames: " + nf.ToString() + "\n")
    {
        Snake = gs.Snake;
        Cookies= gs.Cookies;
        Direction  = gs.Direction;
        Points = gs.Points;
        Frames = nf
    }

let rec gameUpdate (gs:GameState) : GameState =
    let (x,y) = tupleAdd gs.Snake.Head gs.Direction

    let newCookies = 
        match rand.Next CookieChance with
        | 1 -> (rand.Next(1, W), rand.Next(1, H))::gs.Cookies
        | _ -> gs.Cookies

    match (x,y) with
    | 0, _ | _, 0 | W, _ | _, H -> //When snake hit a wall
        drawGameoverScreen()
        gameUpdate DefaultGameState
    | _ when List.contains (x,y) gs.Snake.Tail ->  //When snake ate itself
        drawGameoverScreen()
        gameUpdate DefaultGameState
    | _ when List.contains (x,y) newCookies -> //When snake ate cookie
        let newSnake = (x,y)::gs.Snake
        let newPoints = gs.Points + CookiePoints
        {
            Snake = newSnake;
            Cookies = List.where (fun z -> z <> (x,y)) newCookies
            Direction = gs.Direction;
            Points = newPoints;
            Frames = gs.Frames
        }
    | _ -> //When nothing interesting happens
        {
            Snake = (x,y)::List.take (gs.Snake.Tail.Length) gs.Snake;
            Cookies = newCookies;
            Direction = gs.Direction;
            Points = gs.Points;
            Frames = gs.Frames
        }

    

let checkMovement oldDirection newDirection : Direction = 
    let velocity = tupleAdd newDirection oldDirection
    let isValidDirection = not(tupleEq velocity (0,0))
    match isValidDirection with
    | true -> newDirection
    | false -> oldDirection


let checkKey vk oldDirection newDirection : Direction = 
    if isKeyPressed(vk) then
        checkMovement oldDirection newDirection
    else
        oldDirection


let checkRightKey oldDirection = checkKey VK.Right oldDirection (1,0)
let checkLeftKey oldDirection = checkKey VK.Left oldDirection (-1, 0)
let checkUpKey oldDirection = checkKey VK.Up oldDirection (0, -1)
let checkDownKey oldDirection = checkKey VK.Down oldDirection (0,1)
let checkEscKey oldDirection : Direction = 
    if isKeyPressed VK.Esc then
        drawPauseScreen()
        oldDirection
    else
        oldDirection
        
let getMovementUpdate (gs:GameState) : GameState =
    let getUserInput (oldDirection:Direction) = 
            oldDirection
            |> checkRightKey
            |> checkLeftKey
            |> checkUpKey
            |> checkDownKey
            |> checkEscKey
    
    System.Threading.Thread.Sleep(1000/Fps);
    {
        Snake = gs.Snake;
        Cookies = gs.Cookies;
        Direction = getUserInput gs.Direction;
        Points = gs.Points;
        Frames = gs.Frames
    }

[<EntryPoint>]
let main args =
    let rec playGame (gs:GameState) : GameState =
        let newGs = 
            gs
            |> getMovementUpdate
            |> gameUpdate
            |> gameDraw
        
        if gs.Points < PointsToWin then playGame newGs else newGs

    let gs = playGame DefaultGameState
    printf "Congratulations! You won in %d frames!" gs.Frames
    0