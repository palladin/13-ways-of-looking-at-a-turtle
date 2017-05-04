(* ======================================
13-Interpreter-v2.fsx

Part of "Thirteen ways of looking at a turtle"
Related blog post: http://fsharpforfunandprofit.com/posts/13-ways-of-looking-at-a-turtle/
======================================

Way #13: The interpreter pattern

In this design, the client builds a data structure (`TurtleProgram`) that represents the instructions.

This Turtle Program can then interpreted later in various ways

====================================== *)


open System

#load "Common.fsx"
#load "FPTurtleLib2.fsx"

open System
open Common
open FPTurtleLib2


// ============================================================================
// Turtle Program V3 
//  * Add Trampoline
// ============================================================================

module TurtleProgram_v3 = 
    open Turtle

    /// Create a type to represent each instruction
    type TurtleInstruction<'next> = 
        | Move     of Distance * (MoveResponse -> 'next)
        | Turn     of Angle    * 'next
        | PenUp    of            'next
        | PenDown  of            'next
        | SetColor of PenColor * (SetColorResponse -> 'next)

    /// Create a type to represent the Turtle Program
    type TurtleProgram<'a> = 
        | Stop of 'a
        | KeepGoing of (unit -> TurtleInstruction<TurtleProgram<'a>>)


    /// map the instructions
    let mapInstr f inst  = 
        match inst with
        | Move(dist,next) -> 
            Move(dist,next >> f) 
        | Turn(angle,next) -> 
            Turn(angle,f next)  
        | PenUp(next) -> 
            PenUp(f next)
        | PenDown(next) -> 
            PenDown(f next)
        | SetColor(color,next) -> 
            SetColor(color,next >> f)

    let returnT x = 
        Stop x 

    let rec bindT f program = 
        match program with
        | KeepGoing instruction -> 
            KeepGoing (fun () -> (mapInstr (bindT f) (instruction ())))
        | Stop x -> 
            f x

    // define a computation expression builder
    type TurtleProgramBuilder() =
        member this.Return(x) = returnT x
        member this.Bind(x,f) = bindT f x
        member this.Zero(x) = returnT ()

    // create an instance of the computation expression builder
    let turtleProgram = TurtleProgramBuilder()

// ------------------------
// Example of Turtle Program V3 object
// ------------------------

module TurtleProgram_v3_Example = 
    open TurtleProgram_v3

    // example
    let drawTriangle = 
        KeepGoing (fun () -> Move (100.0, fun response -> 
         KeepGoing (fun () ->
          Turn (120.0<Degrees>,
           KeepGoing (fun () -> Move (100.0, fun response  -> 
            KeepGoing (fun () ->
             Turn (120.0<Degrees>, KeepGoing (fun () -> Move (100.0, fun response  -> 
              KeepGoing (fun () ->
               Turn (
                120.0<Degrees>, Stop () ))))))))))))
    // val drawTriangle : TurtleProgram<unit>  

    // helper functions
    let stop = Stop()
    let move dist  = KeepGoing (fun () -> Move (dist, Stop))    // "Stop" is a function
    let turn angle  = KeepGoing (fun () -> Turn (angle, stop))  // "stop" is a value
    let penUp  = KeepGoing (fun () -> PenUp stop)
    let penDown  = KeepGoing (fun () -> PenDown stop)
    let setColor color = KeepGoing (fun () -> SetColor (color,Stop))

    let handleMoveResponse log moveResponse = turtleProgram {
        match moveResponse with
        | Turtle.MoveOk -> 
            ()
        | Turtle.HitABarrier ->
            // turn 90 before trying again
            log "Oops -- hit a barrier -- turning"
            let! x = turn 90.0<Degrees>
            ()
        }

    // example
    let drawTwoLines log = turtleProgram {
        let! response = move 60.0
        do! handleMoveResponse log response 
        let! response = move 60.0
        do! handleMoveResponse log response 
        }
    // val drawTwoLines: TurtleProgram<unit>


// ------------------------
// Interpreters for Turtle Program v3
// ------------------------

module TurtleProgram_v3_Interpreter = 
    open TurtleProgram_v3

    /// Interpret as a turtle
    let interpretAsTurtle log state program =
        let rec runPrg state program = 
            match program with
            | Stop a -> 
                state 
            | KeepGoing f -> runInstr state (f ())
        and runInstr state instr = 
            match instr with
            | Move (dist,next) ->
                let result,newState = Turtle.move log dist state 
                let nextProgram = next result // compute next program
                runPrg newState nextProgram 
            | Turn (angle,next) ->
                let newState = Turtle.turn log angle state 
                let nextProgram = next        // use next program directly
                runPrg newState nextProgram 
            | PenUp next ->
                let newState = Turtle.penUp log state 
                runPrg newState next
            | PenDown next -> 
                let newState = Turtle.penDown log state 
                runPrg newState next
            | SetColor (color,next) ->
                let result,newState = Turtle.setColor log color state 
                let nextProgram = next result
                runPrg newState nextProgram 
        runPrg state program

    /// Interpret as a distance
    let interpretAsDistance distanceSoFar program =
        let log = printfn "%s"
        let rec runPrg state program = 
            match program with
            | Stop a -> 
                state 
            | KeepGoing f -> runInstr state (f ())
        and runInstr state instr = 
            match instr with
            | Move (dist,next) ->
                let newDistanceSoFar = distanceSoFar + dist
                let result = Turtle.MoveOk
                let nextProgram = next result 
                runPrg newDistanceSoFar nextProgram 
            | Turn (angle,next) ->
                // no change in distanceSoFar
                runPrg state next
            | PenUp next ->
                // no change in distanceSoFar
                runPrg state next
            | PenDown next -> 
                // no change in distanceSoFar
                runPrg state next
            | SetColor (color,next) ->
                // no change in distanceSoFar
                let result = Turtle.ColorOk
                let nextProgram = next result
                runPrg state nextProgram 
        runPrg distanceSoFar program

// ------------------------
// TurtleProgram_v3_Interpreter Tests
// ------------------------

// Interpret `drawTriangle` as turtle
do 
    let log = printfn "%s"
    let interpret = TurtleProgram_v3_Interpreter.interpretAsTurtle 
    let program = TurtleProgram_v3_Example.drawTriangle
    let initialState = Turtle.initialTurtleState
    interpret log initialState program |> ignore

// Interpret `drawTriangle` as distance
do 
    let interpret = TurtleProgram_v3_Interpreter.interpretAsDistance
    let program = TurtleProgram_v3_Example.drawTriangle
    let initialState = 0.0
    interpret initialState program |> printfn "Total distance moved is %0.1f"

  
// Interpret `drawTwoLines` as turtle
do 
    let log = printfn "%s"
    let interpret = TurtleProgram_v3_Interpreter.interpretAsTurtle 
    let program = TurtleProgram_v3_Example.drawTwoLines log 
    let initialState = Turtle.initialTurtleState
    interpret log initialState program |> ignore

// Interpret `drawTwoLines` as distance
do 
    let log = printfn "%s"
    let interpret = TurtleProgram_v3_Interpreter.interpretAsDistance 
    let program = TurtleProgram_v3_Example.drawTwoLines log 
    let initialState = 0.0
    interpret initialState program |> printfn "Total distance moved is %0.1f"
