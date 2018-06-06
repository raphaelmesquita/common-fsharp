module Result 

/// Executa uma função em um resultado
let iter action result = 
    match result with
    | Error _ -> ()
    | Ok x -> action x 

/// Aplicação sequencial
let apply fRes xRes =
    match fRes, xRes with
    | Ok f, Ok x -> Ok (f x)
    | Error e, _ | _, Error e -> Error e

let (<*>) = apply
let (<!>) = Result.map

/// Eleva para Result uma função de dois argumentos
let lift2 f x y =
    f <!> x <*> y

/// Eleva para Result uma função de três argumentos
let lift3 f x y z =
    f <!> x <*> y <*> z

/// Eleva para Result uma função de quatro argumentos
let lift4 f x y z w =
    f <!> x <*> y <*> z <*> w

/// Transforma resultado de resultados em um resultado planificado
let flatten res = Result.bind id res 

/// Transforma uma função "switch" ('a -> Result<'b, 'c>) em uma função 
/// "switch" que funciona em um lista ('a list -> Result<'b list, 'c>)
/// por aplicação sequencial revertida (descartando erros subsequentes)
let traverse f list =    
    let (<*>) = apply
    let retn = Ok
    let cons head tail = head :: tail
    let initState = retn []
    let folder head tail = 
        retn cons <*> (f head) <*> tail
    List.foldBack folder list initState 

/// Transforma uma função "switch" ('a -> Result<'b, 'c>) em uma função 
/// "switch" que funciona em um lista ('a list -> Result<'b list, 'c>)
/// por aplicação sequencial (descartando erros subsequentes)
let traverseForward f list = List.rev list |> traverse f 

/// Encapsula a mensagem de qualquer exceção 
/// lançada pela função f em um Result    
let protect f x =
    try Ok <| f x
    with e -> Error e

/// Encapsula uma função de modo que caso ocorra 
/// uma exceção, um erro Error(msg) será retornado
let protectWithMsg msg f x =
    try Ok <| f x
    with _ -> Error msg

type ResultBuilder() =

    member this.Return(x) = Ok x

    member this.ReturnFrom(m: Result<'T,'U>) = m

    member this.Bind(m, f) = Result.bind f m

    member this.Zero() = Ok ()

    member this.Delay(f: unit -> _) = f

    member this.Run(f) = f()

    member this.Combine(m, f) = this.Bind(m, f)

    member this.While(guard, f) =
        if guard() then this.Bind(f(), fun () -> this.While(guard, f))
        else this.Zero()

    member this.TryWith(f, h) =
        try this.ReturnFrom(f())
        with e -> h e

    member this.TryFinally(f, compensation) =
        try this.ReturnFrom(f())
        finally compensation()

    member this.Using(disposable:#System.IDisposable, f) =
        let f' = fun () -> f disposable
        this.TryFinally (f', fun () ->
            match disposable with
            | null -> ()
            | disp -> disp.Dispose())

    member this.For(sequence:seq<_>, body) =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext,
                this.Delay(fun () -> body enum.Current)))

module Operators =
    
    /// Result computation expression builder
    let result = ResultBuilder()

    /// Wrapper
    let inline retn x = Ok x

    /// Infix map
    let inline (<!>) f x = Result.map f x

    /// Aplicação sequencial
    let inline (<*>) f x = apply f x

    /// Bind
    let inline (=<<) f m = Result.bind f m

    /// Bind invertido
    let inline (>>=) m f = Result.bind f m

    /// Composição Kleisli da esquerda para a direita
    let inline (>=>) f g = fun x -> f x >>= g

    /// Composição Kleisli da direita para a esquerda
    let inline (<=<) f g = g >=> f

    /// Combina os dois resultados mantendo apenas o primeiro
    let inline ( <* ) x y = lift2 (fun left right -> left) x y

    /// Combina os dois resultados mantendo apenas o segundo
    let inline ( *> ) x y = lift2 (fun left right -> right) x y