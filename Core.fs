[<AutoOpen>]
module Core
open System
open System.Text
open System.Linq

/// Usado para plugar uma função ao pipeline ignorando o resultado
let tee fn x = 
    fn x |> ignore
    x

/// Operador para trabalhar com tee
let inline (|>!) x fn = tee fn x

/// Executa uma função e e faz retry a primeira vez que ela lançar exceção
let execWithRetry f = 
    try f()
    with _ -> f()
    
/// Loga uma mensagem
let logMessage (sink:string->unit) messageType message = 
    let date = DateTime.Now.ToString()
    sink <| sprintf "[%s][%s] %s" date messageType message

let private handleMatch (m: RegularExpressions.Match) =
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

/// ParseRegex parses a regular expression and returns a list of the strings that match each group in
/// the regular expression.
/// List.tail is called to eliminate the first element in the list, which is the full matched expression,
/// since only the matches for each group are wanted.
/// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
let (|Regex|_|) regex str =
    RegularExpressions.Regex(regex).Match(str)
    |> handleMatch

let (|RegexGlobal|_|) regex str =
    let folder m acc = 
        match acc with
        | None -> handleMatch m |> Option.map (fun xs -> [xs])
        | Some xss -> handleMatch m |> Option.map (fun xs -> xs::xss)
    (RegularExpressions.Regex(regex).Matches(str).Cast<RegularExpressions.Match>(), None)
    ||> Seq.foldBack folder 

/// https://stackoverflow.com/questions/2225343/case-insensitive-pattern-matching-over-string-lists
let (|InvariantEqual|_|) (str:string) arg = 
  if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

/// Transforma objetos que podem ser nulos em opções
let (|ToOption|) x =
    let is y = obj.ReferenceEquals(x, y)
    match (is null || is DBNull.Value) with
    | true -> None
    | false -> Some x

/// Transforma objetos que podem ser nulos em opções
let toOption (ToOption opt) = opt