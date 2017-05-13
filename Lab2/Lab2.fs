open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "borodin.ms@phystech.edu"


type Lexeme =
  | OpenBracket 
  | CloseBracket
  | OpenBrace 
  | CloseBrace
  | Colon 
  | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let lexeming source =
  let rec parseString acc = function
    | '\\' :: '"' :: l -> parseString (acc + "\"") l
    | '\\' :: 'n' :: l -> parseString (acc + "\n") l
    | '"' :: l -> acc, l  
    | c :: l -> parseString (acc + c.ToString()) l
    | _ -> failwith "Malformed string."
 
  let rec lexeme acc = function
    | (x :: _) as l when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, l
    | w :: l when Char.IsWhiteSpace(w) -> acc, l
    | [] -> acc, [] // end of list
    | c :: l -> lexeme (acc + (c.ToString())) l

  let rec lexeming' acc = function
    | w :: l when Char.IsWhiteSpace(w) -> lexeming' acc l
    | '{' :: l -> lexeming' (OpenBrace :: acc) l
    | '}' :: l -> lexeming' (CloseBrace :: acc) l
    | '[' :: l -> lexeming' (OpenBracket :: acc) l
    | ']' :: l -> lexeming' (CloseBracket :: acc) l
    | ':' :: l -> lexeming' (Colon :: acc) l
    | ',' :: l -> lexeming' (Comma :: acc) l
    | '"' :: l -> // empty string, nothing
      let s, l' = parseString "" l
      lexeming' (String s :: acc) l'
    | 't' :: 'r' :: 'u' :: 'e' :: l -> lexeming' (Boolean true :: acc) l
    | 'n' :: 'u' :: 'l' :: 'l' :: l -> lexeming' (Null :: acc) l
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: l -> lexeming' (Boolean false :: acc) l
    | d :: l -> // parse numbers
      let n, l' = lexeme (d.ToString()) l
      lexeming' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) l'
    | [] -> List.rev acc
    | _ -> failwith "Tokenization error"
  lexeming' [] source

type JSON = 
  | Object of (string * JSON) list
  | Array of JSON list
  | Boolean of bool
  | Number of int
  | String of string
  | Null

let rec parseLexemes obj =
  let rec parseLexemes' obj =
    let rec parseObject list = function
      | CloseBrace :: l -> (Object (List.rev list)), l
      | Comma :: l -> parseObject list l
      | Lexeme.String s :: Colon :: l ->
        let a, l = parseLexemes' l
        parseObject ((s, a) :: list) l
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: l -> (Array (List.rev list)), l
      | Comma :: l -> parseArray list l
      | ob -> 
        let a, l = parseLexemes' ob
        parseArray (a :: list) l  
    match obj with
      | OpenBrace :: l -> parseObject [] l
      | OpenBracket :: l -> parseArray [] l
      | Lexeme.Null :: l -> JSON.Null, l
      | Lexeme.String s :: l -> JSON.String s, l
      | Lexeme.Number s :: l -> JSON.Number s, l
      | Lexeme.Boolean s :: l -> JSON.Boolean s, l
      | _ -> failwith "Incorrect identification"
  match parseLexemes' obj with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"

    
let split (str : string) =
  [for ch in str -> ch]

let parse = //if str = "{}" then Object [] else failwith "Wrong JSON structure!"
  split >> lexeming >> parseLexemes

let lab3 obj =
  let rec lab3' depth = function
      | Number x -> Number(x + depth)
      | Object(list) -> 
         Object(List.rev(List.fold (fun acc x -> (fst(x), lab3' (depth + 1) (snd(x))) :: acc) [] list))
      | Array(arr) ->
          Array(List.rev (List.fold (fun acc x -> (lab3' depth x) :: acc) [] arr))
      | l -> l
  lab3' 0 obj

let json_test = """
{"widget": {
    "debug": "on",
    "window": {
        "title": "Sample Konfabulator Widget",
        "name": "main_window",
        "width": 500,
        "height": 500
    },
    "image": { 
        "src": "Images/Sun.png",
        "name": "sun1",
        "hOffset": 250,
        "vOffset": 250,
        "alignment": "center"
    },
    "text": {
        "data": "Click Here",
        "size": 36,
        "style": "bold",
        "name": "text1",
        "hOffset": 250,
        "vOffset": 100,
        "alignment": "center",
        "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
    }
}}  
"""
parse json_test
lab3 <| parse json_test // simple test

let rec objToStr = function
  | Object(lst) ->
        "{" + (List.map (fun pair -> "\"" + fst pair + "\" : " + objToStr (snd pair)) lst |> String.concat ", ") + "}"
  | Array(arr) ->
        "[" + ((List.map objToStr arr) |> String.concat ", ") + "]"
  | Number(a) -> string a
  | String(a) -> a
  | Boolean(true) -> "true"
  | Boolean(false) -> "false"
  | Null -> "null"

let rec generate() = 
  let rnd = new Random()
  let rec generator() = 
    match rnd.Next(6) with
      | 0 -> Null
      | 1 -> Number(rnd.Next())
      | 2 -> String(rnd.Next().ToString())
      | 3 -> Boolean(if (rnd.Next(2) = 0) then true else false)
      | 4 -> 
        let n = rnd.Next(5)
        let rec generate' acc = function
          | 0 -> acc
          | n -> generate' (generator()::acc) (n - 1)
        Array(generate' [] n)
      | _ ->
        let n = rnd.Next(5)
        let rec generate' acc = function
          | 0 -> acc
          | n -> generate' ((rnd.Next().ToString(), generator())::acc) (n - 1)
        Object(generate' [] n)
  generator()

let testing () = 
  let randomTestJsonObj = generate()
  printfn "random test Json Object %A" randomTestJsonObj
  let randomTestStr = objToStr randomTestJsonObj
  printfn "random test string %s" randomTestStr
  let randomTestAns = lab3 randomTestJsonObj
  printfn "random test answer %A" randomTestAns

testing()

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString


main()