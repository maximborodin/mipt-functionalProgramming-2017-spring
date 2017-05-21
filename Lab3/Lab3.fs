// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "borodin.ms@phystech.edu"
let site = "https://github.com/orgs/Microsoft/people?page="

let rec getUsersFromPage (url : string) = 
  try
    let html = HtmlDocument.Load(url)
    html.Descendants["a"]
      |> Seq.filter (fun x -> x.HasClass("css-truncate-target f4"))
      |> Seq.map(fun x -> x.AttributeValue("href"))
      |> Seq.toList
  with
    | _ -> getUsersFromPage(url)

let getAllUsers() =
  [1..20] 
    |> List.map (fun x -> site + x.ToString()) // ссылки на страницы 
    |> List.collect getUsersFromPage // список списков

let rec check href = 
  try
    let html = HtmlDocument.Load("https://github.com" + href)
    html.Descendants["svg"]
      |> Seq.filter (fun x -> x.HasClass("octicon octicon-link"))
      |> Seq.length
      |> ((<) 0)
  with
    | _ -> check href

let lab3 () =
  getAllUsers()
    |> List.filter check
      
lab3()

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main()