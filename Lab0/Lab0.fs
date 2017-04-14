module lab0

open System
open System.Net
open System.Collections.Specialized

let (email, name) = ("borodin.ms@phystech.edu", "Borodin M.S.") // адрес почты и фамилия с инициалами

let rec pascal c r =  // а тут решение
  if (c = 0) || (c = r) then 1
  else pascal (c - 1) (r - 1) + pascal c (r - 1)

let printIt n = 
  "[" +
  ([for x in 0..n do for y in 0..x do yield pascal y x] 
    |> List.map (fun x -> x.ToString())
    |> List.reduce (fun x y -> x + "," + y) )
  + "]"


let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("name", name)
  values.Add("content", printIt 20)

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab0"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()


//let res = pascal 3 5
//printIt 5