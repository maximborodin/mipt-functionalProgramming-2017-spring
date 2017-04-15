module lab1 =

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "borodin.ms@phystech.edu"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть
// вариант 18
let fTailor x : float = (1.0 + x * x) / 2.0 * Math.Atan(x) - x / 2.0
                                  // функция, которую раскладываем
let n, a, b = 20., 0.1, 0.6 // интервал

let tailor x : Result = 
  let rec stupidRecTailor (x : float) (n : int) (acc : float) =
    let tailorElement x n = //степень 2n + 1
      Math.Pow(-1.0, (float)n + 1.0) * Math.Pow(x, 2.0 *(float) n + 1.0) / (4.0 * Math.Pow((float)n, 2.0) - 1.0)
    let curTailorElement = tailorElement x n
    if (Math.Abs(curTailorElement) < delta) 
      then (acc, n)
    else
      stupidRecTailor x (n + 1) (acc + (float)curTailorElement)
  stupidRecTailor x 1 0.0

let tailorA x : Result = 
  let rec cleverRecTailor (x : float) (n: int) (acc : float) (prevElement : float) =
    let curTailorElement = prevElement * Math.Pow(x, 2.0) * (-1.0) *
      (2.0 * (float)n - 3.0) / (2.0 * (float)n + 1.0)
    if (Math.Abs(curTailorElement) < delta)
      then (acc, n)
    else
      cleverRecTailor x (n + 1) (acc + curTailorElement) curTailorElement
  cleverRecTailor x 1 0.0 x


let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )


//printTailor()
// *** Вторая часть


// функции, решение которых ищем
let fSolve1, a1, b1 = (fun x -> x + Math.Pow(x, 0.5) + Math.Pow(x, 1.0/3.0) - 2.5), 0.4, 1.0
let fSolve2, a2, b2 = (fun x -> x - 1.0/(3.0 + Math.Sin(3.6 * x))), 0.0, 0.85
let fSolve3, a3, b3 = (fun x -> 0.1 * x * x - x * Math.Log(x)), 1.0, 2.0

let maxIterationCount = 10000
// производная

let derivative f x = (f(x + delta) - f(x)) / delta

let iter (f : float -> float) (a : float) (b : float) : Result =
  let middle = (a + b) / 2.0
  let g = fun x -> (x - (f x) / (derivative f middle))
  let rec iterA (n : int) (point : float) =
    let newPoint = g point
    if (Math.Abs(point - newPoint) > (b - a) || n = maxIterationCount) then (nan, n)
    elif (Math.Abs (point - newPoint) < delta) then (point, n)
    else iterA (n + 1) newPoint
  iterA 0 middle

let newton (f : float -> float) (a : float) (b : float) : Result = 
  let middle = (a + b) / 2.0
  let rec newtonA (n : int) (point : float) =
    let newPoint = point - (f point) / (derivative f point)
    if (Math.Abs(point - newPoint) > (b - a) || n = maxIterationCount) then (nan, n)
    elif (Math.Abs(point - newPoint) < delta) then (point, n)
    else newtonA (n + 1) newPoint
  newtonA 0 middle


let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result =
      let c = (a + b) / 2.0
      if (Math.Abs(b - a) < delta) then (c, i)
      elif ((f a) * (f c) <= 0.) then dichotomyA (i + 1) f a c
      elif ((f c) * (f b) <= 0.) then dichotomyA (i + 1) f c b
      else (nan, i)
    dichotomyA 0 // чтобы воспользоваться каррированием

let printSolve fSolve (a : float) (b : float) =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve a b) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

let printSolves() =
  printSolve fSolve1 a1 b1
  printSolve fSolve2 a2 b2
  printSolve fSolve3 a3 b3

//printSolves()

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)
  printf "%A\n" responseString

main ()