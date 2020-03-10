module Synthesis

let abelar n =
     n > 12 && n < 3097 && n % 12 = 0
    

let area b h =
   match b < 0.0 || h < 0.0 with 
   | true -> failwith "Negative number has been supplied"
   | _  -> 0.5 * b * h 
     

let zollo a =
    match a<0 with 
    | true -> -a
    | _ -> a*2
   

let min a b =
   match a<b with 
   | true -> a 
   | _ -> b

let max a b =
     match a > b with
     | true -> a 
     | _ -> b
    

let ofTime h m s =
           h * 3600 + m * 60 + s
let toTime s =
     match s >= 0 with
     | true -> let h = s/3600 
               let m = (s - h*3600) / 60
               let sec = s - h*3600 - m*60 in 
               (h,m,sec)
     | _ -> (0,0,0)

     
       

let digits Suppliednumber =
     
     let rec CountDigits Expression countNtimes =
          match Expression = 0 && countNtimes <> 0 with
          | false  -> CountDigits (Expression/10) (countNtimes+1)
          | true -> countNtimes
     CountDigits Suppliednumber 0
    

let minmax ( a, b, c, d ) =
     let smallV = min a b |> min c |> min d
     let LargeV = max a b |> max c |> max d
     (smallV,LargeV)

let isLeap year =
          match year < 1582 with
          | true ->  failwith "Year is less than 1582"
          | _ ->  match (year % 4 = 0 ) with
                    | false -> false
                    | true -> match year % 100 <> 0 with
                              | true -> true 
                              | false -> year % 400 = 0
   
let month day =
             match  day>= 1 && day <= 12 with
             | true -> match day with
                      | 1 -> "January" ,31
                      | 2  -> "February", 28
                      | 3 ->"March", 31
                      | 4 -> "April", 30
                      | 5 -> "May", 31
                      |6 -> "June", 30
                      |7 -> "July", 31
                      | 8 -> "August", 31
                      |9  -> "September", 30
                      |10 -> "October", 31
                      |11 -> "November", 30
                      |12 -> "December", 31

             |false -> failwith "your nmonth number is not valid"


let toBinary interger =
           
            let rec getBinary n mfwethu =
               match n=0 && mfwethu <> "" with
               | true -> mfwethu
               | false -> match n % 2 with
                          | 0 ->  getBinary (n/2) ("0" + mfwethu)
                          | _ -> getBinary (n/2) ( "1" + mfwethu)
            match (interger>=0) with 
            | true -> getBinary interger ""
            | false -> failwith "negative number is supplied"


let bizFuzz number =
       match number > 0 with 
       | true -> let divisibleBythree = (number/3)
                 let DivisibleByfive = number/5
                 let rec DivisibleByBoth n byBoth =
                    match n <= number with
                    | true  -> match (n % 3 = 0) && ( n % 5 = 0 )with
                                |true -> DivisibleByBoth (n+1) (byBoth+1)
                                | false -> DivisibleByBoth (n+1) byBoth  
                    | false -> (divisibleBythree,DivisibleByfive,byBoth)
                 DivisibleByBoth 1 0
       | false -> (0,0,0)
let monthDay d y =
            let LeapYear = 
            match day>=1 && day <= 365
             



let coord _ =
    failwith "Not implemented"