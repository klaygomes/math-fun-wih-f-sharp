open System

let cofactor (i:float) subM =
  (-1. ** (i+2.)) * laplace subM

let rec laplace (matrix:float[,]) =
  //how can I avoid this mutable?
  let mutable acumulator = 0.
  let determine (m:float[,]) =
    m.[0,0] * m.[1,1] - m.[0,1] * m.[1,0]
  if Array2D.length1 matrix<=2 then
    determine matrix
  else
    let length = (Array2D.length1 matrix)-1
    for i = 0 to length do
      let jump item =
        if item >= i then item+1 else item
      let subM = Array2D.init<float> length length (fun row col -> matrix.[jump row, col+1])
      acumulator <- acumulator + (matrix.[i,0]) * (cofactor (float i) subM)
    acumulator


let teste = array2D [
    [ 2.; 3.; 5.; 6. ]
    [ 4.; 2.; 1.; 1. ]
    [ 5.; 1.; 2.; 3. ]
    [ 6.; 1.; 3.; 2. ]
]

//let teste = array2D [
//    [ 3.;5.;6.]
//    [ 2.;1.;1.]
//    [ 1.;2.;3. ]
//]
laplace teste //should output 84
