module AoC20

open Utils

type image = {
    pixels: Map<(int*int),int>
    width: int
    height: int
    emptyPoints: int
}

let aoc20 () =
    printBanner 20

    let input = readInputFile "AoC20.txt" |> List.ofArray

    let inputIEA = input |> List.head
    let inputImage = input |> List.skip 2

    let getImageEnhancementAlgorithm (str:string) : Map<int,int> = 
        str 
        |> Seq.mapi (fun index bitChar -> (index, if bitChar = '#' then 1 else 0)) 
        |> Map.ofSeq

    let getImage (strs:string list) = 
        let height = strs |> List.length
        let width = strs |> List.head |> Seq.length

        let map = strs 
                    |> List.mapi (fun row str -> (row, str))
                    |> List.collect (fun (row, str) -> str |> List.ofSeq |> List.mapi (fun column char -> ((column, row), if char = '#' then 1 else 0) ))
                    |> Map.ofList

        {
            pixels = map
            height = height
            width = width
            emptyPoints = 0
        }

    let getRoundingPoints (x, y) = [
        (x - 1, y - 1);
        (x, y - 1);
        (x + 1, y - 1);
        (x - 1, y);
        (x, y);
        (x + 1, y);
        (x - 1, y + 1);
        (x, y + 1);
        (x + 1, y + 1);
    ]

    let isValidPoint image (x, y) = x < image.width && x >= 0 && y >= 0 && y < image.height

    let getPixelIndex image point = 
        let binaryStr = 
            point 
              |> getRoundingPoints 
              |> List.map (fun p -> if (isValidPoint image p) then image.pixels[p] else image.emptyPoints)
              |> List.map (fun bit -> string(bit))
              |> List.fold (+) ""

        System.Convert.ToInt32(binaryStr, 2)

    let generateRowPixels row numElements value = [ for i in 0..numElements-1 -> ((row, i), value)]

    let generateColumnPixels column numElements value = [ for i in 0..numElements-1 -> ((i, column), value) ]

    let incrementImage image = 
        let newWidth = image.width + 2
        let newHeight = image.height + 2

        let newPixels = 
            image.pixels 
              |> Map.toList 
              |> List.map (fun ((x, y),value) -> ((x + 1, y + 1), value))
              |> List.append (generateRowPixels 0 newWidth image.emptyPoints)
              |> List.append (generateRowPixels (newHeight - 1) newWidth image.emptyPoints)
              |> List.append (generateColumnPixels 0 newHeight image.emptyPoints)
              |> List.append (generateColumnPixels (newWidth - 1) newHeight image.emptyPoints)
              |> Map.ofList

        {
            width = newWidth
            height = newHeight
            pixels = newPixels
            emptyPoints = image.emptyPoints
        }

    let processImage (algorithm:Map<int,int>) image = 
        let tImage = image |> incrementImage
       

        let newPixels = 
            tImage.pixels 
                |> Map.toList 
                |> List.map (fun (point, pixel) -> (point, algorithm[(getPixelIndex tImage point)]))
                |> Map.ofList

        let nextEmptyPoints = 
            if image.emptyPoints = 1 then
                algorithm[511]
            else
                algorithm[0]

        { 
            tImage with pixels = newPixels
                        emptyPoints = nextEmptyPoints }

    let iea = getImageEnhancementAlgorithm inputIEA
    let image = getImage inputImage

    let getLitPixels (image:image) = image.pixels |> Map.toList |> List.where (fun (point, pixel) -> pixel = 1) |> List.length
    
    let finalImage = 
        image 
            |> processImage iea 
            |> processImage iea
    
    let numPixelsLit = getLitPixels finalImage

    let result1 = sprintf "Number of pixels lit after two iterations: %d" numPixelsLit
    printFirstStarResult result1

    // Part 2

    let image50 = [for i in 1..50 -> i]|> List.fold (fun currentImage index -> processImage iea currentImage ) image
    let numPixelsLit50 = getLitPixels image50

    let result2 = sprintf "Number of pixels lit after 50 iterations: %d" numPixelsLit50
    printSecondStarResult result2

        