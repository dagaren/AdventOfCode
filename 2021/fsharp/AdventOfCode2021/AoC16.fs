module AoC16

open System
open Utils

type PacketBounds = {
    startIndex: int
    length: int
}

type PacketInfo = {
    version: int
    typeId: int
} 

type LiteralPacket = {
    info: PacketInfo
    bounds: PacketBounds
    value: int64
}

type Packet = 
| LiteralPacket of LiteralPacket
| OperatorPacket of OperatorPacket
and OperatorPacket = {
    info: PacketInfo
    bounds: PacketBounds
    subpackets: Packet list
}

type OperatorLength = 
| TotalBits of int
| NumberSubpackets of int
    

let aoc16 () = 
    printBanner 16

    let input = readInputFile "AoC16.txt" |> Array.head

    let hexadecimalToBinary hexString = 
        Convert.ToString(Convert.ToInt64(hexString, 16), 2).PadLeft(4, '0');

    let intFromBinarySlice data = 
        let dataStr = data |> Array.fold (+) "" 
        Convert.ToInt32(dataStr, 2)

    let int64FromBinarySlice data = 
           let dataStr = data |> Array.fold (+) "" 
           Convert.ToInt64(dataStr, 2)

    let parsePacketInfo (data:string[]) = 
        {
            version = intFromBinarySlice data[0..2]
            typeId = intFromBinarySlice data[3..5]
        }

    let parseLiteralPacket startIndex info (data:string[]) =
        let chunks = 
            data[(startIndex + 6)..]
            |> Seq.ofArray
            |> Seq.chunkBySize 5
        
        let firsts = chunks |> Seq.takeWhile (fun x -> x[0] = "1")
        let lasts = chunks |> Seq.skipWhile (fun x -> x[0] = "1") |> Seq.take 1

        let valueChunks = Seq.append firsts lasts

        let value = 
            valueChunks 
            |> Seq.map (fun x -> x[1..])
            |> Seq.collect id
            |> Seq.toArray
            |> int64FromBinarySlice

        let numChunks = valueChunks |> Seq.length
        let totalLength = 6 + numChunks * 5

        {
            info = info
            value = value
            bounds = {
                startIndex = startIndex
                length = totalLength
            }
        }

    let getPacketLength packet =
        match packet with
        | OperatorPacket p -> p.bounds.length
        | LiteralPacket p -> p.bounds.length

    let parseOperatorLength (data:string[]) =
        let first = data[0]

        match first with
        | "0" -> (TotalBits (intFromBinarySlice data[1..15]), 16)
        | "1" -> (NumberSubpackets (intFromBinarySlice data[1..11]), 12)
        | _ -> failwith "Invalid bit value"

    let rec parsePacketsSequence startIndex data currentPackets packetsLength = 
        let endReached = 
            match packetsLength with
            | TotalBits numBits ->
                let currentPackagesLength = currentPackets |> List.map getPacketLength |> List.sum
                if currentPackagesLength = numBits then true else false
            | NumberSubpackets numSubpackets ->
                if currentPackets.Length = numSubpackets then true else false
        
        if endReached = true then
            currentPackets
        else
            let nextPackage = parsePacket startIndex data

            let nextPackages = currentPackets @ [ nextPackage ]

            let nextIndex = startIndex + (getPacketLength nextPackage)

            parsePacketsSequence nextIndex data nextPackages packetsLength
    and parseOperatorPacket startIndex info (data:string[]) = 
        let lengthType, lengthLength = parseOperatorLength data[(startIndex+6)..]

        let subPackets = parsePacketsSequence (startIndex + 6 + lengthLength) data [] lengthType

        let subPacketsLength = subPackets |> List.map getPacketLength |> List.sum

        {
            info = info
            subpackets = subPackets
            bounds = {
                startIndex = startIndex
                length = (6 + lengthLength + subPacketsLength)
            }
        }
    and parsePacket startIndex (data:string[]) : Packet =
        let packetInfo = parsePacketInfo data[startIndex..]

        if packetInfo.typeId = 4 then
            LiteralPacket (parseLiteralPacket startIndex packetInfo data)
        else
            OperatorPacket (parseOperatorPacket startIndex packetInfo data)


    let rec sumVersionNumbers packet = 
        match packet with
        | OperatorPacket p -> 
            p.info.version + (p.subpackets |> List.map sumVersionNumbers |> List.sum)
        | LiteralPacket p ->
            p.info.version 

    let data = 
        input 
        |> Seq.map string 
        |> Seq.map hexadecimalToBinary 
        |> Seq.collect (fun str -> str |> Seq.map string)
        |> Seq.toArray

    let packet = parsePacket 0 data

    let sumVersions = sumVersionNumbers packet

    let result1 = sprintf "Sum of all versions of parsed package: %d" sumVersions
    printFirstStarResult result1

    let rec calculateValue packet = 
        match packet with
        | OperatorPacket p -> 
            let subpacketValues = p.subpackets |> List.map calculateValue

            match p.info.typeId with
            | 0 -> subpacketValues |> List.sum
            | 1 -> subpacketValues |> List.fold (*) 1L
            | 2 -> subpacketValues |> List.min
            | 3 -> subpacketValues |> List.max
            | 5 -> if subpacketValues[0] > subpacketValues[1] then 1L else 0L
            | 6 ->  if subpacketValues[0] < subpacketValues[1] then 1L else 0L
            | 7 -> if subpacketValues[0] = subpacketValues[1] then 1L else 0L
            | _ -> failwith "Invalid type for operator packets"
        | LiteralPacket p ->
            p.value 

    let resultValue = calculateValue packet

    let result2 = sprintf "Evaluation result of packet: %d" resultValue
    printSecondStarResult result2