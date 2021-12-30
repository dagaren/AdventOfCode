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

    let input = "E054831006016008CF01CED7CDB2D495A473336CF7B8C8318021C00FACFD3125B9FA624BD3DBB7968C0179DFDBD196FAE5400974A974B55C24DC580085925D5007E2D49C6579E49252E28600B580272379054AF57A54D65E1586A951D860400434E36080410926624D25458890A006CA251006573D2DFCBF4016919CC0A467302100565CF24B7A9C36B0402840002150CA3E46000042621C108F0200CC5C8551EA47F79FC28401C20042E0EC288D4600F42585F1F88010C8C709235180272B3DCAD95DC005F6671379988A1380372D8FF1127BDC0D834600BC9334EA5880333E7F3C6B2FBE1B98025600A8803F04E2E45700043E34C5F8A72DDC6B7E8E400C01797D02D002052637263CE016CE5E5C8CC9E4B369E7051304F3509627A907C97BCF66008500521395A62553A9CAD312A9CCCEAF63A500A2631CCD8065681D2479371E4A90E024AD69AAEBE20002A84ACA51EE0365B74A6BF4B2CC178153399F3BACC68CF3F50840095A33CBD7EF1393459E2C3004340109596AB6DEBF9A95CACB55B6F5FCD4A24580400A8586009C70C00D44401D8AB11A210002190DE1BC43872C006C45299463005EC0169AFFF6F9273269B89F4F80100507C00A84EB34B5F2772CB122D26016CA88C9BCC8BD4A05CA2CCABF90030534D3226B32D040147F802537B888CD59265C3CC01498A6B7BA7A1A08F005C401C86B10A358803D1FE24419300524F32AD2C6DA009080330DE2941B1006618450822A009C68998C1E0C017C0041A450A554A582D8034797FD73D4396C1848FC0A6F14503004340169D96BE1B11674A4804CD9DC26D006E20008747585D0AC001088550560F9019B0E004080160058798012804E4801232C0437B00F70A005100CFEE007A8010C02553007FC801A5100530C00F4B0027EE004CA64A480287C005E27EEE13DD83447D3009E754E29CDB5CD3C"

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