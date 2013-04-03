﻿module Shaftesbury.Span.Program

open Shaftesbury.FSharp.Utils
open Shaftesbury.Span.XML.Parser
open Shaftesbury.Span.LCH.ExpandedFormat
open Shaftesbury.Span.HK.ExpandedFormat

// download these from http://www.asx.com.au/sfe/span.htm
let XMLfilenames = 
    [
        "ASXCLFEndOfDayRiskParameterFile130306.spn";
        "CFDEndOfDayRiskParameterFile120720.spn";
        "NZFEndOfDayRiskParameterFile130306.spn";
        "SFEEndOfDayRiskParameterFile130128.spn";
        "ASXCLFEndOfDayRiskParameterFile130319.spn";
    ] |> List.map (fun nm -> @"C:\Users\Bob\development\data\Span\"+nm)

// download these from http://www.hkex.com.hk/eng/market/rm/rm_dcrm/riskdata/rpf/riskdata.asp
let HKfilenames = 
    [
        "rci______-_____-__-_____-130314-0844.lis";
        "rci______-_____-__-_____-130314-0948.lis";
        "rci______-_____-__-_____-130314-1037.lis";
        "rci______-_____-__-_____-130314-1242.lis";
        "rcp______-_____-__-_____-130314.lis";
        "rpi______-_____-__-_____-130314-0844.lis";
        "rpi______-_____-__-_____-130314-0948.lis";
        "rpi______-_____-__-_____-130314-1037.lis";
        "rpi______-_____-__-_____-130314-1242.lis";
        "rpp______-_____-__-_____-130314.lis";
    ]
    |> List.map (fun nm -> @"C:\Users\Bob\development\data\Span\RPF_130314\"+nm)

// ftp://ftp.cmegroup.com/pub/span/data/
let CMEfilenames = 
    [
        "cme.20130228.a.pa2";
    ]
    |> List.map (fun nm -> @"C:\Users\Bob\development\data\Span\"+nm)

// download these from http://www.lchclearnet.com/data_downloads/ltd/span.asp
let LCHfilenames = 
    [
//        "EF130305.DAT";
        "HF130305.DAT";
        "lf130305.dat";
        "RF130305.DAT";
        "DF130305.DAT";
        "MF130305.DAT";
        "of130305.dat";
        "xf130305.dat";
    ]
    |> List.map (fun nm -> @"C:\Users\Bob\development\data\Span\"+nm)

[<EntryPoint>]
let main args = 
    if Array.length args <> 0 then
        match args.[0] with
        | "XML" ->
            let trees = XMLfilenames |> List.map prepareXMLFile |> List.map Shaftesbury.Span.XML.Parser.readXML
            0
        | "LCH" ->
            let splitRows =
                LCHfilenames |> 
                List.map (fun filename ->
                            use fs = new System.IO.StreamReader(filename)
                            let lines = readFrom fs
                            let splitRows = 
                                lines |> Seq.map (fun row ->
                                                    let lengths = Shaftesbury.Span.LCH.ExpandedFormat.findLengthArray row
                                                    Seq.unfold splitter (row, lengths) |> List.ofSeq) |> List.ofSeq

                            fs.Close()
                            splitRows)
            let trees = splitRows |> List.map (fun recordSet -> recordSet |> List.map Shaftesbury.Span.LCH.ExpandedFormat.convert |> Shaftesbury.Span.LCH.ExpandedFormat.buildTree)
            0
        | "HK" ->
            let splitRows =
                HKfilenames |> 
                List.map (fun filename ->
                            use fs = new System.IO.StreamReader(filename)
                            let lines = readFrom fs
                            let splitRows = 
                                lines |> Seq.map (fun row ->
                                                    let lengths = Shaftesbury.Span.HK.ExpandedFormat.findLengthArray row
                                                    Seq.unfold splitter (row, lengths) |> List.ofSeq) |> List.ofSeq

                            fs.Close()
                            splitRows)
            let trees = splitRows |> List.map (fun recordSet -> recordSet |> List.map Shaftesbury.Span.HK.ExpandedFormat.convert |> Shaftesbury.Span.HK.ExpandedFormat.buildTree)
            0
        | "CME" ->
            let splitRows =
                CMEfilenames |> 
                List.map (fun filename ->
                            use fs = new System.IO.StreamReader(filename)
                            let lines = readFrom fs
                            let splitRows = 
                                lines |> Seq.map (fun row ->
                                                    let lengths = Shaftesbury.Span.CME.ExpandedFormat.findLengthArray row
                                                    Seq.unfold splitter (row, lengths) |> List.ofSeq) |> List.ofSeq

                            fs.Close()
                            splitRows)
            let trees = splitRows |> List.map (fun recordSet -> recordSet |> List.map Shaftesbury.Span.CME.ExpandedFormat.convert |> Shaftesbury.Span.CME.ExpandedFormat.buildTree)
            0
        | _ -> 1
    else
        1