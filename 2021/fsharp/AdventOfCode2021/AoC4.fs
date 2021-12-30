﻿module AoC4

open Utils

let aoc4 () =
        printBanner 4

        let numbers = [15;62;2;39;49;25;65;28;84;59;75;24;20;76;60;55;17;7;93;69;32;23;44;81;8;67;41;56;43;89;95;97;61;77;64;37;29;10;79;26;51;48;5;86;71;58;78;90;57;82;45;70;11;14;13;50;68;94;99;22;47;12;1;74;18;46;4;6;88;54;83;96;63;66;35;27;36;72;42;98;0;52;40;91;33;21;34;85;3;38;31;92;9;87;19;73;30;16;53;80]

        let boards = [  
            [ 92 ;3 ;88 ;13 ;50 ;90 ;70 ;24 ;28 ;52 ;15 ;98 ;10 ;26 ;5 ;84 ;34 ;37 ;73 ;87 ;25 ;36 ;74 ;33 ;63 ];
            [ 66 ;64 ;50 ;75 ;53 ;73 ;24 ;80 ;84 ;5 ;72 ;20 ;68 ;1 ;99 ;83 ;57 ;44 ;60 ;52 ;32 ;15 ;59 ;48 ;98 ];
            [ 33 ;51 ;85 ;92 ;89 ;38 ;22 ;93 ;62 ;75 ;24 ;76 ;50 ;90 ;25 ;69 ;6 ;52 ;77 ;3 ;47 ;9 ;88 ;53 ;63 ];
            [ 78 ;75 ;29 ;32 ;73 ;22 ;85 ;42 ;1 ;23 ;80 ;98 ;81 ;58 ;9 ;61 ;76 ;69 ;83 ;53 ;71 ;7 ;15 ;11 ;95 ];
            [ 33 ;57 ;76 ;73 ;26 ;6 ;71 ;35 ;39 ;85 ;54 ;77 ;36 ;14 ;87 ;66 ;79 ;8 ;64 ;32 ;2 ;84 ;98 ;34 ;13 ];
            [ 43 ;51 ;16 ;95 ;59 ;22 ;53 ;6 ;49 ;94 ;32 ;72 ;46 ;23 ;37 ;40 ;85 ;39 ;45 ;74 ;87 ;62 ;69 ;98 ;0 ];
            [ 84 ;5 ;73 ;32 ;23 ;40 ;64 ;98 ;27 ;8 ;80 ;71 ;1 ;31 ;69 ;46 ;42 ;7 ;4 ;70 ;88 ;90 ;48 ;33 ;29 ];
            [ 3 ;91 ;8 ;98 ;50 ;54 ;70 ;29 ;94 ;25 ;17 ;77 ;33 ;46 ;12 ;28 ;36 ;39 ;40 ;5 ;22 ;38 ;51 ;69 ;45 ];
            [ 90 ;35 ;94 ;31 ;44 ;11 ;13 ;74 ;38 ;49 ;60 ;96 ;91 ;63 ;16 ;23 ;26 ;84 ;41 ;7 ;73 ;65 ;32 ;18 ;81 ];
            [ 62 ;42 ;35 ;21 ;87 ;57 ;27 ;26 ;71 ;94 ;73 ;92 ;77 ;53 ;86 ;1 ;60 ;38 ;75 ;43 ;10 ;70 ;55 ;84 ;5 ];
            [ 3 ;58 ;57 ;66 ;51 ;67 ;94 ;37 ;86 ;25 ;33 ;11 ;4 ;36 ;83 ;64 ;2 ;0 ;13 ;59 ;77 ;19 ;80 ;93 ;97 ];
            [ 99 ;13 ;24 ;49 ;90 ;96 ;15 ;10 ;67 ;2 ;9 ;78 ;5 ;42 ;80 ;28 ;75 ;51 ;58 ;82 ;31 ;83 ;20 ;60 ;48 ];
            [ 91 ;38 ;65 ;34 ;58 ;71 ;28 ;66 ;64 ;72 ;63 ;10 ;83 ;37 ;56 ;84 ;39 ;19 ;51 ;74 ;23 ;90 ;81 ;85 ;13 ];
            [ 12 ;42 ;10 ;11 ;29 ;99 ;60 ;24 ;94 ;25 ;9 ;40 ;76 ;33 ;97 ;32 ;75 ;16 ;37 ;27 ;15 ;69 ;54 ;52 ;22 ];
            [ 31 ;40 ;33 ;45 ;89 ;61 ;82 ;9 ;32 ;75 ;60 ;88 ;91 ;27 ;62 ;79 ;94 ;36 ;83 ;25 ;56 ;39 ;8 ;13 ;55 ];
            [ 39 ;95 ;92 ;2 ;56 ;88 ;70 ;63 ;62 ;13 ;49 ;43 ;46 ;0 ;47 ;83 ;42 ;44 ;7 ;26 ;60 ;27 ;69 ;73 ;29 ];
            [ 54 ;67 ;26 ;19 ;45 ;8 ;50 ;86 ;51 ;92 ;60 ;98 ;31 ;95 ;53 ;24 ;71 ;55 ;22 ;63 ;4 ;38 ;21 ;35 ;32 ];
            [ 4 ;34 ;26 ;32 ;58 ;16 ;67 ;76 ;78 ;46 ;73 ;95 ;68 ;56 ;60 ;35 ;40 ;42 ;6 ;87 ;7 ;97 ;54 ;92 ;24 ];
            [ 98 ;80 ;66 ;95 ;14 ;73 ;19 ;94 ;63 ;60 ;52 ;18 ;28 ;72 ;26 ;33 ;93 ;56 ;4 ;21 ;59 ;68 ;74 ;48 ;3 ];
            [ 7 ;27 ;84 ;80 ;79 ;1 ;21 ;11 ;37 ;47 ;88 ;38 ;30 ;8 ;72 ;4 ;52 ;13 ;19 ;26 ;57 ;6 ;58 ;0 ;98 ];
            [ 62 ;50 ;0 ;37 ;77 ;32 ;31 ;2 ;53 ;4 ;74 ;56 ;41 ;23 ;59 ;60 ;89 ;94 ;54 ;39 ;76 ;98 ;20 ;61 ;82 ];
            [ 35 ;90 ;5 ;80 ;18 ;45 ;20 ;60 ;8 ;77 ;26 ;17 ;61 ;55 ;29 ;24 ;76 ;3 ;41 ;64 ;4 ;74 ;85 ;10 ;82 ];
            [ 62 ;23 ;27 ;89 ;61 ;45 ;65 ;30 ;14 ;66 ;52 ;72 ;48 ;99 ;0 ;5 ;40 ;42 ;81 ;37 ;93 ;4 ;67 ;2 ;9 ];
            [ 27 ;87 ;68 ;50 ;41 ;18 ;60 ;12 ;45 ;48 ;93 ;38 ;8 ;6 ;13 ;99 ;37 ;59 ;94 ;64 ;40 ;55 ;63 ;67 ;31 ];
            [ 70 ;4 ;34 ;49 ;71 ;36 ;81 ;52 ;62 ;55 ;18 ;64 ;63 ;85 ;5 ;72 ;99 ;77 ;76 ;54 ;22 ;23 ;0 ;1 ;37 ];
            [ 34 ;88 ;69 ;20 ;30 ;73 ;11 ;93 ;68 ;56 ;78 ;35 ;80 ;22 ;24 ;15 ;95 ;32 ;51 ;25 ;67 ;91 ;52 ;5 ;14 ];
            [ 8 ;54 ;26 ;34 ;71 ;16 ;47 ;39 ;96 ;58 ;4 ;95 ;38 ;6 ;45 ;94 ;63 ;18 ;99 ;72 ;19 ;91 ;80 ;73 ;30 ];
            [ 77 ;9 ;78 ;76 ;60 ;8 ;31 ;73 ;74 ;17 ;22 ;25 ;7 ;64 ;47 ;75 ;32 ;89 ;87 ;40 ;13 ;44 ;10 ;95 ;49 ];
            [ 78 ;3 ;90 ;99 ;6 ;22 ;52 ;25 ;53 ;72 ;55 ;98 ;77 ;56 ;32 ;85 ;86 ;0 ;7 ;12 ;74 ;84 ;33 ;45 ;1 ];
            [ 57 ;53 ;26 ;54 ;69 ;56 ;8 ;58 ;91 ;40 ;65 ;97 ;44 ;51 ;2 ;85 ;60 ;72 ;22 ;89 ;66 ;16 ;67 ;90 ;93 ];
            [ 9 ;93 ;65 ;94 ;29 ;2 ;80 ;7 ;16 ;79 ;11 ;5 ;21 ;73 ;50 ;20 ;70 ;37 ;48 ;85 ;99 ;3 ;55 ;58 ;8 ];
            [ 26 ;37 ;60 ;63 ;47 ;21 ;39 ;69 ;68 ;22 ;83 ;94 ;55 ;91 ;80 ;35 ;89 ;6 ;45 ;17 ;23 ;85 ;84 ;73 ;7 ];
            [ 74 ;36 ;81 ;41 ;8 ;14 ;22 ;30 ;86 ;90 ;84 ;97 ;11 ;67 ;77 ;42 ;47 ;55 ;76 ;64 ;95 ;92 ;59 ;93 ;53 ];
            [ 64 ;16 ;19 ;68 ;50 ;90 ;12 ;47 ;40 ;62 ;86 ;1 ;48 ;2 ;58 ;96 ;79 ;92 ;46 ;91 ;14 ;85 ;59 ;45 ;30 ];
            [ 3 ;1 ;55 ;13 ;5 ;59 ;85 ;50 ;42 ;20 ;67 ;99 ;17 ;29 ;39 ;30 ;35 ;23 ;49 ;25 ;89 ;53 ;21 ;9 ;6 ];
            [ 90 ;91 ;47 ;99 ;37 ;82 ;24 ;56 ;27 ;2 ;95 ;57 ;33 ;4 ;97 ;51 ;26 ;29 ;67 ;98 ;21 ;62 ;42 ;43 ;9 ];
            [ 92 ;16 ;89 ;24 ;96 ;31 ;18 ;2 ;64 ;20 ;6 ;34 ;99 ;50 ;85 ;13 ;32 ;19 ;43 ;37 ;48 ;47 ;23 ;78 ;77 ];
            [ 95 ;16 ;87 ;61 ;6 ;46 ;15 ;24 ;72 ;60 ;43 ;56 ;80 ;35 ;53 ;97 ;25 ;98 ;42 ;14 ;51 ;11 ;10 ;3 ;45 ];
            [ 96 ;42 ;4 ;45 ;40 ;65 ;8 ;17 ;58 ;23 ;53 ;38 ;14 ;12 ;84 ;68 ;92 ;11 ;6 ;51 ;87 ;22 ;5 ;99 ;0 ];
            [ 45 ;51 ;26 ;18 ;91 ;7 ;31 ;95 ;37 ;74 ;66 ;41 ;48 ;20 ;87 ;99 ;96 ;64 ;53 ;0 ;3 ;28 ;15 ;46 ;79 ];
            [ 66 ;34 ;23 ;78 ;12 ;65 ;72 ;33 ;14 ;5 ;4 ;59 ;3 ;62 ;64 ;7 ;60 ;31 ;52 ;87 ;80 ;39 ;27 ;58 ;74 ];
            [ 91 ;94 ;64 ;46 ;28 ;99 ;29 ;79 ;58 ;0 ;18 ;19 ;24 ;59 ;16 ;3 ;73 ;52 ;9 ;86 ;37 ;61 ;1 ;93 ;68 ];
            [ 37 ;98 ;80 ;41 ;53 ;85 ;18 ;55 ;31 ;17 ;39 ;61 ;63 ;97 ;52 ;47 ;22 ;99 ;50 ;88 ;48 ;14 ;9 ;93 ;96 ];
            [ 11 ;66 ;89 ;91 ;34 ;98 ;25 ;53 ;7 ;65 ;42 ;32 ;9 ;14 ;77 ;85 ;87 ;26 ;12 ;64 ;45 ;99 ;29 ;88 ;4 ];
            [ 63 ;3 ;16 ;13 ;33 ;28 ;32 ;37 ;90 ;11 ;94 ;44 ;18 ;38 ;68 ;30 ;87 ;95 ;52 ;58 ;79 ;43 ;53 ;70 ;19 ];
            [ 94 ;67 ;56 ;43 ;47 ;77 ;37 ;93 ;90 ;92 ;66 ;48 ;98 ;20 ;61 ;51 ;2 ;85 ;57 ;11 ;22 ;84 ;79 ;17 ;72 ];
            [ 86 ;59 ;15 ;85 ;5 ;93 ;41 ;23 ;53 ;62 ;46 ;48 ;70 ;57 ;49 ;17 ;45 ;32 ;79 ;12 ;64 ;73 ;26 ;6 ;9 ];
            [ 12 ;88 ;27 ;43 ;21 ;66 ;42 ;84 ;82 ;62 ;94 ;46 ;96 ;63 ;86 ;69 ;79 ;40 ;39 ;92 ;22 ;87 ;71 ;44 ;53 ];
            [ 89 ;26 ;45 ;78 ;25 ;21 ;40 ;70 ;66 ;33 ;97 ;80 ;94 ;18 ;1 ;12 ;55 ;20 ;24 ;39 ;7 ;32 ;31 ;37 ;72 ];
            [ 15 ;56 ;39 ;57 ;40 ;67 ;59 ;26 ;30 ;90 ;84 ;2 ;41 ;25 ;7 ;96 ;23 ;79 ;99 ;85 ;13 ;10 ;86 ;51 ;53 ];
            [ 73 ;8 ;79 ;19 ;48 ;29 ;36 ;89 ;62 ;22 ;13 ;96 ;59 ;91 ;10 ;90 ;9 ;1 ;78 ;65 ;83 ;50 ;24 ;88 ;60 ];
            [ 20 ;61 ;63 ;82 ;53 ;86 ;11 ;55 ;10 ;85 ;5 ;37 ;65 ;21 ;54 ;89 ;75 ;59 ;73 ;48 ;41 ;50 ;29 ;71 ;93 ];
            [ 81 ;13 ;46 ;17 ;47 ;95 ;19 ;33 ;91 ;55 ;5 ;73 ;54 ;50 ;98 ;63 ;77 ;30 ;40 ;58 ;9 ;57 ;94 ;92 ;20 ];
            [ 54 ;99 ;94 ;23 ;81 ;32 ;86 ;50 ;28 ;8 ;69 ;18 ;11 ;39 ;67 ;10 ;79 ;91 ;15 ;43 ;13 ;98 ;55 ;16 ;22 ];
            [ 83 ;99 ;54 ;12 ;80 ;94 ;61 ;49 ;33 ;62 ;16 ;23 ;68 ;87 ;10 ;1 ;76 ;25 ;89 ;71 ;8 ;45 ;74 ;28 ;27 ];
            [ 66 ;28 ;72 ;76 ;33 ;9 ;99 ;27 ;96 ;60 ;84 ;67 ;35 ;50 ;79 ;55 ;44 ;18 ;98 ;13 ;94 ;70 ;42 ;21 ;65 ];
            [ 96 ;97 ;79 ;75 ;46 ;11 ;65 ;41 ;72 ;92 ;87 ;59 ;26 ;70 ;10 ;37 ;8 ;68 ;73 ;63 ;55 ;95 ;84 ;49 ;50 ];
            [ 51 ;27 ;63 ;31 ;24 ;82 ;11 ;87 ;6 ;2 ;75 ;57 ;85 ;1 ;46 ;91 ;71 ;72 ;13 ;56 ;10 ;64 ;65 ;49 ;69 ];
            [ 36 ;26 ;67 ;61 ;84 ;99 ;10 ;2 ;24 ;47 ;35 ;28 ;65 ;57 ;91 ;30 ;27 ;1 ;78 ;14 ;96 ;50 ;70 ;38 ;37 ];
            [ 62 ;33 ;41 ;98 ;35 ;80 ;92 ;4 ;48 ;70 ;2 ;11 ;23 ;15 ;52 ;83 ;39 ;79 ;81 ;1 ;54 ;93 ;27 ;18 ;24 ];
            [ 12 ;75 ;20 ;81 ;23 ;77 ;99 ;47 ;24 ;82 ;92 ;29 ;85 ;30 ;21 ;49 ;45 ;98 ;4 ;91 ;9 ;53 ;28 ;1 ;54 ];
            [ 72 ;46 ;53 ;3 ;19 ;83 ;49 ;39 ;12 ;22 ;47 ;62 ;58 ;14 ;79 ;82 ;69 ;84 ;75 ;1 ;67 ;7 ;21 ;45 ;65 ];
            [ 43 ;21 ;47 ;84 ;94 ;93 ;53 ;37 ;44 ;15 ;48 ;10 ;59 ;35 ;41 ;91 ;78 ;98 ;34 ;66 ;85 ;75 ;95 ;92 ;39 ];
            [ 94 ;6 ;17 ;16 ;12 ;39 ;41 ;11 ;65 ;78 ;97 ;85 ;49 ;64 ;72 ;59 ;84 ;83 ;42 ;28 ;32 ;96 ;46 ;89 ;44 ];
            [ 54 ;29 ;71 ;64 ;78 ;32 ;13 ;52 ;58 ;28 ;84 ;85 ;95 ;26 ;86 ;23 ;41 ;70 ;53 ;87 ;27 ;15 ;57 ;16 ;2 ];
            [ 92 ;99 ;45 ;81 ;32 ;86 ;25 ;56 ;76 ;52 ;95 ;3 ;6 ;88 ;1 ;71 ;70 ;24 ;19 ;62 ;59 ;16 ;11 ;2 ;34 ];
            [ 43 ;56 ;11 ;7 ;49 ;1 ;50 ;84 ;89 ;0 ;97 ;18 ;60 ;95 ;25 ;42 ;33 ;75 ;31 ;29 ;35 ;62 ;78 ;99 ;76 ];
            [ 98 ;84 ;53 ;3 ;22 ;54 ;87 ;41 ;76 ;83 ;39 ;27 ;36 ;79 ;78 ;55 ;1 ;89 ;48 ;81 ;49 ;26 ;77 ;96 ;67 ];
            [ 99 ;79 ;98 ;84 ;47 ;72 ;14 ;49 ;3 ;10 ;30 ;9 ;12 ;61 ;1 ;21 ;50 ;75 ;82 ;8 ;86 ;44 ;13 ;83 ;88 ];
            [ 82 ;94 ;33 ;70 ;17 ;97 ;22 ;45 ;53 ;55 ;19 ;71 ;35 ;54 ;52 ;41 ;42 ;63 ;65 ;3 ;88 ;10 ;67 ;81 ;69 ];
            [ 50 ;90 ;18 ;2 ;22 ;51 ;85 ;67 ;40 ;61 ;3 ;71 ;99 ;93 ;46 ;65 ;29 ;45 ;60 ;75 ;5 ;74 ;6 ;66 ;98 ];
            [ 68 ;80 ;59 ;29 ;5 ;6 ;16 ;45 ;44 ;92 ;74 ;13 ;64 ;30 ;25 ;69 ;94 ;54 ;97 ;3 ;42 ;47 ;26 ;19 ;17 ];
            [ 38 ;79 ;36 ;61 ;90 ;19 ;59 ;18 ;3 ;71 ;70 ;99 ;16 ;93 ;22 ;68 ;34 ;88 ;76 ;17 ;75 ;54 ;49 ;85 ;86 ];
            [ 8 ;96 ;80 ;15 ;28 ;23 ;98 ;58 ;84 ;69 ;21 ;3 ;60 ;38 ;97 ;43 ;56 ;34 ;25 ;64 ;24 ;1 ;39 ;44 ;78 ];
            [ 21 ;60 ;14 ;55 ;29 ;34 ;61 ;63 ;18 ;5 ;19 ;28 ;54 ;72 ;7 ;32 ;46 ;92 ;80 ;73 ;40 ;68 ;75 ;67 ;98 ];
            [ 57 ;21 ;88 ;90 ;33 ;63 ;5 ;25 ;24 ;49 ;29 ;7 ;23 ;19 ;13 ;85 ;93 ;75 ;41 ;68 ;98 ;69 ;12 ;76 ;31 ];
            [ 74 ;88 ;75 ;81 ;51 ;46 ;77 ;66 ;60 ;20 ;47 ;0 ;23 ;64 ;43 ;68 ;41 ;38 ;65 ;48 ;53 ;26 ;54 ;17 ;83 ];
            [ 39 ;21 ;78 ;15 ;99 ;25 ;97 ;24 ;70 ;56 ;57 ;66 ;31 ;75 ;71 ;47 ;91 ;30 ;4 ;65 ;94 ;11 ;77 ;76 ;44 ];
            [ 35 ;42 ;72 ;38 ;51 ;96 ;32 ;3 ;64 ;48 ;81 ;50 ;37 ;55 ;79 ;90 ;67 ;54 ;6 ;12 ;31 ;45 ;71 ;25 ;76 ];
            [ 60 ;58 ;90 ;3 ;74 ;48 ;16 ;49 ;30 ;46 ;68 ;51 ;0 ;80 ;96 ;26 ;71 ;36 ;27 ;28 ;57 ;94 ;79 ;42 ;50 ];
            [ 96 ;27 ;94 ;74 ;89 ;57 ;19 ;51 ;5 ;78 ;20 ;59 ;14 ;73 ;69 ;8 ;41 ;79 ;76 ;32 ;24 ;98 ;63 ;46 ;13 ];
            [ 20 ;53 ;42 ;70 ;86 ;12 ;49 ;96 ;0 ;77 ;31 ;26 ;38 ;22 ;87 ;51 ;78 ;60 ;36 ;13 ;57 ;8 ;73 ;94 ;7 ];
            [ 75 ;39 ;93 ;85 ;99 ;78 ;50 ;3 ;96 ;68 ;62 ;10 ;28 ;80 ;41 ;89 ;40 ;46 ;69 ;19 ;37 ;13 ;16 ;2 ;67 ];
            [ 85 ;24 ;99 ;70 ;20 ;31 ;60 ;41 ;63 ;81 ;34 ;87 ;93 ;39 ;37 ;55 ;43 ;44 ;25 ;78 ;97 ;21 ;3 ;28 ;40 ];
            [ 44 ;14 ;92 ;89 ;62 ;90 ;76 ;84 ;52 ;33 ;78 ;54 ;26 ;32 ;9 ;85 ;99 ;25 ;10 ;55 ;28 ;23 ;22 ;97 ;94 ];
            [ 13 ;38 ;37 ;98 ;15 ;78 ;62 ;9 ;50 ;2 ;77 ;68 ;65 ;18 ;74 ;90 ;21 ;95 ;53 ;33 ;60 ;25 ;17 ;64 ;1 ];
            [ 45 ;87 ;64 ;33 ;66 ;31 ;85 ;19 ;90 ;48 ;74 ;3 ;70 ;77 ;9 ;44 ;46 ;61 ;91 ;32 ;0 ;15 ;94 ;65 ;22 ];
            [ 12 ;1 ;66 ;47 ;3 ;63 ;7 ;2 ;42 ;21 ;6 ;75 ;44 ;26 ;82 ;52 ;45 ;48 ;89 ;68 ;96 ;92 ;25 ;15 ;76 ];
            [ 64 ;16 ;49 ;71 ;28 ;7 ;45 ;63 ;74 ;21 ;87 ;25 ;46 ;23 ;9 ;0 ;31 ;92 ;24 ;77 ;65 ;78 ;22 ;60 ;75 ];
            [ 4 ;54 ;58 ;83 ;60 ;25 ;12 ;82 ;0 ;73 ;32 ;62 ;2 ;31 ;49 ;64 ;18 ;35 ;19 ;10 ;61 ;46 ;43 ;34 ;38 ];
            [ 84 ;48 ;30 ;77 ;79 ;15 ;42 ;4 ;25 ;72 ;28 ;78 ;22 ;7 ;70 ;46 ;6 ;31 ;24 ;41 ;98 ;93 ;34 ;37 ;71 ];
            [ 72 ;9 ;71 ;1 ;54 ;97 ;98 ;91 ;90 ;92 ;23 ;88 ;13 ;87 ;68 ;45 ;36 ;86 ;41 ;56 ;69 ;16 ;24 ;20 ;93 ];
            [ 35 ;34 ;60 ;67 ;52 ;12 ;73 ;99 ;89 ;61 ;33 ;94 ;27 ;16 ;15 ;4 ;64 ;47 ;22 ;74 ;24 ;53 ;71 ;66 ;76 ];
            [ 32 ;62 ;51 ;58 ;1 ;11 ;76 ;75 ;33 ;60 ;55 ;54 ;39 ;52 ;48 ;6 ;87 ;3 ;8 ;99 ;40 ;65 ;24 ;66 ;70 ];
            [ 61 ;23 ;22 ;12 ;74 ;73 ;25 ;85 ;11 ;0 ;20 ;38 ;26 ;88 ;33 ;63 ;39 ;50 ;49 ;83 ;71 ;18 ;56 ;37 ;7 ];
            [ 46 ;12 ;90 ;52 ;48 ;73 ;24 ;78 ;34 ;94 ;15 ;19 ;47 ;72 ;89 ;60 ;35 ;74 ;67 ;30 ;13 ;18 ;17 ;93 ;0 ];
            [ 37 ;87 ;0 ;94 ;27 ;18 ;56 ;54 ;4 ;33 ;84 ;20 ;12 ;86 ;6 ;5 ;40 ;52 ;97 ;74 ;63 ;59 ;69 ;19 ;77 ];
            [ 15 ;53 ;20 ;35 ;14 ;24 ;25 ;63 ;85 ;79 ;65 ;96 ;2 ;60 ;50 ;72 ;10 ;77 ;12 ;92 ;32 ;94 ;95 ;16 ;71 ];
            [ 78 ;52 ;55 ;20 ;40 ;33 ;66 ;81 ;48 ;18 ;32 ;69 ;13 ;5 ;84 ;23 ;67 ;68 ;61 ;34 ;11 ;63 ;4 ;93 ;65 ];
            [ 51 ;89 ;37 ;46 ;29 ;69 ;56 ;71 ;9 ;91 ;28 ;54 ;7 ;16 ;31 ;67 ;5 ;97 ;42 ;43 ;98 ;32 ;65 ;34 ;30 ]
        ]

        let reverseBoard board = board 
                                    |> List.mapi (fun i a -> (i, a)) 
                                    |> List.groupBy (fun (i,a) -> i % 5) 
                                    |> List.map (fun (c, elements) -> (elements |> List.map( fun (i, e) -> e)))
                                    |> List.concat

        let checkBoardRowSolved board givenNumbers = 
            board |> List.chunkBySize 5 |> List.exists (fun x -> 0 = (x |> List.except givenNumbers |> List.length) )

        let checkBoardSolved board givenNumbers =
            (checkBoardRowSolved board givenNumbers) || (checkBoardRowSolved (reverseBoard board) givenNumbers)

        let rec solveBoard board pendingNumbers givenNumbers = 
            match pendingNumbers with 
            | [] -> failwith "Board not solved with given numbers"
            | currentNumber :: nextPendingNumbers ->
                let nextGivenNumbers = currentNumber :: givenNumbers

                let solved = checkBoardSolved board nextGivenNumbers

                if solved = false then
                    solveBoard board nextPendingNumbers nextGivenNumbers
                else
                    let numPendingNumbers = nextPendingNumbers |> List.length
                    let notMarkedNumbers = board |> List.except nextGivenNumbers
                   
                    (numPendingNumbers, currentNumber, notMarkedNumbers)

        let (numPendingNumbers, winningNumber, notMarkedNumbers) = 
            boards
                |> List.map (fun x -> solveBoard x numbers [])
                |> List.sortByDescending (fun (x, y, z) -> x)
                |> List.head

        let resultNumber = winningNumber * (notMarkedNumbers |> List.sum)

        let result1 = sprintf "Num pending numbers %d, WinningNumber: %d, PendingNumbers: %A, Result: %d " numPendingNumbers  winningNumber notMarkedNumbers resultNumber
        printFirstStarResult result1

        // Part 2

        let (numPendingNumbers2, losingNumber, notMarkedNumbers2) = 
            boards
                |> List.map (fun x -> solveBoard x numbers [])
                |> List.sortBy (fun (x, y, z) -> x)
                |> List.head

        let resultNumber2 = losingNumber * (notMarkedNumbers2 |> List.sum)

        let result2 = sprintf "Num pending numbers %d, LosingNumber: %d, PendingNumbers: %A, Result: %d " numPendingNumbers2  losingNumber notMarkedNumbers2 resultNumber2
        printSecondStarResult result2
        

