{-# OPTIONS_GHC -w #-}
module Compiler.Tiger.Parser_ where

import Compiler.Tiger.Token
import qualified Compiler.Tiger.AbSyn as A
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 (A.Expr)
	| HappyAbsSyn6 (A.Var)
	| HappyAbsSyn8 ([A.Expr])
	| HappyAbsSyn10 ([A.Field])
	| HappyAbsSyn11 (A.Field)
	| HappyAbsSyn12 ([A.Dec])
	| HappyAbsSyn13 (A.Dec)
	| HappyAbsSyn14 (A.Type)
	| HappyAbsSyn15 ([A.Record])
	| HappyAbsSyn17 (A.Record)
	| HappyAbsSyn18 (Token)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,391) ([0,8300,16452,7168,0,2075,4113,1792,0,0,65024,31,0,0,0,8,0,0,17,0,6912,4360,16,7,0,0,0,1,0,0,0,0,0,0,0,0,2075,4113,1792,0,0,0,0,45056,4225,259,112,27648,17440,64,28,0,0,0,0,0,0,0,0,0,21760,0,0,0,0,0,0,2075,4145,1792,49152,16902,1028,448,0,0,32,64,0,0,0,16,0,0,0,0,0,0,8190,0,0,640,0,0,0,0,0,0,4096,63488,127,0,232,0,0,0,0,0,8,0,256,65504,1,6912,4360,16,7,0,0,0,1,33200,272,28673,0,8300,16452,7168,0,2075,4113,1792,49152,16902,1028,448,45056,4225,257,112,27648,17440,64,28,6912,4360,16,7,1728,1090,49156,1,33200,272,28673,0,8300,16452,7168,0,2075,4113,1792,49152,16902,1028,448,45056,4225,257,112,0,0,32736,0,0,0,8184,0,0,0,30,0,0,32768,7,0,0,57344,1,0,0,30720,0,0,0,7680,0,0,0,1920,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,6,0,0,57344,511,0,0,0,0,0,0,65056,31,45056,4225,257,112,27648,17440,64,28,0,0,0,0,5824,1090,49156,1,0,0,16384,0,0,0,4096,0,0,0,1024,49152,16902,1028,448,45056,4225,257,112,0,0,0,0,0,0,0,0,0,32896,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,65056,31,0,0,65408,7,0,34816,0,0,0,0,0,0,1728,1090,49156,1,0,0,0,0,0,2,0,0,2075,4113,1792,0,0,0,256,0,0,0,0,0,0,65504,1,0,32,32760,0,0,0,32,0,0,64,2048,0,0,64,0,0,64,8,0,0,0,0,0,16384,0,65408,7,0,0,65504,1,0,0,0,0,1728,1090,49156,1,0,0,0,0,0,0,4096,0,0,0,1024,49152,16902,1028,448,0,64,16,64,27648,17440,64,28,0,0,0,0,0,0,8190,0,33200,272,28673,0,0,57344,511,0,0,63488,127,0,0,65024,31,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,1,0,0,0,0,0,57344,511,0,0,0,128,0,0,8,0,0,8192,0,0,0,0,0,0,0,1024,0,0,0,16,8190,0,33200,272,28673,0,0,0,4096,0,0,0,1024,0,0,8193,0,45056,4225,257,112,0,0,8,0,0,0,0,4,0,0,0,0,0,0,0,0,0,57344,511,0,0,0,1024,49152,16902,1028,448,0,0,0,0,0,0,0,0,0,0,32760,0,0,0,8190,0,0,0,8,0,8300,16452,7168,0,0,63488,127,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","program","expr","lvalue","lvalue_","expr_seq","params","records","field","decs","dec","ty","ty_fields","ty_fields_","ty_field","type_id","while","for","to","break","let","in","end","function","var","type","array","if","then","else","do","of","nil","','","':'","';'","'('","')'","'['","']'","'{'","'}'","'.'","'+'","'-'","'*'","'/'","'='","'<>'","'<'","'<='","'>'","'>='","'&'","'|'","':='","string","int","id","%eof"]
        bit_start = st Prelude.* 62
        bit_end = (st Prelude.+ 1) Prelude.* 62
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..61]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (19) = happyShift action_5
action_0 (20) = happyShift action_6
action_0 (22) = happyShift action_7
action_0 (23) = happyShift action_8
action_0 (30) = happyShift action_9
action_0 (35) = happyShift action_10
action_0 (39) = happyShift action_11
action_0 (47) = happyShift action_12
action_0 (59) = happyShift action_13
action_0 (60) = happyShift action_14
action_0 (61) = happyShift action_15
action_0 (4) = happyGoto action_16
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_5
action_1 (20) = happyShift action_6
action_1 (22) = happyShift action_7
action_1 (23) = happyShift action_8
action_1 (30) = happyShift action_9
action_1 (35) = happyShift action_10
action_1 (39) = happyShift action_11
action_1 (47) = happyShift action_12
action_1 (59) = happyShift action_13
action_1 (60) = happyShift action_14
action_1 (61) = happyShift action_15
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (46) = happyShift action_32
action_2 (47) = happyShift action_33
action_2 (48) = happyShift action_34
action_2 (49) = happyShift action_35
action_2 (50) = happyShift action_36
action_2 (51) = happyShift action_37
action_2 (52) = happyShift action_38
action_2 (53) = happyShift action_39
action_2 (54) = happyShift action_40
action_2 (55) = happyShift action_41
action_2 (56) = happyShift action_42
action_2 (57) = happyShift action_43
action_2 _ = happyReduce_1

action_3 (58) = happyShift action_31
action_3 _ = happyReduce_5

action_4 (41) = happyShift action_29
action_4 (45) = happyShift action_30
action_4 _ = happyReduce_35

action_5 (19) = happyShift action_5
action_5 (20) = happyShift action_6
action_5 (22) = happyShift action_7
action_5 (23) = happyShift action_8
action_5 (30) = happyShift action_9
action_5 (35) = happyShift action_10
action_5 (39) = happyShift action_11
action_5 (47) = happyShift action_12
action_5 (59) = happyShift action_13
action_5 (60) = happyShift action_14
action_5 (61) = happyShift action_15
action_5 (5) = happyGoto action_28
action_5 (6) = happyGoto action_3
action_5 (7) = happyGoto action_4
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (61) = happyShift action_27
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_31

action_8 (12) = happyGoto action_26
action_8 _ = happyReduce_48

action_9 (19) = happyShift action_5
action_9 (20) = happyShift action_6
action_9 (22) = happyShift action_7
action_9 (23) = happyShift action_8
action_9 (30) = happyShift action_9
action_9 (35) = happyShift action_10
action_9 (39) = happyShift action_11
action_9 (47) = happyShift action_12
action_9 (59) = happyShift action_13
action_9 (60) = happyShift action_14
action_9 (61) = happyShift action_15
action_9 (5) = happyGoto action_25
action_9 (6) = happyGoto action_3
action_9 (7) = happyGoto action_4
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_4

action_11 (19) = happyShift action_5
action_11 (20) = happyShift action_6
action_11 (22) = happyShift action_7
action_11 (23) = happyShift action_8
action_11 (30) = happyShift action_9
action_11 (35) = happyShift action_10
action_11 (39) = happyShift action_11
action_11 (40) = happyShift action_24
action_11 (47) = happyShift action_12
action_11 (59) = happyShift action_13
action_11 (60) = happyShift action_14
action_11 (61) = happyShift action_15
action_11 (5) = happyGoto action_22
action_11 (6) = happyGoto action_3
action_11 (7) = happyGoto action_4
action_11 (8) = happyGoto action_23
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (19) = happyShift action_5
action_12 (20) = happyShift action_6
action_12 (22) = happyShift action_7
action_12 (23) = happyShift action_8
action_12 (30) = happyShift action_9
action_12 (35) = happyShift action_10
action_12 (39) = happyShift action_11
action_12 (47) = happyShift action_12
action_12 (59) = happyShift action_13
action_12 (60) = happyShift action_14
action_12 (61) = happyShift action_15
action_12 (5) = happyGoto action_21
action_12 (6) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_3

action_14 _ = happyReduce_2

action_15 (39) = happyShift action_17
action_15 (41) = happyShift action_18
action_15 (43) = happyShift action_19
action_15 (45) = happyShift action_20
action_15 _ = happyReduce_34

action_16 (62) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (19) = happyShift action_5
action_17 (20) = happyShift action_6
action_17 (22) = happyShift action_7
action_17 (23) = happyShift action_8
action_17 (30) = happyShift action_9
action_17 (35) = happyShift action_10
action_17 (39) = happyShift action_11
action_17 (40) = happyShift action_77
action_17 (47) = happyShift action_12
action_17 (59) = happyShift action_13
action_17 (60) = happyShift action_14
action_17 (61) = happyShift action_15
action_17 (5) = happyGoto action_75
action_17 (6) = happyGoto action_3
action_17 (7) = happyGoto action_4
action_17 (9) = happyGoto action_76
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_5
action_18 (20) = happyShift action_6
action_18 (22) = happyShift action_7
action_18 (23) = happyShift action_8
action_18 (30) = happyShift action_9
action_18 (35) = happyShift action_10
action_18 (39) = happyShift action_11
action_18 (47) = happyShift action_12
action_18 (59) = happyShift action_13
action_18 (60) = happyShift action_14
action_18 (61) = happyShift action_15
action_18 (5) = happyGoto action_74
action_18 (6) = happyGoto action_3
action_18 (7) = happyGoto action_4
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (44) = happyShift action_72
action_19 (61) = happyShift action_73
action_19 (10) = happyGoto action_70
action_19 (11) = happyGoto action_71
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (61) = happyShift action_69
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_9

action_22 (46) = happyShift action_32
action_22 (47) = happyShift action_33
action_22 (48) = happyShift action_34
action_22 (49) = happyShift action_35
action_22 (50) = happyShift action_36
action_22 (51) = happyShift action_37
action_22 (52) = happyShift action_38
action_22 (53) = happyShift action_39
action_22 (54) = happyShift action_40
action_22 (55) = happyShift action_41
action_22 (56) = happyShift action_42
action_22 (57) = happyShift action_43
action_22 _ = happyReduce_40

action_23 (38) = happyShift action_67
action_23 (40) = happyShift action_68
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_7

action_25 (31) = happyShift action_66
action_25 (46) = happyShift action_32
action_25 (47) = happyShift action_33
action_25 (48) = happyShift action_34
action_25 (49) = happyShift action_35
action_25 (50) = happyShift action_36
action_25 (51) = happyShift action_37
action_25 (52) = happyShift action_38
action_25 (53) = happyShift action_39
action_25 (54) = happyShift action_40
action_25 (55) = happyShift action_41
action_25 (56) = happyShift action_42
action_25 (57) = happyShift action_43
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (24) = happyShift action_62
action_26 (26) = happyShift action_63
action_26 (27) = happyShift action_64
action_26 (28) = happyShift action_65
action_26 (13) = happyGoto action_61
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (58) = happyShift action_60
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (33) = happyShift action_59
action_28 (46) = happyShift action_32
action_28 (47) = happyShift action_33
action_28 (48) = happyShift action_34
action_28 (49) = happyShift action_35
action_28 (50) = happyShift action_36
action_28 (51) = happyShift action_37
action_28 (52) = happyShift action_38
action_28 (53) = happyShift action_39
action_28 (54) = happyShift action_40
action_28 (55) = happyShift action_41
action_28 (56) = happyShift action_42
action_28 (57) = happyShift action_43
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (19) = happyShift action_5
action_29 (20) = happyShift action_6
action_29 (22) = happyShift action_7
action_29 (23) = happyShift action_8
action_29 (30) = happyShift action_9
action_29 (35) = happyShift action_10
action_29 (39) = happyShift action_11
action_29 (47) = happyShift action_12
action_29 (59) = happyShift action_13
action_29 (60) = happyShift action_14
action_29 (61) = happyShift action_15
action_29 (5) = happyGoto action_58
action_29 (6) = happyGoto action_3
action_29 (7) = happyGoto action_4
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (61) = happyShift action_57
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (19) = happyShift action_5
action_31 (20) = happyShift action_6
action_31 (22) = happyShift action_7
action_31 (23) = happyShift action_8
action_31 (30) = happyShift action_9
action_31 (35) = happyShift action_10
action_31 (39) = happyShift action_11
action_31 (47) = happyShift action_12
action_31 (59) = happyShift action_13
action_31 (60) = happyShift action_14
action_31 (61) = happyShift action_15
action_31 (5) = happyGoto action_56
action_31 (6) = happyGoto action_3
action_31 (7) = happyGoto action_4
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (19) = happyShift action_5
action_32 (20) = happyShift action_6
action_32 (22) = happyShift action_7
action_32 (23) = happyShift action_8
action_32 (30) = happyShift action_9
action_32 (35) = happyShift action_10
action_32 (39) = happyShift action_11
action_32 (47) = happyShift action_12
action_32 (59) = happyShift action_13
action_32 (60) = happyShift action_14
action_32 (61) = happyShift action_15
action_32 (5) = happyGoto action_55
action_32 (6) = happyGoto action_3
action_32 (7) = happyGoto action_4
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (19) = happyShift action_5
action_33 (20) = happyShift action_6
action_33 (22) = happyShift action_7
action_33 (23) = happyShift action_8
action_33 (30) = happyShift action_9
action_33 (35) = happyShift action_10
action_33 (39) = happyShift action_11
action_33 (47) = happyShift action_12
action_33 (59) = happyShift action_13
action_33 (60) = happyShift action_14
action_33 (61) = happyShift action_15
action_33 (5) = happyGoto action_54
action_33 (6) = happyGoto action_3
action_33 (7) = happyGoto action_4
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (19) = happyShift action_5
action_34 (20) = happyShift action_6
action_34 (22) = happyShift action_7
action_34 (23) = happyShift action_8
action_34 (30) = happyShift action_9
action_34 (35) = happyShift action_10
action_34 (39) = happyShift action_11
action_34 (47) = happyShift action_12
action_34 (59) = happyShift action_13
action_34 (60) = happyShift action_14
action_34 (61) = happyShift action_15
action_34 (5) = happyGoto action_53
action_34 (6) = happyGoto action_3
action_34 (7) = happyGoto action_4
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (19) = happyShift action_5
action_35 (20) = happyShift action_6
action_35 (22) = happyShift action_7
action_35 (23) = happyShift action_8
action_35 (30) = happyShift action_9
action_35 (35) = happyShift action_10
action_35 (39) = happyShift action_11
action_35 (47) = happyShift action_12
action_35 (59) = happyShift action_13
action_35 (60) = happyShift action_14
action_35 (61) = happyShift action_15
action_35 (5) = happyGoto action_52
action_35 (6) = happyGoto action_3
action_35 (7) = happyGoto action_4
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (19) = happyShift action_5
action_36 (20) = happyShift action_6
action_36 (22) = happyShift action_7
action_36 (23) = happyShift action_8
action_36 (30) = happyShift action_9
action_36 (35) = happyShift action_10
action_36 (39) = happyShift action_11
action_36 (47) = happyShift action_12
action_36 (59) = happyShift action_13
action_36 (60) = happyShift action_14
action_36 (61) = happyShift action_15
action_36 (5) = happyGoto action_51
action_36 (6) = happyGoto action_3
action_36 (7) = happyGoto action_4
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (19) = happyShift action_5
action_37 (20) = happyShift action_6
action_37 (22) = happyShift action_7
action_37 (23) = happyShift action_8
action_37 (30) = happyShift action_9
action_37 (35) = happyShift action_10
action_37 (39) = happyShift action_11
action_37 (47) = happyShift action_12
action_37 (59) = happyShift action_13
action_37 (60) = happyShift action_14
action_37 (61) = happyShift action_15
action_37 (5) = happyGoto action_50
action_37 (6) = happyGoto action_3
action_37 (7) = happyGoto action_4
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (19) = happyShift action_5
action_38 (20) = happyShift action_6
action_38 (22) = happyShift action_7
action_38 (23) = happyShift action_8
action_38 (30) = happyShift action_9
action_38 (35) = happyShift action_10
action_38 (39) = happyShift action_11
action_38 (47) = happyShift action_12
action_38 (59) = happyShift action_13
action_38 (60) = happyShift action_14
action_38 (61) = happyShift action_15
action_38 (5) = happyGoto action_49
action_38 (6) = happyGoto action_3
action_38 (7) = happyGoto action_4
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_5
action_39 (20) = happyShift action_6
action_39 (22) = happyShift action_7
action_39 (23) = happyShift action_8
action_39 (30) = happyShift action_9
action_39 (35) = happyShift action_10
action_39 (39) = happyShift action_11
action_39 (47) = happyShift action_12
action_39 (59) = happyShift action_13
action_39 (60) = happyShift action_14
action_39 (61) = happyShift action_15
action_39 (5) = happyGoto action_48
action_39 (6) = happyGoto action_3
action_39 (7) = happyGoto action_4
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (19) = happyShift action_5
action_40 (20) = happyShift action_6
action_40 (22) = happyShift action_7
action_40 (23) = happyShift action_8
action_40 (30) = happyShift action_9
action_40 (35) = happyShift action_10
action_40 (39) = happyShift action_11
action_40 (47) = happyShift action_12
action_40 (59) = happyShift action_13
action_40 (60) = happyShift action_14
action_40 (61) = happyShift action_15
action_40 (5) = happyGoto action_47
action_40 (6) = happyGoto action_3
action_40 (7) = happyGoto action_4
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (19) = happyShift action_5
action_41 (20) = happyShift action_6
action_41 (22) = happyShift action_7
action_41 (23) = happyShift action_8
action_41 (30) = happyShift action_9
action_41 (35) = happyShift action_10
action_41 (39) = happyShift action_11
action_41 (47) = happyShift action_12
action_41 (59) = happyShift action_13
action_41 (60) = happyShift action_14
action_41 (61) = happyShift action_15
action_41 (5) = happyGoto action_46
action_41 (6) = happyGoto action_3
action_41 (7) = happyGoto action_4
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (19) = happyShift action_5
action_42 (20) = happyShift action_6
action_42 (22) = happyShift action_7
action_42 (23) = happyShift action_8
action_42 (30) = happyShift action_9
action_42 (35) = happyShift action_10
action_42 (39) = happyShift action_11
action_42 (47) = happyShift action_12
action_42 (59) = happyShift action_13
action_42 (60) = happyShift action_14
action_42 (61) = happyShift action_15
action_42 (5) = happyGoto action_45
action_42 (6) = happyGoto action_3
action_42 (7) = happyGoto action_4
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (19) = happyShift action_5
action_43 (20) = happyShift action_6
action_43 (22) = happyShift action_7
action_43 (23) = happyShift action_8
action_43 (30) = happyShift action_9
action_43 (35) = happyShift action_10
action_43 (39) = happyShift action_11
action_43 (47) = happyShift action_12
action_43 (59) = happyShift action_13
action_43 (60) = happyShift action_14
action_43 (61) = happyShift action_15
action_43 (5) = happyGoto action_44
action_43 (6) = happyGoto action_3
action_43 (7) = happyGoto action_4
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (46) = happyShift action_32
action_44 (47) = happyShift action_33
action_44 (48) = happyShift action_34
action_44 (49) = happyShift action_35
action_44 (50) = happyShift action_36
action_44 (51) = happyShift action_37
action_44 (52) = happyShift action_38
action_44 (53) = happyShift action_39
action_44 (54) = happyShift action_40
action_44 (55) = happyShift action_41
action_44 _ = happyReduce_23

action_45 (46) = happyShift action_32
action_45 (47) = happyShift action_33
action_45 (48) = happyShift action_34
action_45 (49) = happyShift action_35
action_45 (50) = happyShift action_36
action_45 (51) = happyShift action_37
action_45 (52) = happyShift action_38
action_45 (53) = happyShift action_39
action_45 (54) = happyShift action_40
action_45 (55) = happyShift action_41
action_45 _ = happyReduce_22

action_46 (46) = happyShift action_32
action_46 (47) = happyShift action_33
action_46 (48) = happyShift action_34
action_46 (49) = happyShift action_35
action_46 (50) = happyFail []
action_46 (51) = happyFail []
action_46 (52) = happyFail []
action_46 (53) = happyFail []
action_46 (54) = happyFail []
action_46 (55) = happyFail []
action_46 _ = happyReduce_21

action_47 (46) = happyShift action_32
action_47 (47) = happyShift action_33
action_47 (48) = happyShift action_34
action_47 (49) = happyShift action_35
action_47 (50) = happyFail []
action_47 (51) = happyFail []
action_47 (52) = happyFail []
action_47 (53) = happyFail []
action_47 (54) = happyFail []
action_47 (55) = happyFail []
action_47 _ = happyReduce_20

action_48 (46) = happyShift action_32
action_48 (47) = happyShift action_33
action_48 (48) = happyShift action_34
action_48 (49) = happyShift action_35
action_48 (50) = happyFail []
action_48 (51) = happyFail []
action_48 (52) = happyFail []
action_48 (53) = happyFail []
action_48 (54) = happyFail []
action_48 (55) = happyFail []
action_48 _ = happyReduce_19

action_49 (46) = happyShift action_32
action_49 (47) = happyShift action_33
action_49 (48) = happyShift action_34
action_49 (49) = happyShift action_35
action_49 (50) = happyFail []
action_49 (51) = happyFail []
action_49 (52) = happyFail []
action_49 (53) = happyFail []
action_49 (54) = happyFail []
action_49 (55) = happyFail []
action_49 _ = happyReduce_18

action_50 (46) = happyShift action_32
action_50 (47) = happyShift action_33
action_50 (48) = happyShift action_34
action_50 (49) = happyShift action_35
action_50 (50) = happyFail []
action_50 (51) = happyFail []
action_50 (52) = happyFail []
action_50 (53) = happyFail []
action_50 (54) = happyFail []
action_50 (55) = happyFail []
action_50 _ = happyReduce_17

action_51 (46) = happyShift action_32
action_51 (47) = happyShift action_33
action_51 (48) = happyShift action_34
action_51 (49) = happyShift action_35
action_51 (50) = happyFail []
action_51 (51) = happyFail []
action_51 (52) = happyFail []
action_51 (53) = happyFail []
action_51 (54) = happyFail []
action_51 (55) = happyFail []
action_51 _ = happyReduce_16

action_52 _ = happyReduce_15

action_53 _ = happyReduce_14

action_54 (48) = happyShift action_34
action_54 (49) = happyShift action_35
action_54 _ = happyReduce_13

action_55 (48) = happyShift action_34
action_55 (49) = happyShift action_35
action_55 _ = happyReduce_12

action_56 (46) = happyShift action_32
action_56 (47) = happyShift action_33
action_56 (48) = happyShift action_34
action_56 (49) = happyShift action_35
action_56 (50) = happyShift action_36
action_56 (51) = happyShift action_37
action_56 (52) = happyShift action_38
action_56 (53) = happyShift action_39
action_56 (54) = happyShift action_40
action_56 (55) = happyShift action_41
action_56 (56) = happyShift action_42
action_56 (57) = happyShift action_43
action_56 _ = happyReduce_6

action_57 _ = happyReduce_38

action_58 (42) = happyShift action_93
action_58 (46) = happyShift action_32
action_58 (47) = happyShift action_33
action_58 (48) = happyShift action_34
action_58 (49) = happyShift action_35
action_58 (50) = happyShift action_36
action_58 (51) = happyShift action_37
action_58 (52) = happyShift action_38
action_58 (53) = happyShift action_39
action_58 (54) = happyShift action_40
action_58 (55) = happyShift action_41
action_58 (56) = happyShift action_42
action_58 (57) = happyShift action_43
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (19) = happyShift action_5
action_59 (20) = happyShift action_6
action_59 (22) = happyShift action_7
action_59 (23) = happyShift action_8
action_59 (30) = happyShift action_9
action_59 (35) = happyShift action_10
action_59 (39) = happyShift action_11
action_59 (47) = happyShift action_12
action_59 (59) = happyShift action_13
action_59 (60) = happyShift action_14
action_59 (61) = happyShift action_15
action_59 (5) = happyGoto action_92
action_59 (6) = happyGoto action_3
action_59 (7) = happyGoto action_4
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (19) = happyShift action_5
action_60 (20) = happyShift action_6
action_60 (22) = happyShift action_7
action_60 (23) = happyShift action_8
action_60 (30) = happyShift action_9
action_60 (35) = happyShift action_10
action_60 (39) = happyShift action_11
action_60 (47) = happyShift action_12
action_60 (59) = happyShift action_13
action_60 (60) = happyShift action_14
action_60 (61) = happyShift action_15
action_60 (5) = happyGoto action_91
action_60 (6) = happyGoto action_3
action_60 (7) = happyGoto action_4
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_47

action_62 (19) = happyShift action_5
action_62 (20) = happyShift action_6
action_62 (22) = happyShift action_7
action_62 (23) = happyShift action_8
action_62 (25) = happyShift action_90
action_62 (30) = happyShift action_9
action_62 (35) = happyShift action_10
action_62 (39) = happyShift action_11
action_62 (47) = happyShift action_12
action_62 (59) = happyShift action_13
action_62 (60) = happyShift action_14
action_62 (61) = happyShift action_15
action_62 (5) = happyGoto action_22
action_62 (6) = happyGoto action_3
action_62 (7) = happyGoto action_4
action_62 (8) = happyGoto action_89
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (61) = happyShift action_88
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (61) = happyShift action_87
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (61) = happyShift action_86
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (19) = happyShift action_5
action_66 (20) = happyShift action_6
action_66 (22) = happyShift action_7
action_66 (23) = happyShift action_8
action_66 (30) = happyShift action_9
action_66 (35) = happyShift action_10
action_66 (39) = happyShift action_11
action_66 (47) = happyShift action_12
action_66 (59) = happyShift action_13
action_66 (60) = happyShift action_14
action_66 (61) = happyShift action_15
action_66 (5) = happyGoto action_85
action_66 (6) = happyGoto action_3
action_66 (7) = happyGoto action_4
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (19) = happyShift action_5
action_67 (20) = happyShift action_6
action_67 (22) = happyShift action_7
action_67 (23) = happyShift action_8
action_67 (30) = happyShift action_9
action_67 (35) = happyShift action_10
action_67 (39) = happyShift action_11
action_67 (47) = happyShift action_12
action_67 (59) = happyShift action_13
action_67 (60) = happyShift action_14
action_67 (61) = happyShift action_15
action_67 (5) = happyGoto action_84
action_67 (6) = happyGoto action_3
action_67 (7) = happyGoto action_4
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_8

action_69 _ = happyReduce_36

action_70 (36) = happyShift action_82
action_70 (44) = happyShift action_83
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_44

action_72 _ = happyReduce_24

action_73 (50) = happyShift action_81
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (42) = happyShift action_80
action_74 (46) = happyShift action_32
action_74 (47) = happyShift action_33
action_74 (48) = happyShift action_34
action_74 (49) = happyShift action_35
action_74 (50) = happyShift action_36
action_74 (51) = happyShift action_37
action_74 (52) = happyShift action_38
action_74 (53) = happyShift action_39
action_74 (54) = happyShift action_40
action_74 (55) = happyShift action_41
action_74 (56) = happyShift action_42
action_74 (57) = happyShift action_43
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (46) = happyShift action_32
action_75 (47) = happyShift action_33
action_75 (48) = happyShift action_34
action_75 (49) = happyShift action_35
action_75 (50) = happyShift action_36
action_75 (51) = happyShift action_37
action_75 (52) = happyShift action_38
action_75 (53) = happyShift action_39
action_75 (54) = happyShift action_40
action_75 (55) = happyShift action_41
action_75 (56) = happyShift action_42
action_75 (57) = happyShift action_43
action_75 _ = happyReduce_42

action_76 (36) = happyShift action_78
action_76 (40) = happyShift action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_10

action_78 (19) = happyShift action_5
action_78 (20) = happyShift action_6
action_78 (22) = happyShift action_7
action_78 (23) = happyShift action_8
action_78 (30) = happyShift action_9
action_78 (35) = happyShift action_10
action_78 (39) = happyShift action_11
action_78 (47) = happyShift action_12
action_78 (59) = happyShift action_13
action_78 (60) = happyShift action_14
action_78 (61) = happyShift action_15
action_78 (5) = happyGoto action_104
action_78 (6) = happyGoto action_3
action_78 (7) = happyGoto action_4
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_11

action_80 (34) = happyShift action_103
action_80 _ = happyReduce_37

action_81 (19) = happyShift action_5
action_81 (20) = happyShift action_6
action_81 (22) = happyShift action_7
action_81 (23) = happyShift action_8
action_81 (30) = happyShift action_9
action_81 (35) = happyShift action_10
action_81 (39) = happyShift action_11
action_81 (47) = happyShift action_12
action_81 (59) = happyShift action_13
action_81 (60) = happyShift action_14
action_81 (61) = happyShift action_15
action_81 (5) = happyGoto action_102
action_81 (6) = happyGoto action_3
action_81 (7) = happyGoto action_4
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (61) = happyShift action_73
action_82 (11) = happyGoto action_101
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_25

action_84 (46) = happyShift action_32
action_84 (47) = happyShift action_33
action_84 (48) = happyShift action_34
action_84 (49) = happyShift action_35
action_84 (50) = happyShift action_36
action_84 (51) = happyShift action_37
action_84 (52) = happyShift action_38
action_84 (53) = happyShift action_39
action_84 (54) = happyShift action_40
action_84 (55) = happyShift action_41
action_84 (56) = happyShift action_42
action_84 (57) = happyShift action_43
action_84 _ = happyReduce_41

action_85 (32) = happyShift action_100
action_85 (46) = happyShift action_32
action_85 (47) = happyShift action_33
action_85 (48) = happyShift action_34
action_85 (49) = happyShift action_35
action_85 (50) = happyShift action_36
action_85 (51) = happyShift action_37
action_85 (52) = happyShift action_38
action_85 (53) = happyShift action_39
action_85 (54) = happyShift action_40
action_85 (55) = happyShift action_41
action_85 (56) = happyShift action_42
action_85 (57) = happyShift action_43
action_85 _ = happyReduce_27

action_86 (50) = happyShift action_99
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (37) = happyShift action_97
action_87 (58) = happyShift action_98
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (39) = happyShift action_96
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (25) = happyShift action_95
action_89 (38) = happyShift action_67
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_32

action_91 (21) = happyShift action_94
action_91 (46) = happyShift action_32
action_91 (47) = happyShift action_33
action_91 (48) = happyShift action_34
action_91 (49) = happyShift action_35
action_91 (50) = happyShift action_36
action_91 (51) = happyShift action_37
action_91 (52) = happyShift action_38
action_91 (53) = happyShift action_39
action_91 (54) = happyShift action_40
action_91 (55) = happyShift action_41
action_91 (56) = happyShift action_42
action_91 (57) = happyShift action_43
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (46) = happyShift action_32
action_92 (47) = happyShift action_33
action_92 (48) = happyShift action_34
action_92 (49) = happyShift action_35
action_92 (50) = happyShift action_36
action_92 (51) = happyShift action_37
action_92 (52) = happyShift action_38
action_92 (53) = happyShift action_39
action_92 (54) = happyShift action_40
action_92 (55) = happyShift action_41
action_92 (56) = happyShift action_42
action_92 (57) = happyShift action_43
action_92 _ = happyReduce_29

action_93 _ = happyReduce_39

action_94 (19) = happyShift action_5
action_94 (20) = happyShift action_6
action_94 (22) = happyShift action_7
action_94 (23) = happyShift action_8
action_94 (30) = happyShift action_9
action_94 (35) = happyShift action_10
action_94 (39) = happyShift action_11
action_94 (47) = happyShift action_12
action_94 (59) = happyShift action_13
action_94 (60) = happyShift action_14
action_94 (61) = happyShift action_15
action_94 (5) = happyGoto action_118
action_94 (6) = happyGoto action_3
action_94 (7) = happyGoto action_4
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_33

action_96 (61) = happyShift action_117
action_96 (15) = happyGoto action_114
action_96 (16) = happyGoto action_115
action_96 (17) = happyGoto action_116
action_96 _ = happyReduce_58

action_97 (61) = happyShift action_111
action_97 (18) = happyGoto action_113
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (19) = happyShift action_5
action_98 (20) = happyShift action_6
action_98 (22) = happyShift action_7
action_98 (23) = happyShift action_8
action_98 (30) = happyShift action_9
action_98 (35) = happyShift action_10
action_98 (39) = happyShift action_11
action_98 (47) = happyShift action_12
action_98 (59) = happyShift action_13
action_98 (60) = happyShift action_14
action_98 (61) = happyShift action_15
action_98 (5) = happyGoto action_112
action_98 (6) = happyGoto action_3
action_98 (7) = happyGoto action_4
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (29) = happyShift action_109
action_99 (43) = happyShift action_110
action_99 (61) = happyShift action_111
action_99 (14) = happyGoto action_107
action_99 (18) = happyGoto action_108
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (19) = happyShift action_5
action_100 (20) = happyShift action_6
action_100 (22) = happyShift action_7
action_100 (23) = happyShift action_8
action_100 (30) = happyShift action_9
action_100 (35) = happyShift action_10
action_100 (39) = happyShift action_11
action_100 (47) = happyShift action_12
action_100 (59) = happyShift action_13
action_100 (60) = happyShift action_14
action_100 (61) = happyShift action_15
action_100 (5) = happyGoto action_106
action_100 (6) = happyGoto action_3
action_100 (7) = happyGoto action_4
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_45

action_102 (46) = happyShift action_32
action_102 (47) = happyShift action_33
action_102 (48) = happyShift action_34
action_102 (49) = happyShift action_35
action_102 (50) = happyShift action_36
action_102 (51) = happyShift action_37
action_102 (52) = happyShift action_38
action_102 (53) = happyShift action_39
action_102 (54) = happyShift action_40
action_102 (55) = happyShift action_41
action_102 (56) = happyShift action_42
action_102 (57) = happyShift action_43
action_102 _ = happyReduce_46

action_103 (19) = happyShift action_5
action_103 (20) = happyShift action_6
action_103 (22) = happyShift action_7
action_103 (23) = happyShift action_8
action_103 (30) = happyShift action_9
action_103 (35) = happyShift action_10
action_103 (39) = happyShift action_11
action_103 (47) = happyShift action_12
action_103 (59) = happyShift action_13
action_103 (60) = happyShift action_14
action_103 (61) = happyShift action_15
action_103 (5) = happyGoto action_105
action_103 (6) = happyGoto action_3
action_103 (7) = happyGoto action_4
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (46) = happyShift action_32
action_104 (47) = happyShift action_33
action_104 (48) = happyShift action_34
action_104 (49) = happyShift action_35
action_104 (50) = happyShift action_36
action_104 (51) = happyShift action_37
action_104 (52) = happyShift action_38
action_104 (53) = happyShift action_39
action_104 (54) = happyShift action_40
action_104 (55) = happyShift action_41
action_104 (56) = happyShift action_42
action_104 (57) = happyShift action_43
action_104 _ = happyReduce_43

action_105 (46) = happyShift action_32
action_105 (47) = happyShift action_33
action_105 (48) = happyShift action_34
action_105 (49) = happyShift action_35
action_105 (50) = happyShift action_36
action_105 (51) = happyShift action_37
action_105 (52) = happyShift action_38
action_105 (53) = happyShift action_39
action_105 (54) = happyShift action_40
action_105 (55) = happyShift action_41
action_105 (56) = happyShift action_42
action_105 (57) = happyShift action_43
action_105 _ = happyReduce_26

action_106 (46) = happyShift action_32
action_106 (47) = happyShift action_33
action_106 (48) = happyShift action_34
action_106 (49) = happyShift action_35
action_106 (50) = happyShift action_36
action_106 (51) = happyShift action_37
action_106 (52) = happyShift action_38
action_106 (53) = happyShift action_39
action_106 (54) = happyShift action_40
action_106 (55) = happyShift action_41
action_106 (56) = happyShift action_42
action_106 (57) = happyShift action_43
action_106 _ = happyReduce_28

action_107 _ = happyReduce_49

action_108 _ = happyReduce_54

action_109 (34) = happyShift action_125
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (61) = happyShift action_117
action_110 (15) = happyGoto action_124
action_110 (16) = happyGoto action_115
action_110 (17) = happyGoto action_116
action_110 _ = happyReduce_58

action_111 _ = happyReduce_62

action_112 (46) = happyShift action_32
action_112 (47) = happyShift action_33
action_112 (48) = happyShift action_34
action_112 (49) = happyShift action_35
action_112 (50) = happyShift action_36
action_112 (51) = happyShift action_37
action_112 (52) = happyShift action_38
action_112 (53) = happyShift action_39
action_112 (54) = happyShift action_40
action_112 (55) = happyShift action_41
action_112 (56) = happyShift action_42
action_112 (57) = happyShift action_43
action_112 _ = happyReduce_50

action_113 (58) = happyShift action_123
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (40) = happyShift action_122
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (36) = happyShift action_121
action_115 _ = happyReduce_57

action_116 _ = happyReduce_59

action_117 (37) = happyShift action_120
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (33) = happyShift action_119
action_118 (46) = happyShift action_32
action_118 (47) = happyShift action_33
action_118 (48) = happyShift action_34
action_118 (49) = happyShift action_35
action_118 (50) = happyShift action_36
action_118 (51) = happyShift action_37
action_118 (52) = happyShift action_38
action_118 (53) = happyShift action_39
action_118 (54) = happyShift action_40
action_118 (55) = happyShift action_41
action_118 (56) = happyShift action_42
action_118 (57) = happyShift action_43
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (19) = happyShift action_5
action_119 (20) = happyShift action_6
action_119 (22) = happyShift action_7
action_119 (23) = happyShift action_8
action_119 (30) = happyShift action_9
action_119 (35) = happyShift action_10
action_119 (39) = happyShift action_11
action_119 (47) = happyShift action_12
action_119 (59) = happyShift action_13
action_119 (60) = happyShift action_14
action_119 (61) = happyShift action_15
action_119 (5) = happyGoto action_133
action_119 (6) = happyGoto action_3
action_119 (7) = happyGoto action_4
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (61) = happyShift action_111
action_120 (18) = happyGoto action_132
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (61) = happyShift action_117
action_121 (17) = happyGoto action_131
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (37) = happyShift action_129
action_122 (50) = happyShift action_130
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (19) = happyShift action_5
action_123 (20) = happyShift action_6
action_123 (22) = happyShift action_7
action_123 (23) = happyShift action_8
action_123 (30) = happyShift action_9
action_123 (35) = happyShift action_10
action_123 (39) = happyShift action_11
action_123 (47) = happyShift action_12
action_123 (59) = happyShift action_13
action_123 (60) = happyShift action_14
action_123 (61) = happyShift action_15
action_123 (5) = happyGoto action_128
action_123 (6) = happyGoto action_3
action_123 (7) = happyGoto action_4
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (44) = happyShift action_127
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (61) = happyShift action_111
action_125 (18) = happyGoto action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_56

action_127 _ = happyReduce_55

action_128 (46) = happyShift action_32
action_128 (47) = happyShift action_33
action_128 (48) = happyShift action_34
action_128 (49) = happyShift action_35
action_128 (50) = happyShift action_36
action_128 (51) = happyShift action_37
action_128 (52) = happyShift action_38
action_128 (53) = happyShift action_39
action_128 (54) = happyShift action_40
action_128 (55) = happyShift action_41
action_128 (56) = happyShift action_42
action_128 (57) = happyShift action_43
action_128 _ = happyReduce_51

action_129 (61) = happyShift action_111
action_129 (18) = happyGoto action_135
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (19) = happyShift action_5
action_130 (20) = happyShift action_6
action_130 (22) = happyShift action_7
action_130 (23) = happyShift action_8
action_130 (30) = happyShift action_9
action_130 (35) = happyShift action_10
action_130 (39) = happyShift action_11
action_130 (47) = happyShift action_12
action_130 (59) = happyShift action_13
action_130 (60) = happyShift action_14
action_130 (61) = happyShift action_15
action_130 (5) = happyGoto action_134
action_130 (6) = happyGoto action_3
action_130 (7) = happyGoto action_4
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_60

action_132 _ = happyReduce_61

action_133 (46) = happyShift action_32
action_133 (47) = happyShift action_33
action_133 (48) = happyShift action_34
action_133 (49) = happyShift action_35
action_133 (50) = happyShift action_36
action_133 (51) = happyShift action_37
action_133 (52) = happyShift action_38
action_133 (53) = happyShift action_39
action_133 (54) = happyShift action_40
action_133 (55) = happyShift action_41
action_133 (56) = happyShift action_42
action_133 (57) = happyShift action_43
action_133 _ = happyReduce_30

action_134 (46) = happyShift action_32
action_134 (47) = happyShift action_33
action_134 (48) = happyShift action_34
action_134 (49) = happyShift action_35
action_134 (50) = happyShift action_36
action_134 (51) = happyShift action_37
action_134 (52) = happyShift action_38
action_134 (53) = happyShift action_39
action_134 (54) = happyShift action_40
action_134 (55) = happyShift action_41
action_134 (56) = happyShift action_42
action_134 (57) = happyShift action_43
action_134 _ = happyReduce_52

action_135 (50) = happyShift action_136
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (19) = happyShift action_5
action_136 (20) = happyShift action_6
action_136 (22) = happyShift action_7
action_136 (23) = happyShift action_8
action_136 (30) = happyShift action_9
action_136 (35) = happyShift action_10
action_136 (39) = happyShift action_11
action_136 (47) = happyShift action_12
action_136 (59) = happyShift action_13
action_136 (60) = happyShift action_14
action_136 (61) = happyShift action_15
action_136 (5) = happyGoto action_137
action_136 (6) = happyGoto action_3
action_136 (7) = happyGoto action_4
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (46) = happyShift action_32
action_137 (47) = happyShift action_33
action_137 (48) = happyShift action_34
action_137 (49) = happyShift action_35
action_137 (50) = happyShift action_36
action_137 (51) = happyShift action_37
action_137 (52) = happyShift action_38
action_137 (53) = happyShift action_39
action_137 (54) = happyShift action_40
action_137 (55) = happyShift action_41
action_137 (56) = happyShift action_42
action_137 (57) = happyShift action_43
action_137 _ = happyReduce_53

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.IntExpr (i_value happy_var_1) (pos happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.StringExpr (s_value happy_var_1) (pos happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.NilExpr (pos happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (A.VarExpr happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (A.AssignExpr happy_var_1 happy_var_3 (pos happy_var_2)
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn5
		 (A.SeqExpr []
	)

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (A.SeqExpr happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.UMinus happy_var_2 (pos happy_var_1)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.Call (id_value happy_var_1) [] (pos happy_var_1)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.Call (id_value happy_var_1) happy_var_3 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.PlusOp happy_var_3 (pos happy_var_2)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.MinusOp happy_var_3 (pos happy_var_2)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.TimesOp happy_var_3 (pos happy_var_2)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.DivideOp happy_var_3 (pos happy_var_2)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.EqOp happy_var_3 (pos happy_var_2)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.NeqOp happy_var_3 (pos happy_var_2)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.LtOp happy_var_3 (pos happy_var_2)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.LeOp happy_var_3 (pos happy_var_2)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.GtOp happy_var_3 (pos happy_var_2)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  5 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.OpExpr happy_var_1 A.GeOp happy_var_3 (pos happy_var_2)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  5 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.IFExpr happy_var_1 happy_var_3 (Just A.zero) (pos happy_var_2)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  5 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (A.IFExpr happy_var_1 A.one (Just happy_var_3) (pos happy_var_2)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  5 happyReduction_24
happyReduction_24 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.RecordsExpr (id_value happy_var_1) [] (pos happy_var_1)
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 5 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.RecordsExpr (id_value happy_var_1) happy_var_3 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 6 5 happyReduction_26
happyReduction_26 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.ArrayExpr (id_value happy_var_1) happy_var_3 happy_var_6 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 5 happyReduction_27
happyReduction_27 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.IFExpr happy_var_2 happy_var_4 Nothing (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 5 happyReduction_28
happyReduction_28 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.IFExpr happy_var_2 happy_var_4 (Just happy_var_6) (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 5 happyReduction_29
happyReduction_29 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.WhileExpr happy_var_2 happy_var_4 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 8 5 happyReduction_30
happyReduction_30 ((HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.ForExpr (id_value happy_var_2) happy_var_4 happy_var_6 happy_var_8 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  5 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (A.BreakExpr (pos happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 5 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.LetExpr happy_var_2 (A.SeqExpr []) (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 5 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (A.LetExpr happy_var_2 (A.SeqExpr happy_var_4) (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  6 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (simpleVar happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  6 happyReduction_35
happyReduction_35 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  7 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (A.FieldVar (simpleVar happy_var_1) (id_value happy_var_3) (pos happy_var_2)
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 7 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (A.IndexedVar (simpleVar happy_var_1) happy_var_3 (pos happy_var_2)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  7 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (A.FieldVar happy_var_1 (id_value happy_var_3) (pos happy_var_2)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 7 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (A.IndexedVar happy_var_1 happy_var_3 (pos happy_var_2)
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  8 happyReduction_40
happyReduction_40 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  8 happyReduction_41
happyReduction_41 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  9 happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  9 happyReduction_43
happyReduction_43 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  10 happyReduction_44
happyReduction_44 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  10 happyReduction_45
happyReduction_45 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  11 happyReduction_46
happyReduction_46 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (A.Field (id_value happy_var_1) happy_var_3 (pos happy_var_1)
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  12 happyReduction_47
happyReduction_47 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  12 happyReduction_48
happyReduction_48  =  HappyAbsSyn12
		 ([]
	)

happyReduce_49 = happyReduce 4 13 happyReduction_49
happyReduction_49 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (A.TypeDec (id_value happy_var_2) happy_var_4 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 13 happyReduction_50
happyReduction_50 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (A.VarDec (id_value happy_var_2) happy_var_4 Nothing (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 6 13 happyReduction_51
happyReduction_51 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (A.VarDec (id_value happy_var_2) happy_var_6 (Just (id_value happy_var_4)) (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 7 13 happyReduction_52
happyReduction_52 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (A.FuncDec (id_value happy_var_2) happy_var_4 Nothing happy_var_7 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 9 13 happyReduction_53
happyReduction_53 ((HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (A.FuncDec (id_value happy_var_2) happy_var_4 (Just (id_value happy_var_7)) happy_var_9 (pos happy_var_1)
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  14 happyReduction_54
happyReduction_54 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn14
		 (A.SimpleType (id_value happy_var_1) (pos happy_var_1)
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  14 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn15  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (A.Records happy_var_2 (pos happy_var_1)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  14 happyReduction_56
happyReduction_56 (HappyAbsSyn18  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (A.Array (id_value happy_var_3) (pos happy_var_1)
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  15 happyReduction_57
happyReduction_57 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  15 happyReduction_58
happyReduction_58  =  HappyAbsSyn15
		 ([]
	)

happyReduce_59 = happySpecReduce_1  16 happyReduction_59
happyReduction_59 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  16 happyReduction_60
happyReduction_60 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  17 happyReduction_61
happyReduction_61 (HappyAbsSyn18  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (A.Record (id_value happy_var_1) (id_value happy_var_3) (pos happy_var_1)
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  18 happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 62 62 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	While _ -> cont 19;
	For _ -> cont 20;
	To _ -> cont 21;
	Break _ -> cont 22;
	Let _ -> cont 23;
	In _ -> cont 24;
	End _ -> cont 25;
	Function _ -> cont 26;
	Var _ -> cont 27;
	Type _ -> cont 28;
	Array _ -> cont 29;
	If _ -> cont 30;
	Then _ -> cont 31;
	Else _ -> cont 32;
	Do _ -> cont 33;
	Of _ -> cont 34;
	Nil _ -> cont 35;
	Comma _ -> cont 36;
	Colon _ -> cont 37;
	Semicolon _ -> cont 38;
	LeftParen _ -> cont 39;
	RightParen _ -> cont 40;
	LeftBracket _ -> cont 41;
	RightBracket _ -> cont 42;
	LeftBrace _ -> cont 43;
	RightBrace _ -> cont 44;
	Dot _ -> cont 45;
	Plus _ -> cont 46;
	Minus _ -> cont 47;
	Times _ -> cont 48;
	Divide _ -> cont 49;
	Eq _ -> cont 50;
	NotEq _ -> cont 51;
	Lt _ -> cont 52;
	Le _ -> cont 53;
	Gt _ -> cont 54;
	Ge _ -> cont 55;
	And _ -> cont 56;
	Or _ -> cont 57;
	Assign _ -> cont 58;
	String _ _ -> cont 59;
	Int _ _ -> cont 60;
	ID _ _ -> cont 61;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 62 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError ts = error $ "syntax error on " ++ show (take 3 ts)

simpleVar :: Token -> A.Var
simpleVar t = A.SimpleVar (id_value t) (pos t)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
