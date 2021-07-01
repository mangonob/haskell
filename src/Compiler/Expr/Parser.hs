{-# OPTIONS_GHC -w #-}
module  Compiler.Expr.Parser where

import Compiler.Expr.Token
import Compiler.Expr.Lexer (lexer)
import Control.Monad.State (MonadState (get, put), State, evalState)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 (Int)
	| HappyAbsSyn6 (())

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,48) ([9856,13313,9,60,10,0,0,39424,53252,36,0,6016,0,2560,0,16384,147,32,9424,9856,13313,40969,73,0,0,24576,0,53251,36,0,0,18848,0,0,0,120,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","program","expr","decs","dec","let","in","int","var","'='","'+'","'-'","'*'","'/'","'('","')'","%eof"]
        bit_start = st Prelude.* 19
        bit_end = (st Prelude.+ 1) Prelude.* 19
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..18]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_3
action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (17) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_3
action_1 (10) = happyShift action_4
action_1 (11) = happyShift action_5
action_1 (14) = happyShift action_6
action_1 (17) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (13) = happyShift action_15
action_2 (14) = happyShift action_16
action_2 (15) = happyShift action_17
action_2 (16) = happyShift action_18
action_2 _ = happyReduce_1

action_3 (9) = happyShift action_13
action_3 (11) = happyShift action_14
action_3 (6) = happyGoto action_11
action_3 (7) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_9

action_5 _ = happyReduce_11

action_6 (8) = happyShift action_3
action_6 (10) = happyShift action_4
action_6 (11) = happyShift action_5
action_6 (14) = happyShift action_6
action_6 (17) = happyShift action_7
action_6 (5) = happyGoto action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (8) = happyShift action_3
action_7 (10) = happyShift action_4
action_7 (11) = happyShift action_5
action_7 (14) = happyShift action_6
action_7 (17) = happyShift action_7
action_7 (5) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (19) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (13) = happyShift action_15
action_9 (14) = happyShift action_16
action_9 (15) = happyShift action_17
action_9 (16) = happyShift action_18
action_9 (18) = happyShift action_27
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_10

action_11 (9) = happyShift action_26
action_11 (11) = happyShift action_14
action_11 (7) = happyGoto action_25
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_12

action_13 (8) = happyShift action_3
action_13 (10) = happyShift action_4
action_13 (11) = happyShift action_5
action_13 (14) = happyShift action_6
action_13 (17) = happyShift action_7
action_13 (5) = happyGoto action_24
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (12) = happyShift action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (8) = happyShift action_3
action_15 (10) = happyShift action_4
action_15 (11) = happyShift action_5
action_15 (14) = happyShift action_6
action_15 (17) = happyShift action_7
action_15 (5) = happyGoto action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (8) = happyShift action_3
action_16 (10) = happyShift action_4
action_16 (11) = happyShift action_5
action_16 (14) = happyShift action_6
action_16 (17) = happyShift action_7
action_16 (5) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_3
action_17 (10) = happyShift action_4
action_17 (11) = happyShift action_5
action_17 (14) = happyShift action_6
action_17 (17) = happyShift action_7
action_17 (5) = happyGoto action_20
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_3
action_18 (10) = happyShift action_4
action_18 (11) = happyShift action_5
action_18 (14) = happyShift action_6
action_18 (17) = happyShift action_7
action_18 (5) = happyGoto action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_8

action_20 _ = happyReduce_7

action_21 (15) = happyShift action_17
action_21 (16) = happyShift action_18
action_21 _ = happyReduce_6

action_22 (15) = happyShift action_17
action_22 (16) = happyShift action_18
action_22 _ = happyReduce_5

action_23 (8) = happyShift action_3
action_23 (10) = happyShift action_4
action_23 (11) = happyShift action_5
action_23 (14) = happyShift action_6
action_23 (17) = happyShift action_7
action_23 (5) = happyGoto action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_3

action_25 _ = happyReduce_13

action_26 (8) = happyShift action_3
action_26 (10) = happyShift action_4
action_26 (11) = happyShift action_5
action_26 (14) = happyShift action_6
action_26 (17) = happyShift action_7
action_26 (5) = happyGoto action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_2

action_28 _ = happyReduce_4

action_29 (13) = happyShift action_15
action_29 (14) = happyShift action_16
action_29 (15) = happyShift action_17
action_29 (16) = happyShift action_18
action_29 _ = happyReduce_14

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	_
	 =  HappyAbsSyn5
		 (happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (seq happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 +  happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 - happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 *  happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 `div` happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyTerminal (Int happy_var_1))
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 ((- happy_var_2)
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyMonadReduce 1 5 happyReduction_11
happyReduction_11 ((HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( do
                                                   env <- get
                                                   case lookup happy_var_1 env of
                                                     Just v -> return v
                                                     Nothing -> error $ "undefined var " ++ happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (seq happy_var_2 happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyMonadReduce 3 7 happyReduction_14
happyReduction_14 ((HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen ((( do
                                                     env <- get 
                                                     put ((happy_var_1, happy_var_3) : env)))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyNewToken action sts stk [] =
	action 19 19 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Let -> cont 8;
	In -> cont 9;
	Int happy_dollar_dollar -> cont 10;
	Var happy_dollar_dollar -> cont 11;
	Eq -> cont 12;
	Plus -> cont 13;
	Minus -> cont 14;
	Times -> cont 15;
	Div -> cont 16;
	OB -> cont 17;
	CB -> cont 18;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 19 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Environment a -> (a -> Environment b) -> Environment b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Environment a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Environment a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Environment a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Environment a
parseError = undefined

type Environment = State [(String, Int)]
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
