module LTL where



import qualified Data.Set as S
import Data.Set (Set)
import qualified SetAux as S

import Prelude hiding (break, negate)

import Debug.Trace


-- LTL Formulas 
data Formula = 	And Formula Formula
			|	Or Formula Formula
			|	If Formula Formula
			|	Iff Formula Formula
			|	Xor Formula Formula
			|	Not Formula		
			|	Prop String
			|	TrueConst
			|	FalseConst
			-- temporal part
			| 	U Formula Formula
			|	W Formula Formula
			|	X Formula
			|	G Formula
			|	F Formula
				deriving (Eq,Ord)


instance Show Formula where
	show (And p q) 	=	show p ++ " && " ++ show q
	show (Or p q) 	=	show p ++ " || " ++ show q 
	show (If p q)	=	show p ++ " -> " ++ show q 
	show (Iff p q) 	=	show p ++ " == " ++ show q
	show (Xor p q) 	=	show p ++ " != " ++ show q
	show (Not p)		=	"!" ++ show p
	show (U p q) 	=	"(" ++ show p ++ " U " ++ show q ++ ")"
	show (W p q) 	=	"(" ++ show p ++ " W " ++ show q ++ ")"
	show (X p) 	=	"X (" ++ show p ++ ")"
	show (G p) 	=	"G (" ++ show p ++ ")"
	show (F p)	=	"F (" ++ show p ++ ")"
	show (Prop s) 	= 	s	
	show TrueConst 	= 	"true"
	show FalseConst	= 	"false"
	 


isX :: Formula -> Bool
isX (X _) = True
isX _ = False

isProp :: Formula -> Bool
isProp (Prop _) = True
isProp _ = False

isNegLiteral :: Formula -> Bool
isNegLiteral (Not f) = isProp f
isNegLiteral _ = False

isLiteral :: Formula -> Bool
isLiteral f = (isProp f) || (isNegLiteral f)

isNLit = isNegLiteral

isPLit = isProp


chopX :: Formula -> Formula
chopX (X f) = f
chopX f = f

isEventuality :: Formula -> Bool
isEventuality (U _ _) = True
isEventuality (F _) = True
isEventuality _ = False



elementary :: Formula -> Bool
elementary (Not f) = elementary f
elementary (X _) = True
elementary (Prop _) = True
elementary TrueConst = True
elementary FalseConst = True
elementary _ = False

isAlpha :: Formula -> Bool
isAlpha f = if elementary f then 
				False 
			else 
				1 == (length $ break_rule f) 

isBeta :: Formula -> Bool
isBeta f = if elementary f then 
				False 
			else 
				1 < (length $ break_rule f)  




break :: Formula -> Set (Set Formula)
break f = S.fromList (map S.fromList (break_rule f))

break_rule :: Formula -> [[Formula]]
-- Propositional
break_rule (Or p q)		=	[[p],[q]]
break_rule (And p q)	= 	[[p,q]]
break_rule (If p q)		=	[[negate p],[q]]
break_rule (Iff p q)	=	[[negate p, negate q],[p,q]]
break_rule (Xor p q)	=	[[negate p, q],[p, negate q]]
-- Temporal
break_rule (U p q)	=	[[q],[p, X (U p q)]]	
break_rule (G p)	=	[[p, X (G p)]]
break_rule (F p )	=	[[p],[X (F p)]]
break_rule f@_ = error ("Not Implemented Yet" ++ show f)




negate :: Formula -> Formula
-- Propositional
negate (Not f) 		= 	f
negate (Or p q)		=	And (negate p) (negate q)
negate (And p q)	= 	Or (negate p) (negate q)
negate (If p q)		=	And p (negate q)
negate (Iff p q)	=	Xor p q
negate (Xor p q)	=	Iff p q
negate (Prop p)		=	Not (Prop p)
negate TrueConst 			=	FalseConst
negate FalseConst 			=	TrueConst
negate f@_ = error ("Not Implemented Yet" ++ show f)





inconsistent :: Set Formula -> Bool
inconsistent s = (S.member FalseConst s) || (not $ S.null $ S.intersection pos (S.map chopNeg neg))
					where
						pos = S.filter isProp s
						neg = S.filter isNegLiteral s
						chopNeg = \f -> case f of
											(Not x) -> x
											_ -> f




