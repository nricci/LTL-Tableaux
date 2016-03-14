module Tableaux where

import Parser

import qualified Data.Set as S
import Data.Set (Set)
import qualified SetAux as S

import qualified Relation as R
import Relation (Relation)

import qualified Data.Map as M
import Data.Map (Map)

import LTL
import Closure

import Data.Maybe        (isJust, fromJust, isJust, fromMaybe)
import Data.List 		(sortBy, (\\))
import Data.Ord

import Debug.Trace





data Node 	= 	OrNode {formulas :: Set Formula} 
			|	AndNode {formulas :: Set Formula} 
			deriving (Eq, Ord)

instance Show Node where
	show (AndNode s) = "<AndNode " ++ show (S.toList s) ++ ">"
	show (OrNode s) = "<OrNode " ++ show (S.toList s) ++ ">"


isOr :: Node -> Bool
isOr (OrNode _) = True
isOr _ = False

isAnd :: Node -> Bool
isAnd (AndNode _) = True
isAnd _ = False


-- TABLEAUX
data Tableaux = Tableaux {
					root :: Node, 					--root  
					nodes :: Set Node,				-- nodes 
					rel :: Relation Node Node		-- relation
				} deriving (Show, Eq)



-- Tableaux Aux
succesors :: Tableaux -> Node -> Set Node
succesors t n = case R.lookupDom n $ rel t of
									Just s -> s
									Nothing -> S.empty

-- Tableaux Aux
predecesors :: Tableaux -> Node -> Set Node
predecesors t n = case R.lookupRan n $ rel t of
									Just s -> s
									Nothing -> S.empty




make_tableaux :: Set Formula -> Tableaux
make_tableaux s = let root = OrNode s in 
					Tableaux root (S.singleton root) R.empty

frontier :: Tableaux -> Set Node
frontier t = nodes t S.\\ (R.dom . rel) t


blocks :: Node -> Set Node
blocks (OrNode s) = S.map AndNode $ closure s


tiles :: Node -> Set Node
tiles (AndNode s) = let x = S.map chopX (S.filter isX s) in
						if (S.null x) then
							S.empty
						else
							S.singleton (OrNode x)
					


expand_node :: Node -> Tableaux -> (Tableaux, Set Node)
expand_node n t@(Tableaux root nodes rel) = case n of
												OrNode _ ->	(Tableaux root nodes' rel', S.empty)
																where
																	succs = S.toList (blocks n)
																	nodes' = nodes `S.union` S.fromList succs
																	rel' = rel `R.union` R.fromList [(n,succ) | succ <- succs]
												AndNode s -> case succs of
																[] ->	(t, S.empty) --(Tableaux root nodes' rel', S.singleton dummy)
																		--	where
																		--	dummy = (OrNode s)
																		--	nodes' = dummy `S.insert` nodes
																		--	rel' = rel `R.union` R.fromList [(n,dummy), (dummy,n)]
																x:_ -> (Tableaux root nodes' rel', S.empty)
																			where 
																				nodes' = nodes `S.union` S.fromList succs
																				rel' = rel `R.union` R.fromList [(n,succ) | succ <- succs]
																where succs = S.toList (tiles n)



do_tableaux_impl :: Set Node -> Tableaux -> Tableaux
do_tableaux_impl used t = case S.pick $ nodes t S.\\ used of
							(Just n) -> let (t',s) = expand_node n t in 
											do_tableaux_impl (used S.+ s S.<+ n) $  t'
							Nothing -> t

	--where xxx = trace ("nodes " ++ (show $ S.size $ nodes t) ++ " - relation " ++ (show $ R.size $ rel t))						

do_tableaux :: Tableaux -> Tableaux
do_tableaux t = do_tableaux_impl S.empty t



{-------------------------


		DELETION


--------------------------}			

{-
delete_node :: Node -> Tableaux -> Tableaux
delete_node n t@(Tableaux root nodes rel) = case n of
										(AndNode _) -> Tableaux root nodes' rel'
										(OrNode _) -> S.fold delete_node (Tableaux root nodes' rel') (predecesors t n)

		where
			rel' = R.fromList $ filter (\(x,y) -> x /= n && y /= n) $ R.toList rel --nodes' R.<| rel R.|> nodes'
			nodes' = nodes S.\\ nn			
			nn = S.singleton n 

delete_nodes :: Set Node -> Tableaux -> Tableaux
delete_nodes s t = S.fold delete_node t s

delete_inconsistent :: Tableaux -> Tableaux
delete_inconsistent t = let inc = S.filter inconsistent_node $ nodes t in
												S.fold delete_node t inc


inconsistent_node :: Node -> Bool
inconsistent_node (AndNode s) = inconsistent s
inconsistent_node (OrNode s) = inconsistent s



delete_unreachable :: Tableaux -> Tableaux
delete_unreachable t@(Tableaux root nodes rel) = let lookup = R.lookupDom root (R.closure rel) in
													let reach = if isJust lookup then fromJust lookup else S.empty in 
														S.fold delete_node t (nodes S.\\ reach)


delete_or :: Tableaux -> Tableaux
delete_or t = let to_delete = S.filter (\n -> isOr n && S.null (succesors t n)) (nodes t) in
												S.fold delete_node t to_delete




checkEU :: Tableaux -> Node -> Formula -> Bool
checkEU t n f = let val = fromJust $ M.lookup n (tagmap t f) in
					val /= pinf && val /= ninf-- checkEU_impl t n f S.empty


checkEU_impl :: Tableaux -> Node -> Formula -> Set Node -> Bool
checkEU_impl t n@(OrNode s) f@(E (U g h)) v = S.some (\m -> checkEU_impl t m f v) ((succesors t n) `S.difference` v)
checkEU_impl t n@(AndNode s) f@(E (U g h)) v = if S.member h s then 
													True
												else
													S.member g s && S.some (\m -> checkEU_impl t m f (n `S.insert` v)) ((succesors t n) `S.difference` v)


delete_EU :: Tableaux -> Tableaux
delete_EU t = let eus = [(n,f) | n <- S.toList $ nodes t, f <- S.toList $ formulas n, isEU f] in
				let to_delete0 = filter (\(m,g) -> not (checkEU t m g)) eus in
					let to_delete1 = map fst to_delete0 in 
						foldl (flip delete_node) t to_delete1




checkAU :: Tableaux -> Node -> Formula -> Bool
checkAU t n f = let val = fromJust $ M.lookup n (tagmap t f) in
					val /= pinf && val /= ninf --checkAU_impl t n f S.empty


checkAU_impl :: Tableaux -> Node -> Formula -> Set Node -> Bool
checkAU_impl t n@(OrNode s) f@(A (U g h)) v = S.some (\m -> checkAU_impl t m f (n `S.insert` v)) ((succesors t n) `S.difference` v)
checkAU_impl t n@(AndNode s) f@(A (U g h)) v = if S.member h s then 
													True
												else
													S.member g s && S.all (\m -> checkAU_impl t m f (n `S.insert` v)) ((succesors t n) `S.difference` v)

delete_AU :: Tableaux -> Tableaux
delete_AU t = let aus = [(n,f) | n <- S.toList $ nodes t, f <- S.toList $ formulas n, isAU f] in
				let to_delete0 = {-(trace ("aus = " ++ show aus)) -} filter (\(m,g) -> not (checkAU t m g)) aus in
					let to_delete1 = map fst to_delete0 in 
						foldl (flip delete_node) t to_delete1



deletion_rules :: Tableaux -> Tableaux
deletion_rules = delete_EU . delete_AU . delete_or . delete_unreachable . delete_inconsistent 


refine_tableaux :: Tableaux -> Tableaux
refine_tableaux t = let t' = deletion_rules t in
						if t' == t then t else refine_tableaux t'

-}

refine_tableaux :: Tableaux -> Tableaux
refine_tableaux t = t






{-------------------------  TAGGING  ---------------------------}

-- AUX
pinf :: Int
pinf = 2^29-1

ninf :: Int
ninf = -2^29
-------
{-
init_tag :: Tableaux -> Formula -> Map Node Int
init_tag t g@(A (U f h)) = M.fromList $ l0 ++ linf
	
	where
		l0 = [(n,0) | n <- goal_nodes]
		linf = [(n,pinf) | n <- (S.toList $ nodes t) \\ goal_nodes]
		goal_nodes = [n | n <- S.toList $ nodes t, S.member h $ formulas n]

init_tag t g@(E (U f h)) = M.fromList $ l0 ++ linf
	
	where
		l0 = [(n,0) | n <- goal_nodes]
		linf = [(n,pinf) | n <- (S.toList $ nodes t) \\ goal_nodes]
		goal_nodes = [n | n <- S.toList $ nodes t, S.member h $ formulas n]		



evolve_tag :: Tableaux -> Formula -> Map Node Int -> Map Node Int
evolve_tag t g m = foldl (new_tag t g) m $ M.keys m


new_tag :: Tableaux -> Formula -> Map Node Int -> Node -> Map Node Int
new_tag t g@(A (U f h)) m n@(AndNode s) = 	if S.member g s && S.member f s && fromJust (M.lookup n m) == pinf && S.all (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (1 + (S.findMax $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n))) m
											else
												m
new_tag t g@(A (U f h)) m n@(OrNode s) = 	if S.member g s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n)) m
											else
												m
new_tag t g@(E (U f h)) m n@(AndNode s) = 	if S.member g s && S.member f s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (1 + (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n))) m
											else
												m
new_tag t g@(E (U f h)) m n@(OrNode s) = 	if S.member g s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n)) m
											else
												m

compute_tag ::  Tableaux -> Formula -> Map Node Int -> Map Node Int
compute_tag t g m = let m' = evolve_tag t g m in 
						if m' == m then
							m
						else
							compute_tag t g m'	  


tagmap ::  Tableaux -> Formula -> Map Node Int
tagmap t g = iterate (compute_tag t g) (init_tag t g) !! (S.size . nodes $ t)




-}



{-------------------------


		PRINTING


--------------------------}	




numberNodes :: Tableaux -> Map Node Int
numberNodes t = M.fromList (zip (S.toList $ nodes t) [1..])

-- Auxiliary function
flipMap :: (Ord a, Ord b) => Map a b -> Map b a
flipMap x = M.fromList (map (\p -> (snd p, fst p)) (M.toList x))

 
(+++) :: String -> String -> String
(+++) = (\x y -> x ++ ("\n" ++ y))

tab2dot :: Tableaux -> String
tab2dot t@(Tableaux r nodes rel) =  let num = numberNodes t in
								"digraph {\n" ++ 
								(S.fold (+++) "" (S.map (renderNode num) nodes)) ++ 
								"\n" ++ 
								renderArcs num t
								++ "\n}"

order_flas :: Set Formula -> [String]
order_flas s = reverse $ sortBy (comparing length) (S.toList (S.map show selection))

	where selection = s-- S.filter isLiteral s 	

renderNode :: Map Node Int -> Node -> String
renderNode num n@(OrNode s) = let label = foldr (+++) "" (order_flas s) in
										"n" ++ show (num M.! n) ++ " [shape=circle, label=\"" ++ label ++ "\"];" 
renderNode num n@(AndNode s) = let label = foldr (+++) "" (order_flas s) in
										"n" ++ show (num M.! n) ++ " [shape=square, label=\"" ++ label ++ "\"];" 





renderArcs :: Map Node Int -> Tableaux -> String
renderArcs num t@(Tableaux r nodes rel) = foldl (+++) "" (map (uncurry (renderOneArc num)) (R.toList rel))

renderOneArc :: Map Node Int -> Node -> Node -> String
renderOneArc num n n' = "n" ++ show (num M.! n) ++ " -> " ++ "n" ++ show (num M.! n') 







tab2dotWithTags :: (Show a) => Tableaux -> Map Node a -> String
tab2dotWithTags t@(Tableaux r nodes rel) m =  let num = numberNodes t in
								"digraph {\n" ++ 
								(S.fold (+++) "" (S.map (renderNodeWithTags num m) nodes)) ++ 
								"\n" ++ 
								renderArcs num t
								++ "\n}"

renderNodeWithTags :: (Show a) => Map Node Int -> Map Node a -> Node -> String
renderNodeWithTags num tag n@(OrNode s) = let label = "tag = " ++ (show $ tag M.! n) ++ "\n" ++ foldr (+++) "" (order_flas s) in
										"n" ++ show (num M.! n) ++ " [shape=circle, label=\"" ++ label ++ "\"];" 
renderNodeWithTags num tag n@(AndNode s) = let label = "tag = " ++ (show $ tag M.! n) ++ "\n" ++ foldr (+++) "" (order_flas s) in
										"n" ++ show (num M.! n) ++ " [shape=square, label=\"" ++ label ++ "\"];" 



