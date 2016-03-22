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

import qualified Model
import Model (Model)





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
							S.singleton (OrNode S.empty)
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
																[] ->	error "should not get here" --(Tableaux root nodes' rel', S.singleton dummy)
																		--	where
																		--		dummy = (OrNode S.empty)
																		--		nodes' = dummy `S.insert` nodes
																		--		rel' = rel `R.union` R.fromList [(n,dummy), (dummy,n)]
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


delete_node :: Node -> Tableaux -> Tableaux
delete_node n t@(Tableaux root nodes rel) = case n of
										(AndNode _) -> Tableaux root nodes' rel'
										(OrNode _) -> S.fold delete_node (Tableaux root nodes' rel') (predecesors t n)

		where
			--rel' = R.fromList $ filter (\(x,y) -> x /= n && y /= n) $ R.toList rel -- Highly Ineficient
			rel' = foldl (flip $ uncurry R.delete) rel (delete_succ ++ delete_pre)
			delete_pre = [(x,n) | x <- S.toList $ predecesors t n]
			delete_succ = [(n,x) | x <- S.toList $ succesors t n]
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




check_eventuality :: Tableaux -> Node -> Formula -> Bool
check_eventuality t n f = distance /= pinf
	
	where
		distance = fromJust $ M.lookup n (tagmap t f)

	


delete_non_event :: Tableaux -> Tableaux
delete_non_event t = foldl (flip delete_node) t to_delete1
	
	where
		to_delete1 = map fst to_delete0
		to_delete0 = filter (\(m,g) -> not (check_eventuality t m g)) candidates
		candidates = [(n,f) | n <- S.toList $ nodes t, f <- S.toList $ formulas n, isEventuality f]
				



deletion_rules :: Tableaux -> Tableaux
deletion_rules = delete_non_event . delete_or . delete_unreachable . delete_inconsistent 


refine_tableaux :: Tableaux -> Tableaux
refine_tableaux t = let t' = deletion_rules t in
						if t' == t then t else refine_tableaux t'








{-------------------------  TAGGING  --------------------------}

-- AUX
pinf :: Int
pinf = 2^29-1


init_tag :: Tableaux -> Formula -> Map Node Int
init_tag t g@(U f h) = M.fromList $ l0 ++ linf
	
	where
		l0 = [(n,0) | n <- goal_nodes]
		linf = [(n,pinf) | n <- (S.toList $ nodes t) \\ goal_nodes]
		goal_nodes = [n | n <- S.toList $ nodes t, S.member h $ formulas n]

init_tag t g@(F f) = M.fromList $ l0 ++ linf
	
	where
		l0 = [(n,0) | n <- goal_nodes]
		linf = [(n,pinf) | n <- (S.toList $ nodes t) \\ goal_nodes]
		goal_nodes = [n | n <- S.toList $ nodes t, S.member f $ formulas n]		



evolve_tag :: Tableaux -> Formula -> Map Node Int -> Map Node Int
evolve_tag t g m = foldl (new_tag t g) m $ M.keys m


new_tag :: Tableaux -> Formula -> Map Node Int -> Node -> Map Node Int
new_tag t g@(U f h) m n@(AndNode s) = 	if S.member g s && S.member f s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (1 + (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n))) m
											else
												m
new_tag t g@(U f h) m n@(OrNode s) = 	if S.member g s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n)) m
											else
												m
new_tag t g@(F f) m n@(AndNode s) = 	if S.member g s && (not $ S.member f s) && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
												M.insert n (1 + (S.findMin $ S.map (\x -> fromJust (M.lookup x m)) (succesors t n))) m
											else
												m
new_tag t g@(F f) m n@(OrNode s) = 	if S.member g s && fromJust (M.lookup n m) == pinf && S.some (\x -> fromJust (M.lookup x m) < pinf) (succesors t n) then
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






{-------------------------  DAGS  ---------------------------}

dag :: Tableaux -> Node -> Formula -> Tableaux
dag t n@(AndNode _) f@(U g h) = build_dag t (tagmap t f) $ init_dag n
dag t n@(AndNode _) f@(F g) = build_dag t (tagmap t f) $ init_dag n
dag t n@(OrNode _) f = error ("dag called with OrNode : " ++ (show n) ++ "and formula f : " ++ (show f)) 

init_dag = \n -> Tableaux n (S.singleton n) R.empty


--build_dagU :: Tableaux -> Map Node Int -> Tableaux -> Tableaux
--build_dagU t m dag  = 	if stop then 
--							dag 
--						else
--							build_dagAU t m $ S.fold (flip $ treat_dag_node t m) dag (frontier dag)
--
--	where stop = S.all (\x -> fromJust (M.lookup x m) == 0 && isAnd x) $ frontier dag


build_dag :: Tableaux -> Map Node Int -> Tableaux -> Tableaux
build_dag t m dag  = 	if stop then 
							dag 
						else
							build_dag t m $ treat_dag_node t m dag pick

	where 
		stop = S.some (\x -> fromJust (M.lookup x m) == 0 && isAnd x) $ frontier dag
		pick = fst . head $ sortBy (comparing snd) (filter (\p -> S.member (fst p) (frontier dag)) (M.toList m))


treat_dag_node :: Tableaux -> Map Node Int -> Tableaux -> Node -> Tableaux
treat_dag_node t@(Tableaux r ns rel) m dag@(Tableaux dr dns drel) n@(OrNode _) = let c = fst . head $ candidates in
																					Tableaux dr (dns S.<+ c) (R.insert n c drel)
	where 
		candidates = sortBy (comparing snd) (filter (\p -> S.member (fst p) (succesors t n)) (M.toList m))

treat_dag_node t@(Tableaux r ns rel) m dag@(Tableaux dr dns drel) n@(AndNode _) = Tableaux dr ns' rel'
																					where
																						succs = S.toList (succesors t n)
																						ns' = dns `S.union` S.fromList succs
																						rel' = drel `R.union` R.fromList [(n,succ) | succ <- succs]





tab_to_model :: Int -> Tableaux -> Model
tab_to_model k t@(Tableaux r ns rel) = Model.Model (trans r) (S.fromList mnodes) new_rel

	where
		new_rel = (R.map trans ((S.fromList) tnodes R.<| (rel R.* rel) R.|> (S.fromList tnodes))) 
		trans = \tn -> fromJust $ M.lookup tn tmmap
		tmmap = M.fromList $ zip tnodes mnodes 	
		mnodes = map (\p -> Model.Node (fst p) (formulas $ snd p)) (zip [k..] tnodes)
		tnodes = S.toList $ S.filter isAnd $ nodes t


{-------------------------  FRAG  ---------------------------}




frag :: Int -> Tableaux -> Node -> Model
frag k t n@(AndNode _) = 	if null eventualities then 
					tab_to_model k $ build_frag_noeven t n
				else	
					build_frag  (k+k') t (tail eventualities) initm 

	where 
		k' = Model.size initm
		initm = tab_to_model k $ (dag t n (head eventualities))
		eventualities = S.toList $ S.filter (\f -> isEventuality f) (formulas n)



build_frag_noeven :: Tableaux -> Node -> Tableaux
build_frag_noeven t n@(AndNode _) = result

	where
		result = Tableaux n new_nodes new_rel
		new_rel = R.fromList $ [(n,l1) | l1 <- S.toList $ level1] ++ [(l1,l2) | l1 <- S.toList $ level1, l2 <- S.toList $ level2, R.member l1 l2 (rel t)] --new_nodes R.<| (rel t) R.|> new_nodes
		new_nodes = level1 S.+ level2 S.<+ n	
		level2 = S.map (fromJust . S.pick) $ S.map (succesors t) level1
		level1 = succesors t n



build_frag :: Int -> Tableaux -> [Formula] -> Model -> Model
build_frag k t [] mres = mres
build_frag k t (f:fs) mres = build_frag (k+k') t fs mres3

	where 
		k' = Model.size dg
		mres3 = Model.paste mres2 mn dg (Model.root dg)
		dg = tab_to_model k $ (dag t c f)
		c = find_and t mn
		mn = fromJust $ S.pick $ Model.frontier mres2 ---((trace $ "mres2 --------------- \n\n " ++ show mres2)  mres2)
		mres2 = Model.identifyFrontier mres ---((trace $ "mres -------------------- \n\n " ++ show mres)  mres)


find_and :: Tableaux -> Model.Node -> Node
find_and t nm = fromJust $ S.pick $ S.filter (\n -> isAnd n && formulas n == Model.formulas nm) (nodes t)






{-------------------   MODEL  -----------------------}




model :: Tableaux -> Model
model t = build_model k fr (S.toList $ Model.frontier $ init_model) t init_model	

	where
		fr = S.singleton (choose_and, Model.root init_model)
		k = Model.size init_model
		init_model = frag 0 t choose_and
		choose_and = fromJust $ S.pick $ succesors t (root t) 


build_model :: Int -> Set (Node, Model.Node) -> [Model.Node] -> Tableaux -> Model -> Model
build_model k froots [] t mres = mres
build_model k froots (mn:mns) t mres = case da_one of
										(Just pair) -> build_model k froots (mns) t  new_model_just
										Nothing -> build_model (k'+1) (froots S.<+ (c,mn)) new_front t new_model_noth						

	where
		new_front = S.toList $  Model.frontier $ new_model_noth
		k' = k + Model.size da_frag
		new_model_noth = Model.paste mres mn da_frag (Model.root da_frag) 
		da_frag = frag (k+1) t c
		c = (find_and t mn)
		new_model_just = let p = (mn, snd $ fromJust $ da_one) in Model.identify mres p --- (trace ("p = " ++ (show p) ++ " " ++ (show $ fst p == snd p)) p)
		da_one = S.pick $ S.filter (\p -> fst p == find_and t mn {-&& notElem (snd p) (mn:mns)-}) froots 
		---trrr = trace $ "Current model ----------------------\n" ++ show mres




















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



