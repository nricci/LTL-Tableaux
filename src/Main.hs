#!/opt/local/bin/runhugs

import System.Environment
import LTL
import Parser
import Tableaux

import qualified Data.Set as S
import Data.Set (Set)
import qualified SetAux as S

import Debug.Trace

import qualified Model as Model
import Model (Model)

import Data.Time.Clock 


main = do {
	args <- getArgs;
	run_tableaux $ head args
}


run_tableaux = \path -> do {
	start_time <- getCurrentTime;
	str <- readFile path;
	spec <- return $ S.fromList $ parseSpecification str;
	putStrLn ("Specification Successfully Parsed (" ++ (show (S.size spec)) ++ " formulas).");
	putStr ("Tableaux .. ");
	t <- return $ do_tableaux $ make_tableaux spec;
	putStrLn ("done (" ++ (show $ S.size $ nodes t ) ++ " nodes).");
	writeFile "output/tableaux_raw.dot" (tab2dot t);
	putStr ("Refining tableaux .. ");
	t2 <- return $ refine_tableaux t;
	putStrLn ("done (" ++ (show $ S.size $ nodes t2 ) ++ " nodes).");
	if S.member (root t2) (nodes t2) then
		do 	writeFile "output/tableaux.dot" (tab2dot t2)
			putStrLn ("Extracting model.")
			writeFile "output/model.dot" (Model.model2dot $ Model.flatten $ model t2)
	else
		putStrLn ("The specification is inconsistent.");
	end_time <- getCurrentTime;	
	putStrLn $ "time elapsed: " ++ show (diffUTCTime end_time start_time) 
}








