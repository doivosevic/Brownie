
makeStatement :: Statement -> VarTable -> Maybe VarTable
makeStatement (If e st mst) vt
  | fromMaybe 0 (eval vt e) /= 0    = makeStatement st vt
  | isJust mst                      = makeStatement (fromJust mst) vt
  | otherwise                       = Just vt
makeStatement (Assignment v e) vt = insert v vt <$> eval vt e
    where insert k = flip (M.insert k)

run :: [Statement] -> Maybe VarTable
run = run' (Just M.empty)
    where run' = foldl (\ vt a -> join $ makeStatement a <$> vt)
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- join :: Monad m => m (m a) -> m a

interpret :: String -> Maybe VarTable
interpret s = case parse (many1 statement) "error" s of 
                Left  _ -> Nothing
                Right p -> run   p
                
eval :: VarTable -> Expression -> Maybe Int
eval vt e = case e of (Val   v)   -> Just v
                      (Var   v)   -> M.lookup v vt
                      (Plus  a b) -> (+) <$> eval vt a <*> eval vt b
                      (Minus a b) -> (-) <$> eval vt a <*> eval vt b
                      (Mult  a b) -> (*) <$> eval vt a <*> eval vt b
                      (Div   a b) -> div <$> eval vt a <*> eval vt b
            where liftValue f v1 v2 = Just $ toValue $ f (fromValue v1) (fromValue v2)

type VarTable = M.Map String Int
