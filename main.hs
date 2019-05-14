import Data.List
import System.Environment

report n = "" ++
  "Antal grupper = " ++ (show n) ++ 
  (if (n `mod` 3 == 1) then "\n\nNOTERA: Detta antal rekommenderas inte. \nDet kommer bli en kollision, några grupper kommer mötas två gånger." ++
     "\nDu kanske kan omstrukturera grupperna så det blir " ++ (show $ n-1) ++ " eller " ++ (show $ n+1) ++ " istället?"
     else "") ++
  "\n\n\n" ++
  "Ge varje grupp en bokstav ('A' till " ++ (show $ letterIndex (n-1)) ++ ")\n\n" ++
  "\"X -> Y -> Z\" betyder att grupp X kommer få åka till grupp Y och grupp Z\n" ++
  "Förrätt serveras av: " ++ (intercalate ", " $ sort $ map letter $ filter ((==2).snd) members) ++ "\n" ++
  "Huvudrätt serveras av: " ++ (intercalate ", " $ sort $ map letter $ filter ((==1).snd) members) ++ "\n" ++
  "Efterrätt serveras av: " ++ (intercalate ", " $ sort $ map letter $ filter ((==0).snd) members) ++ "\n" ++
  description ++ "\n\n" ++
  "Antal kollisioner: " ++ (show $ div (numCollisions out) 2)
  where
    a = floor((fromIntegral n+2)/3) :: Int
    b = floor((fromIntegral n+1)/3)
    c = floor(fromIntegral n/3)

    yMax = 3
    xMax = a

    grid = [[0..a-1]
      ,[0..a-1]
      ,[0..a-1]]
    
    titled = zip [0..] grid

    coorded = map coordRow titled
    coordRow (y, xs) = zip xs (repeat y) 

    coords = concat coorded

    upClassic (x, y) = (x, y-1)
    downClassic (x, y) = (x+1, y+1)

    get target li = map snd $ filter matches li where
      matches (src, dst) = target == src
    
    getRev target li = map fst $ filter matches li where
      matches (src, dst) = target == dst
    
    goingTo src = (src :) . get src

    receiving dst = (dst :) . getRev dst
    
    
    mappings = concat $ map ruleExpand coords where
        ruleExpand c = [(c, applyUp c), (c, applyDown c)]
        applyUp = modulate . upClassic
        applyDown = modulate . downClassic
        modulate (x, y) = (mod x xMax, mod y yMax)
    
    withoutSource target = filter sourceNotS where
        sourceNotS (src, dst) = src /= target
    
    withoutDest target = filter dstNotS where
        dstNotS (src, dst) = dst /= target
    
    withoutDests targets = foldl (.) id $ map withoutDest targets
    withoutSources targets = foldl (.) id $ map withoutSource targets

    frb1 = (xMax-1, 0)
    frb2 = (xMax-1, 1)
    frb3 = (xMax-1, 2)
    frb = [frb1, frb2]

    leftFrb1 = (xMax-2, 0)
    leftFrb2 = (xMax-2, 1)
    leftFrb3 = (xMax-2, 2)


    rightFrb1 = (0, 0)
    rightFrb2 = (0, 1)


    regular = mappings

    plusOne = additionals ++ (withoutDests frb $ withoutSources frb mappings) where
        additionals = [(leftFrb1, rightFrb2)
                        ,(leftFrb3, rightFrb1)
                        ,(frb3, (1,1))]
                    
    plusTwo = additionals ++ (withoutDests frb $ withoutSource frb1 mappings) where
        additionals = [(frb2, (2, 0))
                        ,(frb3, frb2)
                        ,(leftFrb1, (0, 1))
                        ,(leftFrb3, (0, 0))]
    
    out = case (mod n 3) of 
      0 -> regular
      1 -> plusOne
      2 -> plusTwo
    
    numCollisions mp = sum $ map collHelper members where
        collHelper member = (length meetings) - (length $ nub meetings) where
            meetings = (getRev member mp) ++ (get member mp) ++ (filter (/= member) $ concat $ map (flip getRev mp) parties)
            parties = (get member mp)
        members = nub $ map fst mp
    --members = [(0,0)]
    
    members = nub $ map fst out
    
    description = intercalate "\n" $ sort $ map describe members where
        describe member = intercalate " -> " $ map letter (member:get member out) 
    

    letter = (:[]) . letterIndex . index
    letterIndex ind = case lookup ind (zip [0..] ['A'..]) of
      Just l -> l
    index (x,2) = x
    index (x,1) = xMax + x
    index (x,0) = if (mod n 3 /= 1) 
      then 2*xMax + x
      else 2*xMax + x - 1

main = do
  args <- getArgs
  putStrLn $ report (read $ head args)
  
