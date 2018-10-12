

module DataTypes
  (
  Byte, toByte, splitBy,
  IP (..), toIP, defineSubnet,
  Transmit (..), makeTransmit, reverseTransmit, getNodeIP, getNodeRcIP, getNodeUDP, getNodeSpeed, defineTransmitSubnet,
  TrRecordset (..), extendRecordset, getAverageSpeed, transformRecordset)
  where
    import Data.List (map, foldl, maximumBy, take, head, length, sort, sortBy, union, filter, group, groupBy, intersect, nub)
    import Data.String
    import Data.Char
    import Data.Maybe

    data Byte = Byte Int deriving (Eq, Ord, Read, Show)
    -- вводим новый тип: Byte = [0, 255]
    -- можно, конечно, использовать вшитые библиотеки работы с битовыми строками или определить байты и биты в двоичном виде -- но мне лень.
    toByte :: Int -> Byte
    toByte x  | x < 0 = error "Negative value!"
              | x > 255 = error "Greater than 255!"
              | otherwise = Byte x

    fromByte :: Byte -> Int
    fromByte (Byte i) = i

    bitConj :: Byte -> Byte -> Byte
    -- операция поразрядной конъюнкции для битов
    bitConj x y = toByte $ bin2dec $ zipWith (*) (dec2bin (fromByte x)) (dec2bin (fromByte y))
      where
        dec2bin n =
           if n < 2 then [n]
           else (dec2bin (div n 2)) ++ [(mod n 2)]
        bin2dec = foldl1 ((+) . (* 2))

    splitBy :: Char -> String -> [String]
    -- разбиение на подстроки по разделителю
    splitBy p s =  case dropWhile (==p) s of
                            "" -> []
                            s' -> w : splitBy p s''
                                  where (w, s'') = break (==p) s'

    data IP = IP Byte Byte Byte Byte
    -- вводим новый тип: IP

    toIP :: String -> Maybe IP
    -- разбор строки и формирование адреса IP
    toIP x
      | length octList == 4 = Just $ IP (octList !! 0) (octList !! 1) (octList!!2) (octList !! 3)
      | otherwise = Nothing
      where
        octList = take 4 $ map digitsToNumbers $ filter (\x -> x /= []) $ splitBy '.' $ head $ splitBy ':' $ filter (\a -> isDigit a || a == '.' || a ==':') x
        digitsToNumbers xs = toByte $ foldl(\acc x -> acc*10 + digitToInt x) 0 xs -- replace list of digits with appropriate number, like ['1', '3'] -> 13

    instance Show IP where
    -- преобразуем IP в строку
      show (IP a b c d) = show (fromByte a) ++"."++ show (fromByte b) ++ "." ++ show (fromByte c) ++ "." ++ show (fromByte d)

    -- определяем правила сравнения для IP адреса
    instance Eq IP where
      (==) (IP a b c d) (IP a' b' c' d') = (a == a')&&(b == b')&&(c == c')&&(d == d')

    --compareIP :: IP -> IP -> Ordering
    instance Ord IP where
      compare (IP a b c d) (IP a' b' c' d') = (a `compare` a') `mappend` (b `compare` b') `mappend` (c `compare` c') `mappend` (d `compare` d')
    -- определяем правила сравнения для типа IP-адрес: последовательно с первого октета

    defineSubnet :: IP -> IP -> IP
    -- определяем подсеть по маске
    defineSubnet (IP a b c d) (IP a' b' c' d') = IP (bitConj a a') (bitConj b b') (bitConj c c') (bitConj d d')

    -- определяем новый тип: Вектор передачи
    data Transmit = Transmit {
                          trIP :: IP,
                          -- trMAC :: String,
                          rcIP :: IP,
                          -- rcMAC :: String,
                          isUDP :: Bool,
                          bytes :: Float,
                          time :: Float
                        }

    -- определяем правила равества для типа Вектор: вектора равны, если совпадает источник, приемник и протокол.
    instance Eq Transmit where
        (==) (Transmit trIP rcIP isUDP bytes time) (Transmit trIP' rcIP' isUDP' bytes' time') = (trIP  == trIP') && (rcIP == rcIP') && (isUDP == isUDP')

    instance Ord Transmit where
    -- определяем сравнение векторов -- вектора сравниваются по IP источника
      (compare) (Transmit trIP rcIP isUDP bytes time) (Transmit trIP' rcIP' isUDP' bytes' time') = (trIP `compare` trIP') -- `mappend` (rcIP `compare` rcIP')

    -- создаем конструктор вектора
    makeTransmit :: Maybe IP -> Maybe IP -> Bool -> Float -> Float -> Maybe Transmit
    makeTransmit trIP rcIP isUDP bytes time
      | trIP == Nothing = Nothing
      | rcIP == Nothing = Nothing
      | bytes < 0 = Nothing
      | time < 0 = Nothing
      | otherwise = Just $ Transmit (fromJust trIP) (fromJust rcIP) isUDP bytes time

    compareNodeByIP :: Transmit -> Transmit -> Ordering
    -- упорядочение векторов по IP адресам передающего узла
    compareNodeByIP a b = compare (getNodeIP a) (getNodeIP b)

    reverseTransmit :: Transmit -> Transmit
    -- обращаем вектор передачи: меняем местами приемник и передатчик
    reverseTransmit x = Transmit {trIP = rcIP x, rcIP=trIP x, isUDP=isUDP x, bytes=bytes x, time=time x}

    getNodeIP :: Transmit -> IP
    -- получаем адрес передающего узла
    getNodeIP x = trIP x

    getNodeRcIP :: Transmit -> IP
    -- получаем адрес принимающего узла
    getNodeRcIP x = rcIP x

    getNodeUDP :: Transmit -> Bool
    -- получаем протокол вектора передачи
    getNodeUDP x = isUDP x

    getNodeSpeed :: Transmit -> Float
    -- получаем скорость вектора передачи
    getNodeSpeed x = bytes x / (time x)

    defineTransmitSubnet :: IP -> Transmit -> Transmit
    -- подменяем источник в векторе передачи на подсеть по маске
    defineTransmitSubnet ip x = Transmit {trIP = (defineSubnet (trIP x) ip), rcIP=rcIP x, isUDP=isUDP x, bytes=bytes x, time=time x}


    type TrRecordset = [Transmit]
    -- определяем множество векторов передачи

    {-addTransmit :: TrRecordset -> Maybe Transmit -> TrRecordset
    -- добавляем вектор передачи в список
    addTransmit x y
      | y == Nothing = x
      | otherwise = (fromJust y) : x -}

    extendRecordset :: TrRecordset -> TrRecordset
    -- дополняем список передач списком приема - обращенным списком передачи
    extendRecordset x = x ++ map reverseTransmit x

    transformRecordset :: TrRecordset -> [TrRecordset]
    -- преобразуем список передач в упорядоченный (по IP)список подсписков передач каждого узла
    transformRecordset x = groupBy (\ a b -> compareNodeByIP a b == EQ) $ sortBy (compareNodeByIP) x

    getAverageSpeed :: TrRecordset -> Float
    -- получаем среднюю скорость по множеству векторов передачи: суммарный объем данных деленный на суммарное время передачи
    getAverageSpeed x = (foldl (\acc y -> acc + bytes y) 0 x ) / (foldl (\acc y -> acc + time y) 0 x )
