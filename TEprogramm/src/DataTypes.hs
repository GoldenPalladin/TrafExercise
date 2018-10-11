

module DataTypes
  (
  Byte, toByte,
  IP, showIP, defineSubnet,
  Transmit, reverseTransmit, getNodeIP, getNodeUDP, getNodeSpeed, defineTransmitSubnet,
  TrRecordset, extendRecordset, getAverageSpeed, transformRecordset)
  where
    import Data.Monoid
    import Data.List
    import Data.String
    import Data.Char

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
        splitBy p s =  case dropWhile (==p) s of
                                "" -> []
                                s' -> w : splitBy p s''
                                      where (w, s'') = break (==p) s'

    showIP :: IP -> String
    -- преобразуем IP в строку
    showIP (IP a b c d) = show a ++"."++ show b ++ "." ++ show c ++ "." ++ show d

    -- определяем правила сравнения для IP адреса
    instance Eq IP where
      (==) (IP a b c d) (IP a' b' c' d') = (a == a')&&(b == b')&&(c == c')&&(d == d')

    compareIP :: IP -> IP -> Ordering
    compareIP (IP a b c d) (IP a' b' c' d') = (a `compare` a') `mappend` (b `compare` b') `mappend` (c `compare` c') `mappend` (d `compare` d')
    -- определяем правила сравнения для типа IP-адрес: последовательно с первого октета

    defineSubnet :: IP -> IP -> IP
    -- определяем подсеть по маске
    defineSubnet (IP a b c d) (IP a' b' c' d') = IP (bitConj a a') (bitConj b b') (bitConj c c') (bitConj d d')

    -- определяем новый тип: Вектор передачи
    data Transmit = Transmit {
                          trIP :: IP,
                          trMAC :: String,
                          rcIP :: IP,
                          rcMAC :: String,
                          isUDP :: Bool,
                          bytes :: Float,
                          time :: Float
                        }

    -- определяем правила равества для типа Вектор: вектора равны, если совпадает источник, приемник и протокол.
    instance Eq Transmit where
        (==) (Transmit trIP trMAC rcIP rcMAC isUDP bytes time) (Transmit trIP' trMAC' rcIP' rcMAC' isUDP' bytes' time') = (trIP  == trIP') && (rcIP == rcIP') && (isUDP == isUDP')

    compareNodeByIP :: Transmit -> Transmit -> Ordering
    -- упорядочение векторов по IP адресам передающего узла
    compareNodeByIP a b = compareIP (getNodeIP a) (getNodeIP b)

    reverseTransmit :: Transmit -> Transmit
    -- обращаем вектор передачи: меняем местами приемник и передатчик
    reverseTransmit x = Transmit {trIP = rcIP x, trMAC= rcMAC x, rcIP=trIP x, rcMAC=trMAC x, isUDP=isUDP x, bytes=bytes x, time=time x}

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
    defineTransmitSubnet ip x = Transmit {trIP = (defineSubnet (trIP x) ip), trMAC= trMAC x, rcIP=rcIP x, rcMAC=rcMAC x, isUDP=isUDP x, bytes=bytes x, time=time x}


    type TrRecordset = [Transmit]
    -- определяем множество векторов передачи

    extendRecordset :: TrRecordset -> TrRecordset
    -- дополняем список передач списком приема - обращенным списком передачи
    extendRecordset x = x ++ map reverseTransmit x

    transformRecordset :: TrRecordset -> [TrRecordset]
    -- преобразуем список передач в упорядоченный (по IP)список подсписков передач каждого узла
    transformRecordset x = groupBy (\ a b -> compareNodeByIP a b == EQ) $ sortBy (compareNodeByIP) x

{-
    defineTransmitSpeed :: Transmit -> Transmit
    -- обновляем вектор передачи расчетом скорости
    defineTransmitSpeed x = Transmit {x.trIP, x.trMAC, x.rcIP, x.rcMAC, x.isUDP, x.bytes, x.time, (x.bytes / x.time)}
    -- вынесем расчет скорости в инициализацию векторов в множестве -}

    getAverageSpeed :: TrRecordset -> Float
    -- получаем среднюю скорость по множеству векторов передачи: суммарный объем данных деленный на суммарное время передачи
    getAverageSpeed x = (foldl (\acc y -> acc + bytes y) 0 x ) / (foldl (\acc y -> acc + time y) 0 x )
