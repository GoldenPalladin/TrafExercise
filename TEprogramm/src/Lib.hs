module Lib
    ( trCase1
    ) where

import DataTypes
import Data.List
import Data.String

trCase1 :: TrRecordset -> String
-- определяем уникальный узел по IP адресу, т.к. MАС-адрес определяет не уникальный узел сети, а уникальное устройство, что, наверное, разные вещи
-- объединяем списки векторов приемки и векторов передачи, удаляем дубликаты, считаем длину списка
trCase1 x = wrapByText $ length $ nubBy (\a b -> getNodeIP a == getNodeIP b) (extendRecordset x)
  where
    wrapByText x = "В сети " ++ show(x) ++ " уникальных узлов."

trCase2 :: TrRecordset -> String
-- вычисляем среднюю скорость передачи по всему списку векторов передачи
trCase2 x = wrapByText $ getAverageSpeed x
  where
    wrapByText x = "Cредняя скорость передачи данных всей наблюдаемой сети: " ++ show(x) ++ " (байт/сек)."

trCase3 :: TrRecordset -> String
-- на упорядоченных подмножествах векторов передачи из одного источника, каждое подмножество отображаем на множество пар (скорость передачи, признак UDP),
-- из каждого множества берем пару с максимальной скоростью и формируем список таких пар, проверяем, что для каждой пары в списке признак UDP == true
trCase3 x
  | checkUDPOnly x = "Максимальная скорость передачи данных зарегистрирована только по UDP"
  | otherwise = "Максимальная пиковая скорость может также передаваться по TCP"
  where
    -- принимаем упорядоченное множество подмножеств, в каждом подмножестве преобразуем каждый вектор в пару (скорость, признак)
    reduceToSpeedSet x = map (\a -> map (\b -> (getNodeSpeed b, getNodeUDP b)) a) x
    -- из каждого подмножества отбираем пару с максимальной скоростью
    maximumSpeedList x = map (maximumBy (\(a,_) (b,_) -> compare a b)) x
    -- делаем свертку множества пар по признаку UDP == true
    foldTrue x = foldl (\ acc (_, a) -> acc && a) True x
    -- применяем все это
    checkUDPOnly x = foldTrue $ maximumSpeedList $ reduceToSpeedSet $ transformRecordset x

trCase4 :: TrRecordset -> String
-- на упорядоченных подмножествах векторов передачи из одного источника, каждое подмножество отображаем на пару (средняя скорость передачи, IP источника)
-- сортируем пары по убыванию скорости и берем первые 10
trCase4 x = wrapByText $ extractIP $ takeTenMax $ reduceToAverageSpeedSet $ transformRecordset x
  where
    wrapByText x = "10 узлов сети с самой высокой средней скоростью передачи данных (байт/сек): " ++ show(x)
    -- отображаем каждое подмножество на пару
    reduceToAverageSpeedSet x = map (\a -> (getNodeIP (head a), getAverageSpeed a)) x
    -- сортируем пары по убыванию по скорости и берем первые 10
    takeTenMax x = take 10 $ sortBy (\(_, a) (_, b) -> compare a b) x
    -- извлекаем IP адреса
    extractIP x = foldl (\ acc (x, _) -> acc ++ showIP x ++ ", " ) "" x

trCase5 :: TrRecordset -> String
-- множество векторов передач из узлов сети отображаем на множество векторов передач из подсетей
-- на упорядоченных подмножествах векторов передачи из одной подсети. каждое подмножество отображаем на пару (количесво сессий, IP подсети /24)
-- сортируем пары по убыванию количества сессий и берем первые 10
trCase5 x = wrapByText $ extractIP $ takeTenMax $ reduceToTransmissionCountSet $ transformRecordset $ transformToSubnet x
  where
    wrapByText x = "10 самых активных подсетей /24 (A.B.C.xxx) по критерию количества сессий передачи данных (штук): " ++ show(x)
    -- определяем IP подсети с маской /24
    subnetMask = IP (toByte 255) (toByte 255) (toByte 255) (toByte 0)
    -- подменяем IP узлов передачи на IP подсетей
    transformToSubnet x = map ( defineTransmitSubnet subnetMask ) x
    -- отображаем каждое подмножество на пару
    reduceToTransmissionCountSet x = map (\a -> (getNodeIP (head a), length a)) x
    -- сортируем пары по убыванию по количеству и берем первые 10
    takeTenMax x = take 10 $ sortBy (\(_, a) (_, b) -> compare a b) x
    -- извлекаем IP адреса
    extractIP x = foldl (\ acc (x, _) -> acc ++ showIP x ++ ", " ) "" x

trCase6 :: TrRecordset -> String
-- узел является ПРОКСИ, если он порождает минимум два несовпадающих отрезка приема-передачи (является для них общим)
-- отрезок приема-передачи двух узлов -- это пара взаимно обратных векторов передачи между этими узлами с одинаковым протоколом
-- все отрезки, соединяющие два узла по одному протоколу, равны
-- отрезок уникален, если на множестве он не имеет равных
-- таким образом, узел является прокси, если он входит во множество узлов уникальных отрезков более одного раза
trCase6 x =
  let
    -- убираем дубликаты векторов передачи
    uniqueTransmissionSet = nub x
    -- делаем пересечение множеств приема и передачи (получаем множество уникальных отрезков)
    uniqueTrRcSet = intersect uniqueTransmissionSet (map reverseTransmit uniqueTransmissionSet)
    -- отображаем его на уполрядоченное множество узлов
    sortedNodeSet = sort $ foldl (\x acc -> acc ++ [ getNodeIP x ] ++ [ getNodeRcIP x ] ) [ ] uniqueTrRcSet
    -- группируем в подмножества, удаляем те, которые содержат только одну точку, в оставшихся удаляем дубликаты
    proxySet = map (head) $ filter (\x -> length x == 1) $ group sortedNodeSet
    -- извлекаем IP адреса
    extractIP = foldl (\ acc (x, _) -> acc ++ showIP x ++ ", " ) "" proxySet
    wrapByText = "Список прокси-узлов: " ++ show(extractIP)
    in wrapByText
