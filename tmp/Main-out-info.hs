{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Concurrent (0,Start,4,25,RequestedSpace) ((1,Start,4,57,RequestedNothing) threadDelay(1,End,4,57,RequestedNothing) )(0,End,4,25,RequestedSpace) 
import Control.Monad (2,Start,4,20,RequestedSpace) ((3,Start,4,52,RequestedNothing) forever(3,End,4,52,RequestedNothing) )(2,End,4,20,RequestedSpace) 
import Data.ByteString (4,Start,4,22,RequestedSpace) ((5,Start,4,54,RequestedNothing) ByteString(5,End,4,54,RequestedNothing) )(4,End,4,22,RequestedSpace) 
import Data.List.NonEmpty (6,Start,4,25,RequestedSpace) ((7,Start,4,57,RequestedNothing) (8,Start,4,89,RequestedNothing) NonEmpty (9,Start,8,129,RequestedSpace) ((10,Start,8,162,RequestedNothing) (11,Start,8,196,RequestedNothing) (:|)(11,End,8,196,RequestedNothing) (10,End,8,162,RequestedNothing) )(9,End,8,129,RequestedSpace) (8,End,4,89,RequestedNothing) (7,End,4,57,RequestedNothing) )(6,End,4,25,RequestedSpace) 
import Data.List.NonEmpty qualified as NE

main :: (12,Start,4,7,RequestedSpace) IO (13,Start,8,40,RequestedSpace) ()(13,End,8,40,RequestedSpace) (12,End,4,7,RequestedSpace) 
main = do
    forever $ do
        threadDelay 500_000

data NewUDevice = NewUDevice (14,Start,4,28,RequestedSpace) {(15,Start,4,61,RequestedNothing) (16,Start,4,94,RequestedNothing) name(16,End,4,94,RequestedNothing)  :: (17,Start,4,165,RequestedSpace) ByteString(17,End,4,165,RequestedSpace) (15,End,4,61,RequestedNothing) , (18,Start,4,270,RequestedSpace) (19,Start,4,303,RequestedNothing) uniq(19,End,4,303,RequestedNothing)  :: (20,Start,4,376,RequestedSpace) (21,Start,8,409,RequestedNothing) Maybe (22,Start,12,448,RequestedSpace) ()(22,End,12,448,RequestedSpace) (21,End,8,409,RequestedNothing) (20,End,4,376,RequestedSpace) (18,End,4,270,RequestedSpace) , (23,Start,4,608,RequestedSpace) (24,Start,4,641,RequestedNothing) keys(24,End,4,641,RequestedNothing)  :: (25,Start,4,714,RequestedSpace) (26,Start,8,747,RequestedNothing) [Int](26,End,8,747,RequestedNothing) (25,End,4,714,RequestedSpace) (23,End,4,608,RequestedSpace) , (27,Start,4,879,RequestedSpace) (28,Start,4,912,RequestedNothing) absAxes(28,End,4,912,RequestedNothing)  :: (29,Start,4,988,RequestedSpace) (30,Start,8,1021,RequestedNothing) [(31,Start,8,1057,RequestedNothing) ((32,Start,8,1093,RequestedNothing) Int(32,End,8,1093,RequestedNothing) , (33,Start,8,1165,RequestedSpace) NewUDevice(33,End,8,1165,RequestedSpace) )(31,End,8,1057,RequestedNothing) ](30,End,8,1021,RequestedNothing) (29,End,4,988,RequestedSpace) (27,End,4,879,RequestedSpace) }(14,End,4,28,RequestedSpace) 

(34,Start,0,0,RequestedNewline) -- only matches keys and axes (note DS4 also has FF events)(34,End,0,0,RequestedNewline) 
ps4opts :: ByteString -> NewUDevice
ps4opts (35,Start,0,7,RequestedSpace) (36,Start,0,38,RequestedNothing) name(36,End,0,38,RequestedNothing) (35,End,0,7,RequestedSpace)  =
    NewUDevice
        (37,Start,8,0,AfterNewline) { (38,Start,8,37,RequestedSpace) name = name(38,End,8,37,RequestedSpace) 
        , (39,Start,8,9,RequestedSpace) uniq = Nothing(39,End,8,9,RequestedSpace) 
        , (40,Start,8,9,RequestedSpace) keys =
            (41,Start,12,0,AfterNewline) [ (42,Start,12,42,RequestedSpace) 0(42,End,12,42,RequestedSpace) 
            , (43,Start,12,13,RequestedSpace) 1(43,End,12,13,RequestedSpace) 
            , (44,Start,12,13,RequestedSpace) 12(44,End,12,13,RequestedSpace) 
            ](41,End,12,0,AfterNewline) (40,End,8,9,RequestedSpace) 
        , (45,Start,8,9,RequestedSpace) absAxes =
            zip
                (46,Start,16,0,AfterNewline) [ (47,Start,16,46,RequestedSpace) 0(47,End,16,46,RequestedSpace) 
                , (48,Start,16,17,RequestedSpace) 1(48,End,16,17,RequestedSpace) 
                , (49,Start,16,17,RequestedSpace) 7(49,End,16,17,RequestedSpace) 
                ](46,End,16,0,AfterNewline) 
                $ repeat $
                    NewUDevice
                        (50,Start,24,0,AfterNewline) { (51,Start,24,54,RequestedSpace) name = ""(51,End,24,54,RequestedSpace) 
                        , (52,Start,24,25,RequestedSpace) uniq = Just ()(52,End,24,25,RequestedSpace) 
                        , (53,Start,24,25,RequestedSpace) keys = [](53,End,24,25,RequestedSpace) 
                        , (54,Start,24,25,RequestedSpace) absAxes = [](54,End,24,25,RequestedSpace) 
                        }(50,End,24,0,AfterNewline) (45,End,8,9,RequestedSpace) 
        }(37,End,8,0,AfterNewline) 

xx :: (55,Start,4,5,RequestedSpace) ((56,Start,4,37,RequestedNothing) (57,Start,4,70,RequestedNothing) Num a1(57,End,4,70,RequestedNothing) (56,End,4,37,RequestedNothing) , (58,Start,4,172,RequestedSpace) (59,Start,4,205,RequestedNothing) Num a2(59,End,4,205,RequestedNothing) (58,End,4,172,RequestedSpace) , (60,Start,4,308,RequestedSpace) (61,Start,4,341,RequestedNothing) Num b(61,End,4,341,RequestedNothing) (60,End,4,308,RequestedSpace) , (62,Start,4,443,RequestedSpace) (63,Start,4,476,RequestedNothing) Num c(63,End,4,476,RequestedNothing) (62,End,4,443,RequestedSpace) )(55,End,4,5,RequestedSpace)  => (64,Start,4,609,RequestedSpace) ((65,Start,4,643,RequestedNothing) (66,Start,4,677,RequestedNothing) ((67,Start,4,712,RequestedNothing) a1(67,End,4,712,RequestedNothing) , (68,Start,4,781,RequestedSpace) (69,Start,4,814,RequestedNothing) [a2](69,End,4,814,RequestedNothing) (68,End,4,781,RequestedSpace) )(66,End,4,677,RequestedNothing) (65,End,4,643,RequestedNothing) , (70,Start,4,980,RequestedSpace) b(70,End,4,980,RequestedSpace) , (71,Start,4,1045,RequestedSpace) c(71,End,4,1045,RequestedSpace) )(64,End,4,609,RequestedSpace) 
xx =
    (72,Start,4,0,AfterNewline) ( (73,Start,4,33,RequestedSpace) (74,Start,4,65,RequestedNothing) ( (75,Start,4,99,RequestedSpace) 1(75,End,4,99,RequestedSpace) 
    , (76,Start,4,5,RequestedSpace) (77,Start,4,36,RequestedNothing) [ (78,Start,4,70,RequestedSpace) 2(78,End,4,70,RequestedSpace) 
    , (79,Start,4,5,RequestedSpace) 2(79,End,4,5,RequestedSpace) 
    , (80,Start,4,5,RequestedSpace) 2(80,End,4,5,RequestedSpace) 
    ](77,End,4,36,RequestedNothing) (76,End,4,5,RequestedSpace) 
    )(74,End,4,65,RequestedNothing) (73,End,4,33,RequestedSpace) 
    , (81,Start,4,5,RequestedSpace) 3(81,End,4,5,RequestedSpace) 
    , (82,Start,4,5,RequestedSpace) (83,Start,4,36,RequestedNothing) ((84,Start,4,70,RequestedNothing) 4(84,End,4,70,RequestedNothing) )(83,End,4,36,RequestedNothing) (82,End,4,5,RequestedSpace) 
    )(72,End,4,0,AfterNewline) 

(85,Start,0,0,RequestedNewline) -- >>> min1 $ 1 :| [3,7,4](85,End,0,0,RequestedNewline) 
(86,Start,0,0,AfterNewline) -- 1(86,End,0,0,AfterNewline) 
min1 :: (87,Start,4,7,RequestedSpace) Ord a(87,End,4,7,RequestedSpace)  => (88,Start,4,74,RequestedSpace) NonEmpty a(88,End,4,74,RequestedSpace)  -> a
min1 (89,Start,0,4,RequestedSpace) (90,Start,0,35,RequestedNothing) (91,Start,0,68,RequestedNothing) ((92,Start,0,102,RequestedNothing) x :| xs(92,End,0,102,RequestedNothing) )(91,End,0,68,RequestedNothing) (90,End,0,35,RequestedNothing) (89,End,0,4,RequestedSpace)  = case NE.nonEmpty xs of
    (93,Start,4,0,AfterNewline) Just (94,Start,8,36,RequestedSpace) (95,Start,8,68,RequestedNothing) xs'(95,End,8,68,RequestedNothing) (94,End,8,36,RequestedSpace) (93,End,4,0,AfterNewline)  ->
        (96,Start,8,0,AfterNewline) let (97,Start,8,39,RequestedSpace) (98,Start,8,71,RequestedNothing) m = min1 xs'(98,End,8,71,RequestedNothing) (97,End,8,39,RequestedSpace) 
         in (99,Start,8,11,RequestedSpace) if x < m then x else m(99,End,8,11,RequestedSpace) (96,End,8,0,AfterNewline) 
    (100,Start,4,0,AfterNewline) Nothing(100,End,4,0,AfterNewline)  -> x

min1' :: (101,Start,4,8,RequestedSpace) Ord a(101,End,4,8,RequestedSpace)  => (102,Start,4,77,RequestedSpace) NonEmpty a(102,End,4,77,RequestedSpace)  -> a
min1' (103,Start,0,5,RequestedSpace) (104,Start,0,37,RequestedNothing) (105,Start,0,71,RequestedNothing) ((106,Start,0,106,RequestedNothing) x :| xs(106,End,0,106,RequestedNothing) )(105,End,0,71,RequestedNothing) (104,End,0,37,RequestedNothing) (103,End,0,5,RequestedSpace)  = case min2 xs of
    (107,Start,4,0,AfterNewline) Just (108,Start,8,37,RequestedSpace) (109,Start,8,70,RequestedNothing) m(109,End,8,70,RequestedNothing) (108,End,8,37,RequestedSpace) (107,End,4,0,AfterNewline)  -> if x < m then x else m
    (110,Start,4,0,AfterNewline) Nothing(110,End,4,0,AfterNewline)  -> x

(111,Start,0,0,RequestedNewline) -- >>> min2 [1,3,7,4](111,End,0,0,RequestedNewline) 
(112,Start,0,0,AfterNewline) -- Just 1(112,End,0,0,AfterNewline) 
min2 :: (113,Start,4,7,RequestedSpace) Ord a(113,End,4,7,RequestedSpace)  => (114,Start,4,76,RequestedSpace) [a](114,End,4,76,RequestedSpace)  -> (115,Start,4,145,RequestedSpace) Maybe a(115,End,4,145,RequestedSpace) 
min2 = \case
    (116,Start,4,0,AfterNewline) [](116,End,4,0,AfterNewline)  -> Nothing
    x : xs ->
        Just $ case min2 xs of
            (117,Start,12,0,AfterNewline) Nothing(117,End,12,0,AfterNewline)  -> x
            (118,Start,12,0,AfterNewline) Just (119,Start,16,46,RequestedSpace) (120,Start,16,80,RequestedNothing) m'(120,End,16,80,RequestedNothing) (119,End,16,46,RequestedSpace) (118,End,12,0,AfterNewline)  -> if x < m' then x else m'

min2' :: (121,Start,4,8,RequestedSpace) Ord a(121,End,4,8,RequestedSpace)  => (122,Start,4,77,RequestedSpace) [a](122,End,4,77,RequestedSpace)  -> (123,Start,4,146,RequestedSpace) Maybe a(123,End,4,146,RequestedSpace) 
min2' = fmap min1 . NE.nonEmpty

min2'' :: (124,Start,4,9,RequestedSpace) Ord a(124,End,4,9,RequestedSpace)  => (125,Start,4,78,RequestedSpace) [a](125,End,4,78,RequestedSpace)  -> (126,Start,4,147,RequestedSpace) Maybe a(126,End,4,147,RequestedSpace) 
min2'' = \case
    (127,Start,4,0,AfterNewline) [](127,End,4,0,AfterNewline)  -> Nothing
    x : xs -> Just $ min1 $ x :| xs

head1 :: (128,Start,4,8,RequestedSpace) NonEmpty a(128,End,4,8,RequestedSpace)  -> a
head1 (129,Start,0,5,RequestedSpace) (130,Start,0,37,RequestedNothing) (131,Start,0,71,RequestedNothing) ((132,Start,0,106,RequestedNothing) x :| xs(132,End,0,106,RequestedNothing) )(131,End,0,71,RequestedNothing) (130,End,0,37,RequestedNothing) (129,End,0,5,RequestedSpace)  = maybe x id $ head2 xs

head2 :: (133,Start,4,8,RequestedSpace) [a](133,End,4,8,RequestedSpace)  -> (134,Start,4,75,RequestedSpace) Maybe a(134,End,4,75,RequestedSpace) 
head2 = fmap NE.head . NE.nonEmpty

f1 :: (135,Start,4,5,RequestedSpace) ((136,Start,4,38,RequestedNothing) (137,Start,4,72,RequestedNothing) NonEmpty a(137,End,4,72,RequestedNothing)  -> b(136,End,4,38,RequestedNothing) )(135,End,4,5,RequestedSpace)  -> (138,Start,4,218,RequestedSpace) [a](138,End,4,218,RequestedSpace)  -> (139,Start,4,289,RequestedSpace) Maybe b(139,End,4,289,RequestedSpace) 
f1 (140,Start,0,2,RequestedSpace) (141,Start,0,34,RequestedNothing) f(141,End,0,34,RequestedNothing) (140,End,0,2,RequestedSpace)  = fmap f . Just . NE.fromList

(142,Start,0,0,RequestedNewline) -- f2 :: ([a] -> Maybe b) -> NonEmpty a -> b(142,End,0,0,RequestedNewline) 
(143,Start,0,0,AfterNewline) -- f2 g xs = _(143,End,0,0,AfterNewline) 

(144,Start,0,0,RequestedNewline) -- f2' :: ([a] -> Maybe a) -> NonEmpty a -> a(144,End,0,0,RequestedNewline) 
(145,Start,0,0,AfterNewline) -- f2' g (x :| xs) = maybe x id $ g xs(145,End,0,0,AfterNewline) 
