{-
 - Mod11PATest.hs -- CS 430 Spring 2018 Module 11 PA.
 - author: YOUR NAME HERE
 - 
 - You don't need to turn this code in, but you __should__ do it. 
 -}

import Mod11PA

-- TODO: Finish writing a test suite


testToCCW =
       toCCW ((0,0),(1,0),(1,1)) == ((0,0),(1,0),(1,1)) &&
       toCCW ((0,0),(1,1),(1,0)) == ((0,0),(1,0),(1,1))

testPairsFromPoints = 
    pairsFromPoints [(3,2), (1,2), (1,1)] == [((3,2), (1,2)), ((3,2), (1,1)), ((1,2), (1,1))] &&  pairsFromPoints [(3,2)] == [] && pairsFromPoints [] == []
testTriplesFromPoints = 
    triplesFromPoints [(3,2), (1,2), (1,1), (3,4)] ==  [((3,2), (1,2), (1,1)),((3,2), (3,4), (1,2)), ((3,2), (3,4), (1,1)), ((1,2), (1,1), (3,4))] && triplesFromPoints [(1,1), (3,4)] == [] && triplesFromPoints [] == []
testIsDelaunayTriangle =
    isDelaunayTriangle [(8,9), (2,2)] ((7,7), (9,9), (8,10)) == False && isDelaunayTriangle [(1,1), (2,2)] ((7,7), (9,9), (8,9)) == True &&  isDelaunayTriangle [] ((7,7), (9,9), (8,9)) == True
testDelaunayTriangulation = 
    delaunayTriangulation[(3,2), (1,2), (1,1), (3,4)] == [((3,2),(1,2),(1,1)),((3,2),(3,4),(1,2))] &&  delaunayTriangulation [] == []

test =
  testToCCW &&
  testPairsFromPoints &&
  testTriplesFromPoints &&
  testIsDelaunayTriangle &&
  testDelaunayTriangulation

-----------------------------------------------------------------
-- The stuff below is for compiling and running (by typing
-- runhaskell Mod11PATest.hs, for example) your code to run all tests.
-- In ghci, execute test or main.
main =
  do
    let resultAll = "all tests: " ++ (show test);
        resultToCCW = "toCCW: " ++ (show testToCCW);
        resultPairsFromPoints = "pairsFromPoints: " ++ (show testPairsFromPoints);
        resultTriplesFromPoints = "triplesFromPoints: " ++ (show testTriplesFromPoints);
        resultIsDelaunayTriangle = "isDelaunayTriangle: " ++ (show testIsDelaunayTriangle);
        resultDelaunayTriangulation = "delaunayTriangulation: " ++ (show testDelaunayTriangulation);
    putStrLn resultAll
    putStrLn resultToCCW
    putStrLn resultPairsFromPoints
    putStrLn resultTriplesFromPoints
    putStrLn resultIsDelaunayTriangle
    putStrLn resultDelaunayTriangulation
