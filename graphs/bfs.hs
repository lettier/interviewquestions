{-
  David Lettier (C) 2016
  http://www.lettier.com

  Implement the Breadth First Search algorithm.
-}

data Vertex = Vertex {
                          vertexLabel :: [Char]
                        , vertexNeighbors :: [[Char]]
                        , vertexDistance :: Int
                        , vertexPredecessor :: [Char]
                      } deriving (Show)

-- We define a graph as a list of vertexes.
-- Each vertex is a label, an adjacency list (neighbors),
-- a distance away from the root, and a predecessor label.
data Graph = Graph [Vertex] deriving (Show)

-----------------------------------------------------------------------------

-- Takes in a vertex, a list of vertexes, and returns true or false.
vertexInVertexes :: Vertex -> [Vertex] -> Bool
-- Given an empty list of vertices, return false.
vertexInVertexes _ [] = False
-- Reduce the list of vertexes to a bool.
-- If at least one vertex label in the list matches the input vertex label, the result will be true.
vertexInVertexes Vertex {vertexLabel = label} (x:y) = foldl (\ acc x -> vertexLabel x == label || acc) False (x:y)

-----------------------------------------------------------------------------

-- Takes a graph, a list of strings, and outputs a list of vertexes.
graphVertexes :: Graph -> [[Char]]-> [Vertex]
-- Empty graph.
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:y)) [] = x : y
-- The vertex label is an element in the list of labels (keys).
graphVertexes (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)

-----------------------------------------------------------------------------

--     In       Out      Queue       Seen        Out
bfs :: Graph -> Graph -> [Vertex] -> [Vertex] -> Graph
bfs (Graph []) _ _ _ = Graph []
-- If the queue is empty, output the breadth first search tree (graph).
bfs _ outGraph [] _ = outGraph
bfs (Graph (a:b)) (Graph (c:d)) (e:f) (g:h) = bfs inGraph outGraph queue seen'
    where inGraph = Graph (a:b)
          -- Get the current vertex label.
          eLabel = vertexLabel e
          -- Get the list of labels that are the neighbors of the current vertex.
          eNeighbors = vertexNeighbors e
          -- Get the vertexes from the vertex neighbor labels.
          eVertexNeighbors = graphVertexes inGraph eNeighbors
          -- The current distance, for the current vertex neighbors, is one more then
          -- the distance the current vertex is at.
          dist = vertexDistance e + 1
          -- Seen is the vertexes that have been queued before.
          seen = g : h
          -- Remove all neighbors, to the current vertex, that have
          -- been queued up before.
          filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors
          -- Update the predecessor label and distance for each current vertex neighbor.
          enqueue = updateDistPred filteredNeighbors dist eLabel
          -- Update our breadth first search tree/graph.
          outGraph = Graph $ (c:d) ++ enqueue
          -- Add the neighbors to the queue.
          queue = f ++ enqueue
          -- Update seen with the enqueued vertexes.
          seen' = seen ++ enqueue

-----------------------------------------------------------------------------

filterVertexNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
-- If either `s` (seen) or `vn` (vertex neighbors) are empty, return an empty list.
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
-- Filter out all vertexes in `vn` that are also in `s`.
filterVertexNeighbors s vn = filter (\ x -> not $ vertexInVertexes x s) vn

-----------------------------------------------------------------------------

updateDistPred :: [Vertex] -> Int -> [Char] -> [Vertex]
updateDistPred [] _ _ = []
-- Go though each vertex and swap the current distance and predecessor labels with the new parameters.
updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _) -> Vertex label n dist predLabel) (x:y)

-----------------------------------------------------------------------------

-- An impure function that takes a graph and performs input/output (IO).
printGraph :: Graph -> IO ()
printGraph (Graph []) = putStrLn ""
printGraph (Graph (x:y)) = do
  -- Print the first vertex.
  print x
  -- Print the rest of the vertexes.
  printGraph (Graph y)
  return ()

-----------------------------------------------------------------------------

-- The main entry point for the program.
main :: IO ()
main = do
  -- Create a graph of nine vertexes.
  let inGraph = Graph [
                  Vertex "a" ["b", "c"          ] 0 ""
                , Vertex "b" ["a", "d", "e"     ] 0 ""
                , Vertex "c" ["a", "d"          ] 0 ""
                , Vertex "d" ["b", "c", "e"     ] 0 ""
                , Vertex "e" ["b", "d", "f", "g"] 0 ""
                , Vertex "f" ["e", "g", "h"     ] 0 ""
                , Vertex "g" ["e", "f", "i"     ] 0 ""
                , Vertex "h" ["f", "i"          ] 0 ""
                , Vertex "i" ["g", "h"          ] 0 ""
                ]
  -- Start the queue off with the starting vertex/root.
  let queue = graphVertexes inGraph ["e"]
  let outGraph = Graph queue
  let seen = queue
  printGraph $ bfs inGraph outGraph queue seen
  return ()

{-
  a --- b -   - f --- h
  -     -  - -  -     -
  -     -   e   -     -
  -     -  - -  -     -
  c --- d -   - g --- i

  /trees git:master ❯❯❯ runhaskell bfs.hs
  Vertex {vertexLabel = "e", vertexNeighbors = ["b","d","f","g"], vertexDistance = 0, vertexPredecessor = ""}
  Vertex {vertexLabel = "b", vertexNeighbors = ["a","d","e"], vertexDistance = 1, vertexPredecessor = "e"}
  Vertex {vertexLabel = "d", vertexNeighbors = ["b","c","e"], vertexDistance = 1, vertexPredecessor = "e"}
  Vertex {vertexLabel = "f", vertexNeighbors = ["e","g","h"], vertexDistance = 1, vertexPredecessor = "e"}
  Vertex {vertexLabel = "g", vertexNeighbors = ["e","f","i"], vertexDistance = 1, vertexPredecessor = "e"}
  Vertex {vertexLabel = "a", vertexNeighbors = ["b","c"], vertexDistance = 2, vertexPredecessor = "b"}
  Vertex {vertexLabel = "c", vertexNeighbors = ["a","d"], vertexDistance = 2, vertexPredecessor = "d"}
  Vertex {vertexLabel = "h", vertexNeighbors = ["f","i"], vertexDistance = 2, vertexPredecessor = "f"}
  Vertex {vertexLabel = "i", vertexNeighbors = ["g","h"], vertexDistance = 2, vertexPredecessor = "g"}

  a --- b -   - f --- h
           - -
            e
           - -
  c --- d -   - g --- i
-}
