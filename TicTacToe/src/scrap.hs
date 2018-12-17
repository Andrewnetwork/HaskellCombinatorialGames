wiseMover :: p ->GameTree -> Move
wiseMover player tree@(Node parent children) = head$boardPositionDiff parent (getParent$snd$last$sortedCounts)
                                              where pm = possibilityMatrix tree
                                                    combinedCounts = combinationRule pm
                                                    sortedCounts = (sortBy (\(a,_)(b,_)->compare a b) combinedCounts)
