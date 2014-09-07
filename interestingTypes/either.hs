{-
Something absolutely beautiful about the either type. So some thoughts. Its a sum type. And paired with the product type it represents something awesome. Firstly isomorphic to (+) (*), then to sequence and if statements in control flow, and also in regular expressions - concatentation and alternation.
Also denotational semantics seem to be the dual of operational semantics or is that a stretch. What I mean to say is that, when considering properties of regular expressions concatenation behaves like the product and alternation behaves like the sum. However when considering the operational semantics its the reverse. And since they are the duals of each other, my conclusion.

Also lensing into a stream of eithers forms a geometric series, what is the relationship to the geometric series on natural numbers.
-}


data Tree b = Branch b [Tree b]

{-
list(a) = 1 + a*list(a)
list(a) = 1/(1 - a)
tree(a) = a*list(tree(a))
tree(a) = a*(1/(1-tree(a)))
tree(a)(1 - tree(a)) = a
tree(a) - tree^2(a) = a
tree(a) = a + tree^2(a)
-}

data TreeF a n = Leaf a | BranchF n n

-- degrees of freedom much like in equations. All the theory applicable on numbers should
-- then apply such as integration, numerical analysis and all other such nonsense.
