## Higher order parser combinators for left-recursive grammars

Haskell parser combinator library that supports:
* Parsing of left-recursive grammars
* Monadic combinators
* Parse of any unambiguous context-free grammars in O(n^2) time complexity using memoization

The work is based on paper "[Memoization of Top-down Parsing](https://arxiv.org/pdf/cmp-lg/9504016)".

This code was written in 2024 as part of an undergraduate thesis. You can find the thesis itself [here](https://github.com/enotvtapke/cps-paper/blob/main/formal/ВКР_Ступников_А_С.pdf).
