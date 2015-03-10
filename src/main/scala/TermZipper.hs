module TermZipper where

data Term f v = Var v
              | Term f [Term f v]
