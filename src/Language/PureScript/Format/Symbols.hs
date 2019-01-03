module Language.PureScript.Format.Symbols where

import Language.PureScript.Format.Pretty

atSym :: Doc Ann
atSym = operator "@"

underscoreSym :: Doc Ann
underscoreSym = punctuation "_"

pipeSym :: Doc Ann
pipeSym = punctuation "|"

backwardArrowSym :: Doc Ann
backwardArrowSym = operator "<-"

forwardArrowSym :: Doc Ann
forwardArrowSym = operator "->"

doubleForwardArrowSym :: Doc Ann
doubleForwardArrowSym = operator "=>"

questionmarkSym :: Doc Ann
questionmarkSym = operator "?"

backtickSym :: Doc Ann
backtickSym = punctuation "`"

dashSym :: Doc Ann
dashSym = operator "-"

leqSym :: Doc Ann
leqSym = operator "<="

doublecolonSym :: Doc Ann
doublecolonSym = operator "::"

colonSym :: Doc Ann
colonSym = operator ":"

backslashSym :: Doc Ann
backslashSym = operator "\\"

equalsSym :: Doc Ann
equalsSym = operator "="

dotSym :: Doc Ann
dotSym = operator "."