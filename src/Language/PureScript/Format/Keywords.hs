module Language.PureScript.Format.Keywords where

import Language.PureScript.Format.Pretty

whereKw :: Doc Ann
whereKw = keyword "where"

classKw :: Doc Ann
classKw = keyword "class"

importKw :: Doc Ann
importKw = keyword "import"

typeKw :: Doc Ann
typeKw = keyword "type"

ifKw :: Doc Ann
ifKw = keyword "if"

thenKw :: Doc Ann
thenKw = keyword "then"

elseKw :: Doc Ann
elseKw = keyword "else"

atKw :: Doc Ann
atKw = keyword "at"

asKw :: Doc Ann
asKw = keyword "as"

deriveKw :: Doc Ann
deriveKw = keyword "derive"

foreignKw :: Doc Ann
foreignKw = keyword "foreign"

dataKw :: Doc Ann
dataKw = keyword "data"

newtypeKw :: Doc Ann
newtypeKw = keyword "newtype"

moduleKw :: Doc Ann
moduleKw = keyword "module"

hidingKw :: Doc Ann
hidingKw = keyword "hiding"

instanceKw :: Doc Ann
instanceKw = keyword "instance"

letKw :: Doc Ann
letKw = keyword "let"

ofKw :: Doc Ann
ofKw = keyword "of"

doKw :: Doc Ann
doKw = keyword "do"

caseKw :: Doc Ann
caseKw = keyword "case"

forallKw :: Doc Ann
forallKw = keyword "forall"