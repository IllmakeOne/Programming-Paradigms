module TreeWalkerTests where

import Parser
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import TreeWalker
import Structure


------Checking different function declaration errors----------
fundecl_err1 = checkCorrectProgram "{ func bool fib (& int x) { return 2;};fib(x); }"
            == (False,[Err (Arg SimplyNull "") (Er "No params because didnt find method "),Err (Arg SimplyBol "fib") (Er "Return type of method not same type and method's type ")])
fundecl_err2 = checkCorrectProgram "{ func bool fib (& int x) { };fib(x); }"
            == (False,[Err (Arg SimplyNull "") (Er "No params because didnt find method "),Err (Arg SimplyBol "fib") (Er "Method has wrong return or no return ")])
fundecl_err3 = checkCorrectProgram "{ func bool fib (& int x) { return ya;};fib(x); }"
            == (False,[Err (Arg SimplyNull "") (Er "Non void methods called ")])
fundecl_err4 = checkCorrectProgram "{ global int jesse = 1000;func int add(int amount) { amount += 1; return amount;};jesse = add(3); print jesse;}"
            == (True,[])


----- tests symbol table building-------------
symbolTableBuilder_test1 = symbolTableBuilder_fromStg
      "{ global int z = 3 ;func int fib(bool x, & int y){ x =ya ;return y;}; int x = fib (ya, 2);func int fibi(int a, & int b){ a =2 ;return b;};}"
      == [DB (Arg SimplyInt "z") 0 0 False,DBF (Arg SimplyInt "fib") [ByVal (Arg SimplyBol "x"),ByRef (Arg SimplyInt "y")] (Block [Ass "x" (BoolConst True),Return (Identifier "y"),End]),DB (Arg SimplyBol "x") 2 1 True,DB (Arg SimplyInt "y") 2 2 True,DB (Arg SimplyInt "x") 0 1 False,DBF (Arg SimplyInt "fibi") [ByVal (Arg SimplyInt "a"),ByRef (Arg SimplyInt "b")] (Block [Ass "a" (Constant 2),Return (Identifier "b"),End]),DB (Arg SimplyInt "a") 2 1 True,DB (Arg SimplyInt "b") 2 2 True]
symbolTableBuilder_test2 = symbolTableBuilder_fromStg
      "{ global int a = 0 ;global bool b = ya; int c = 1; bool d; global bool e;func void aux(){ int b = 0;}; }"
      == [DB (Arg SimplyInt "a") 0 0 False,DB (Arg SimplyBol "b") 0 1 False,DB (Arg SimplyInt "c") 0 2 False,DB (Arg SimplyBol "d") 0 2 False,DB (Arg SimplyBol "e") 0 2 False,DBF (Arg SimplyNull "aux") [] (Block [VarDecl (Arg SimplyInt "b") (Constant 0),End]),DB (Arg SimplyInt "b") 2 1 False]
--this method was used to test if the parameters are correclty added to the function's scopes
symbolTableBuilder_test4 = symbolTableBuilder_fromStg
                  "{ int x; func int fib(& int x, int y, int z){ return y; };int z =2; }"
                == [DB (Arg SimplyInt "x") 0 0 False,DBF (Arg SimplyInt "fib") [ByRef (Arg SimplyInt "x"),ByVal (Arg SimplyInt "y"),ByVal (Arg SimplyInt "z")] (Block [Return (Identifier "y"),End]),DB (Arg SimplyInt "x") 2 1 True,DB (Arg SimplyInt "y") 2 2 True,DB (Arg SimplyInt "z") 2 3 True,DB (Arg SimplyInt "z") 0 0 False]
-- this method was used to test if it correctly rejects FunCall wiht values instead of indetifies for pram by ref
symbolTableBuilder_test5 = checkCorrectProgram
      "{ func int fib(& int x){ return x; }; int x = fib(2); }"
symbolTableBuilder_test6 = symbolTableBuilder_fromStg
                  "{ func int add(int amount) { amount += 1; return amount;};}"


---- various error tests---------
typeCheckProgram_test1 = runChecksfromString "{ int x = 0; bool x = ya; bool z = y+ya; print x;}"
        == [Er "Addition elemets are not the same type in VarDecl z"]
typeCheckProgram_test2 = runChecksfromString "{ int x = 0; int y = 2; int z = ya; print x;}"
        == [Er "VarDecl z wrong type assigned"]
typeCheckProgram_test3 = runChecksfromString "{ while(ya >= 2){ int x;};}"
        == [Er "Not same type in Gq Condtion in While"]

----------Tests which tests if the correct error arises-------------------
typeCheck_test_vadecl1 = runChecksfromString "{ int x = ya;}" == [Er "VarDecl x wrong type assigned"]
typeCheck_test_vadecl2 = runChecksfromString "{ bool x = 2*3;}" ==[Er "VarDecl x wrong type assigned"]
typeCheck_test_globalvadecl1 = runChecksfromString "{ global int x = ya;}"
            == [Er "GlobalVarDecl x wrong type assigned"]
typeCheck_test_globalvadecl2 = runChecksfromString "{ global bool x = 2+4*2;}"
            == [Er "GlobalVarDecl x wrong type assigned"]
typeCheck_test_funcall1 = runChecksfromString "{ func int fib(int x){ int y; return y;}; int x = fib(2);}"
            == []
typeCheck_test_funcall2 = runChecksfromString "{ func void fib(int x){ int y;}; fib(2,3);}"
             == [Er "Fewer parameters than arguments"]
typeCheck_test_funcall3 = runChecksfromString "{ func int fib(int x){ int y;return y; }; fib(2);}"
            == [Er "Funcall fib is not Void type"]
typeCheck_test_ass1 = runChecksfromString "{ int x; bool y; x = 2 + ya; }"
            == [Er "Addition elemets are not the same type in Assigment x"]
typeCheck_test_ass2 = runChecksfromString "{ int x; bool y; y = x;}"
            == [Er "Wrong type in Assigmnet y"]
typeCheck_test_incr = runChecksfromString "{ bool x;x++;}"
            == [Er "x is not int, cannot ++"]
typeCheck_test_decr = runChecksfromString "{ bool x;x--;}"
            == [Er "x is not int, cannot --"]
typeCheck_test_addcom1 = runChecksfromString "{ bool x; x+= 2+ya;}"
            == [Er "Addition elemets are not the same type in Add Command x"]
typeCheck_test_addcom2 = runChecksfromString "{ int x; x+= ya;}"
            == [Er "Type error Add Command x"]
typeCheck_test_mincom1 = runChecksfromString "{ bool x; x-= 2+ya;}"
            == [Er "Addition elemets are not the same type in Min Command x"]
typeCheck_test_mincom2 = runChecksfromString "{ int x; x-= ya;}"
            == [Er "Type error in Min Command x"]
typeCheck_test_while1 = runChecksfromString "{ while(ya >= 2){ int x;};}"
            == [Er "Not same type in Gq Condtion in While"]
typeCheck_test_ifcomand1 = runChecksfromString "{ if(ya >= 2){ int x;}{};}"
            == [Er "Not same type in Gq Condtion in IfCom"]
typeCheck_test_ifcomand2 = runChecksfromString "{ if(ya >= 2){ int x;}{ return 3;};}"
            == [Er "Second block of IfCom has return"]
typeCheck_test_ifcomand3 = runChecksfromString "{ if(ya >= 2){ return 3;}{ int x;};}"
            == [Er "First block of IfCom has return"]
typeCheck_test_ifexpr1 = runChecksfromString "{ int x = int ?(2==ya){2}{2} ;}"
            == [Er "Not same type in Eq Condtion in IfExpr in VarDecl x"]
typeCheck_test_ifexpr2 = runChecksfromString "{ int x = int ? (ya >= ya) {ya}{2};}"
            == [Er "Bools cannot be >= in IfExpr and first expr is wrong type in VarDecl x"]
typeCheck_test_report = runChecksfromString
        "{ int x = ya; x = ya*2; x = int ? (ya >= ya) {ya}{2}; func int fib(int x) { int y; return y;};  int z = fib(2,3);}"
