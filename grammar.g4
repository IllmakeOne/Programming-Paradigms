grammar BigProject;



block : '{' (command ';')* '}' ;

command: declr
       | functDecl
       | IDENTIFIER '=' expr
       | IDENTIFIER ('--' | '++' )
       | IDENTIFIER ( '+=' | '-=' ) expr
       | while
       | ifcom
       | FORK block
       | JOINT
       | PRINT expr
       | RETURN expr
       | fucntionCall
       ;

declr: GLOBAL? Type IDENTIFIER ('=' expr)?;

functDecl : Func Ftype LPar ('&'? Type IDENTIFIER)* RPar block ;

while : WHILE condition block;

ifcom : 'if' condition block block;

expr: term Add expr
    | term Div expr
    | term ;

term : factor Mult term
     | factor;

factor : NUBER | IDENTIFIER | BOOLVAL
       | fucntionCall | LPar expr RPar |
       | Type '?' condition '{' expr '}' '{' expr '}';

fucntionCall : IDENTIFIER LPar expr* RPar;

condition: LPar expr CmpSign expr RPar;


IDENTIFIER : LETTER+ ;
RETURN : 'return';
GLOBAL : 'global' ;
NUBER: DIGIT+ ;
JOIN : 'join' ;
FORK : 'fork' ;
PRINT : 'print' ;
WHILE : 'while' ;
BOOLVAL : 'ya' | 'nu';
fragment DIGIT: [0-9];
fragment LETTER: [a-zA-Z];
fragment Type : 'int' | 'bool' ;
fragment Ftype: Type | 'void' ;
fragment Func : 'func' ;
fragment CmpSign: '==' | '<' | '>' | '>=' | '<=';
fragment Mult: '*' ;
fragment Add: '+' ;
fragment Div: '-' ;
fragment LPar : '(' ;
fragment RPar : ')' ;
fragment SemiCol : ';' ;
fragment IfExpr: '?' ;