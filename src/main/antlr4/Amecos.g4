grammar Amecos;

/** Lexer rules */
NOT: 'not';
TRUE: 'true';
FALSE: 'false';
COUNT: 'count';
LATEST: 'latest';
EXISTS: 'exists';
FORALL: 'forall';
THIS: 'this';
THAT: 'that';

WS : [ \t\r\n]+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;
OBJ: [A-Z] [A-Z0-9_]*;
DATATYPE: 'int' | 'string' | 'void';
PROC: [a-z] [a-z0-9_]*;
NUM: [0-9]+ ;
STRING: '"' ( '\\' . | ~["\\] )* '"';
NAME : [A-Z] [a-zA-Z0-9_]* ;



/** Parser rules */
app: typedef* consistencies? init+ process* order* EOF;

typedef: 'type' NAME typePars? (':' opdef+)?;
opdef: 'operation' NAME ':' signature predicate*;
predicate: ('V' | 'S' | 'L') '=' formula ';';
typePars: '<' typeParam (',' typeParam)* '>';
typeParam: NAME | OBJ;
typeargs: '<' DATATYPE (',' DATATYPE)* '>';
signature:  inputtype '->' datatype;
inputtype: datatype (',' datatype)*;
datatype: NAME | OBJ | DATATYPE;

consistencies: 'check' NAME (',' NAME)*;
init: 'new' (NAME OBJ typeargs? | NAME typeargs OBJ);
args: argValue (',' argValue)*;
argValue: NUM | STRING;
interval: '(' NUM ',' NUM ')';
process: 'process' PROC ':'? opex*;
opex: OBJ '.' NAME '(' args? ')' ('/' argValue)? interval?;
opexRef: PROC '.' NUM;
order: opexRef '->' opexRef;

/** Boolean predicate expressions. */
formula: disjunction;
disjunction: conjunction ('or' conjunction)*;
conjunction: unary ('and' unary)*;
unary: NOT unary | formulaAtom;
formulaAtom: TRUE
           | FALSE
           | '(' formula ')'
           | quantifier
           | valueExpr comparator valueExpr;

quantifier: (EXISTS | FORALL) 'in' setExpr  where? ':' formula;
where: 'where' formula;

/** Values expose operation fields, for example `output` or `name`. */
valueExpr: valueAtom
         | COUNT '(' setExpr ')'
         | index
         | valueExpr mathOp valueExpr;
         
index: tupleExpr '['NUM']';
tupleExpr: tupleAtom;
tupleAtom: 'input' | THIS '.' 'input' | THAT '.' 'input';
mathOp: '+' | '-' | '*' | '/';
valueAtom: NUM | '-' NUM | 'output' | 'name' | THIS | THAT | STRING | operationField;
operationField: (THIS | THAT) '.' ('output' | 'name');
comparator: '==' | '!=' | '<' | '<=' | '>' | '>=' | 'same object as' | 'same input as' | 'same output as';
setAtom: 'context' | 'future' | 'all';
setExpr: setExpr setOperator setExpr | setAtom | THIS '.' setAtom | THAT '.' setAtom;
setOperator: 'union' | 'intersect' | 'difference';
