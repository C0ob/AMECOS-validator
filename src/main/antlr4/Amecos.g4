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
PROC: [a-z] [a-z0-9_]*;
NUM: [0-9]+ ;
NAME : [A-Z] [a-zA-Z0-9_]* ;
STRING: '"' ( '\\' . | ~["\\] )* '"';



/** Parser rules */
app: typedef* consistencies? init+ process* order* EOF;

typedef: 'type' NAME (':' opdef+)?;
opdef: 'operation' NAME ':' signature predicate*;
predicate: ('V' | 'S' | 'L') '=' formula ';';
signature:  inputtype '->' outputtype;
inputtype: iotype (',' iotype)*;
outputtype: iotype;
iotype: 'int' | 'string' | 'void';

consistencies: 'check' NAME (',' NAME)*;
init: 'new' (OBJ NAME | NAME OBJ);
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
         
index: setExpr '['NUM']';
mathOp: '+' | '-' | '*' | '/';
valueAtom: NUM | '-' NUM | 'output' | 'name' | THIS | THAT | STRING | operationField;
operationField: (THIS | THAT) '.' ('output' | 'name');
comparator: '==' | '!=' | '<' | '<=' | '>' | '>=' | 'same object as' | 'same input as' | 'same output as';
setAtom: 'context' | 'future' | 'all' | 'input';
setExpr: setAtom | THIS '.' setAtom;
