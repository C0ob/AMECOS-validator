grammar Amecos;

/** Lexer rules */
WS : [ \t\r\n]+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;
OBJ: [A-Z_]+ ;
PROC: [a-z] [a-z0-9_]*;
NUM: [0-9]+ ;
NAME : [a-zA-Z] [a-zA-Z0-9_]* ;

/** Parser rules */
app: typedef* consistencies? init+ process* order* EOF;

typedef: 'type' NAME (':' opdef+)?;
opdef: 'operation' NAME ':' signature predicate*;
predicate: ('V' | 'S' | 'L') '=' formula ';';
signature: iotype (',' iotype)* '->' iotype;
iotype: 'int' | 'void';



consistencies: 'check' NAME (',' NAME)*;
init: 'new' NAME OBJ;
args: NUM (',' NUM)*;
interval: '(' NUM ',' NUM ')';
process: 'process' PROC ':' opex*;
opex: OBJ '.' NAME '(' args? ')' ('/' NUM)? interval;
opexRef: PROC '.' NUM;
order: opexRef '->' opexRef;

/** Boolean predicate expressions. */
formula: disjunction;
disjunction: conjunction ('or' conjunction)*;
conjunction: unary ('and' unary)*;
unary: 'not' unary | formulaAtom;
formulaAtom: 'true'
           | 'false'
           | '(' formula ')'
           | valueExpr comparator valueExpr;

/** Values expose operation fields, for example `output` or `name`. */
valueExpr: valueAtom | 'count' '(' setExpr ')' | 'latest' '(' setExpr ')' | valueExpr mathOp valueExpr;
mathOp: '+' | '-' | '*' | '/';
valueAtom: NAME
         | NUM
         | 'output';
comparator: '==' | '!=' | '<' | '<=' | '>' | '>=';
setExpr: 'context' | 'future' | 'all' | 'input';
