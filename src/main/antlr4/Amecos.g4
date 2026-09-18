grammar Amecos;

/** Lexer rules */
NOT: 'not';
TRUE: 'true';
FALSE: 'false';
COUNT: 'count';
LATEST: 'latest';
EXISTS: 'exists';
FORALL: 'forall';

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
signature:  inputtype '->' outputtype;
inputtype: iotype (',' iotype)*;
outputtype: iotype;
iotype: 'int' | 'void';

consistencies: 'check' NAME (',' NAME)*;
init: 'new' (OBJ NAME | NAME OBJ);
args: NUM (',' NUM)*;
interval: '(' NUM ',' NUM ')';
process: 'process' PROC ':'? opex*;
opex: OBJ '.' NAME '(' args? ')' ('/' NUM)? interval?;
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

quantifier: (EXISTS | FORALL) 'in' setExpr ':' formula;

/** Values expose operation fields, for example `output` or `name`. */
valueExpr: valueAtom
         | COUNT '(' setExpr ')'
         | LATEST '(' setExpr ')'
         | valueExpr mathOp valueExpr;
mathOp: '+' | '-' | '*' | '/';
valueAtom: NUM | 'output';
comparator: '==' | '!=' | '<' | '<=' | '>' | '>=';
setExpr: 'context' | 'future' | 'all' | 'input';
