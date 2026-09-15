grammar Amecos;

/** Lexer rules */
WS : [ \t\r\n]+ -> skip;
COMMENT: '//' ~[\r\n]* -> skip;
OBJ: [A-Z_]+ ;
PROC: [a-z] [a-z0-9_]*;
NUM: [0-9]+ ;
NAME : [a-zA-Z] [a-zA-Z0-9_]* ;

/** Parser rules */

app: consistencies? init+ process* order* EOF;
consistencies: 'check' NAME (',' NAME)*;
init: 'new' NAME OBJ;
args: NUM (',' NUM)*;
interval: '(' NUM ',' NUM ')';
process: 'process' PROC ':' opex*;
opex: OBJ '.' NAME '(' args? ')' ('/' NUM)? interval;
opexRef: PROC '.' NUM;
order: opexRef '->' opexRef;
