grammar Amecos;

/** Lexer rules */
WS : [ \t\r\n]+ -> skip ;

OBJ: [A-Z_]+ ;
NUM: [0-9]+ ;
NAME : [a-zA-Z] [a-zA-Z0-9_]* ;

/** Parser rules */

app: consistency* init+ opex* order*;
consistency: 'check' NAME;
init: 'new' NAME OBJ;
args: NUM (',' NUM)*;
interval: '(' NUM ',' NUM ')';
opex: OBJ '.' NAME '(' args? ')' ('/' NUM)? interval;
order: NUM '->' NUM;

