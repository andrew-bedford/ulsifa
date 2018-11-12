%{ open Types %}

%token <string> IDENTIFIER
%token ASSIGN SKIP IF THEN ELSE END WHILE DO SEMICOLON RECEIVE FROM SEND TO
%token <int> INT
%token PLUS MINUS TIMES DIV EQ NEQ LT LTEQ GT GTEQ
%token EOF

%left EQ NEQ LT LTEQ GT GTEQ
%left PLUS MINUS
%left TIMES DIV

%start instructions
%type <Types.command> instructions

%%
instructions:
	| command SEMICOLON instructions 		 												{ Sequence($1, $3) }
	| command																										{ $1 }
;
expression:
	| IDENTIFIER                     														{ Identifier($1) }
	| INT                     																	{ Number($1) }
  | expression PLUS expression          											{ Operation($1, BinaryOperator(Plus), $3) }
  | expression MINUS expression         											{ Operation($1, BinaryOperator(Minus), $3) }
  | expression TIMES expression         											{ Operation($1, BinaryOperator(Multiplication), $3) }
  | expression DIV expression           											{ Operation($1, BinaryOperator(Division), $3) }
  | expression EQ expression           												{ Operation($1, BinaryOperator(Equal), $3) }
	| expression NEQ expression           											{ Operation($1, BinaryOperator(NotEqual), $3) }
	| expression LT expression           												{ Operation($1, BinaryOperator(LessThan), $3) }
	| expression LTEQ expression          											{ Operation($1, BinaryOperator(LessThanOrEqual), $3) }
	| expression GT expression           												{ Operation($1, BinaryOperator(GreaterThan), $3) }
	| expression GTEQ expression          											{ Operation($1, BinaryOperator(GreaterThanOrEqual), $3) }
;
command:
	| IDENTIFIER ASSIGN expression 															{ Assign(Identifier($1), $3) }
	| SKIP 																											{ Skip }
  | IF expression THEN instructions END 											{ IfThenElse($2, $4, Skip) }
	| IF expression THEN instructions ELSE instructions END 		{ IfThenElse($2, $4, $6) }
	| WHILE expression DO instructions END 											{ WhileDo($2, $4) }
	| RECEIVE IDENTIFIER FROM IDENTIFIER 												{ Receive(Identifier($2), Identifier($4)) }
	| SEND IDENTIFIER TO IDENTIFIER 														{ Send(Identifier($2), Identifier($4)) }
;