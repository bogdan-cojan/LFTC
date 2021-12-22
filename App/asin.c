#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

//Analizator Lexical

typedef enum {
	ID, INT, REAL, STR, VAR, FUNCTION, IF, ELSE, WHILE, END, RETURN,
	TYPE_INT, TYPE_REAL, TYPE_STR, COLON, SEMICOLON, LPAR, RPAR,
	COMMA, OR, AND, NOT, EQUAL, NOTEQUAL, LESS, ASSIGN, ADD, SUB,
	MUL, DIV, FINISH
}tipAtom;

char* wordDex[] = { "ID", "INT", "REAL", "STR", "VAR", "FUNCTION", "IF", "ELSE", "WHILE", "END", "RETURN",
	"TYPE_INT", "TYPE_REAL", "TYPE_STR", "COLON", "SEMICOLON", "LPAR", "RPAR",
	"COMMA", "OR", "AND", "NOT", "EQUAL", "NOTEQUAL", "LESS", "ASSIGN", "ADD", "SUB",
	"MUL", "DIV", "FINISH" };

typedef struct {
	int cod;
	int linie;
	union {
		char s[100];
		double vr;
		int vi;
	};
}Atom;

Atom atomi[10000];
int nAtomi = 0;

char bufin[30001];
char* pch;
int linie = 1;

void addAtom(int codAtom) {
	atomi[nAtomi].cod = codAtom;
	atomi[nAtomi].linie = linie;
	nAtomi++;
}

int getNextTk() {
	int state = 0;
	char buf[100];
	int n = 0;
	for (;;) {
		char ch = *pch;
		switch (state)
		{
		case 0:
			if (isalpha(ch) || ch == '_') { state = 1; pch++; buf[n++] = ch; }
			else if (isdigit(ch)) { state = 3; pch++; buf[n++] = ch; }
			else if (ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t') { pch++;  if (ch == '\n') { linie++; } }
			else if (ch == '\"') { state = 8; pch++; }
			else if (ch == ':') { state = 10; pch++; }
			else if (ch == ';') { state = 11; pch++; }
			else if (ch == '(') { state = 12; pch++; }
			else if (ch == ')') { state = 13; pch++; }
			else if (ch == ',') { state = 14; pch++; }
			else if (ch == '|') { state = 15; pch++; }
			else if (ch == '&') { state = 17; pch++; }
			else if (ch == '!') { state = 19; pch++; }
			else if (ch == '=') { state = 22; pch++; }
			else if (ch == '<') { state = 25; pch++; }
			else if (ch == '+') { state = 26; pch++; }
			else if (ch == '-') { state = 27; pch++; }
			else if (ch == '*') { state = 28; pch++; }
			else if (ch == '/') { state = 29; pch++; }
			else if (ch == '\0') { state = 30; pch++; }
			else if (ch == '#') { state = 31; pch++; }
			break;
		case 1:
			if (isalnum(ch) || ch == '_') { pch++; buf[n++] = ch; }
			else { state = 2; }
			break;
		case 2:
			buf[n] = '\0';
			if (strcmp(buf, "var") == 0) { addAtom(VAR); return(VAR); }
			else if (strcmp(buf, "function") == 0) { addAtom(FUNCTION); return(FUNCTION); }
			else if (strcmp(buf, "if") == 0) { addAtom(IF); return(IF); }
			else if (strcmp(buf, "else") == 0) { addAtom(ELSE); return(ELSE); }
			else if (strcmp(buf, "while") == 0) { addAtom(WHILE); return(WHILE); }
			else if (strcmp(buf, "end") == 0) { addAtom(END); return(END); }
			else if (strcmp(buf, "return") == 0) { addAtom(RETURN); return(RETURN); }
			else if (strcmp(buf, "int") == 0) { addAtom(TYPE_INT); return(TYPE_INT); }
			else if (strcmp(buf, "real") == 0) { addAtom(TYPE_REAL); return(TYPE_REAL); }
			else if (strcmp(buf, "str") == 0) { addAtom(TYPE_STR); return(TYPE_STR); }
			else {
				addAtom(ID);
				strcpy(atomi[nAtomi - 1].s, buf);
				return ID;
			}
		case 3:
			if (isdigit(ch)) { pch++; buf[n++] = ch; }
			else if (ch == '.') { state = 5; pch++; buf[n++] = ch; }
			else { state = 4; }
			break;
		case 4:
			buf[n] = '\0';
			addAtom(INT);
			atomi[nAtomi - 1].vi = atoi(buf);
			return INT;
		case 5:
			if (isdigit(ch)) { state = 6; pch++; buf[n++] = ch; }
			else { printf("linia %d --> Eroare! Trebuie cifre zecimale.\n", linie); exit(1); }
			break;
		case 6:
			if (isdigit(ch)) { pch++; buf[n++] = ch; }
			else { state = 7; }
			break;
		case 7:
			buf[n] = '\0';
			addAtom(REAL);
			atomi[nAtomi - 1].vr = atof(buf);
			return REAL;
		case 8:
			if (ch == '\"') { state = 9; pch++; }
			else if(ch == ';' || ch == ')') { printf("linia %d --> Eroare! Trebuie inchis string-ul cu \".\n", linie); exit(1); }
			else { pch++; buf[n++] = ch; }
			break;
		case 9:
			buf[n] = '\0';
			addAtom(STR);
			strcpy(atomi[nAtomi - 1].s, buf);
			return STR;
		case 10:
			addAtom(COLON);
			return COLON;
		case 11:
			addAtom(SEMICOLON);
			return SEMICOLON;
		case 12:
			addAtom(LPAR);
			return LPAR;
		case 13:
			addAtom(RPAR);
			return RPAR;
		case 14:
			addAtom(COMMA);
			return COMMA;
		case 15:
			if (ch == '|') { state = 16; pch++; }
			else  exit(1);
			break;
		case 16:
			addAtom(OR);
			return OR;
		case 17:
			if (ch == '&') { state = 18; pch++; }
			else  exit(1);
			break;
		case 18:
			addAtom(AND);
			return AND;
		case 19:
			if (ch == '=') { state = 21; pch++; }
			else { state = 20; }
			break;
		case 20:
			addAtom(NOT);
			return NOT;
		case 21:
			addAtom(NOTEQUAL);
			return NOTEQUAL;
		case 22:
			if (ch == '=') { state = 24; pch++; }
			else { state = 23; }
			break;
		case 23:
			addAtom(ASSIGN);
			return ASSIGN;
		case 24:
			addAtom(EQUAL);
			return EQUAL;
		case 25:
			addAtom(LESS);
			return LESS;
		case 26:
			addAtom(ADD);
			return ADD;
		case 27:
			addAtom(SUB);
			return SUB;
		case 28:
			addAtom(MUL);
			return MUL;
		case 29:
			addAtom(DIV);
			return DIV;
		case 30:
			addAtom(FINISH);
			return FINISH;
		case 31:
			if (ch == '\r' || ch == '\n' || ch == '\0') { state = 0; }
			else { pch++; }
			break;
		default: printf("linia %d -> stare invalida %d\n", linie, state);
		}
	}
}

void printAtomi() {
	int line = 0;

	if (line < atomi[0].linie) {
		line = atomi[0].linie;
		printf("linia %d: ", line);
		for (int i = 0; i < nAtomi; i++) {
			if (line < atomi[i].linie)
			{
				line = atomi[i].linie;
				printf("\nlinia %d: ", line);
			}
			if (wordDex[atomi[i].cod] == "ID") {
				printf(" %s:%s ", wordDex[atomi[i].cod], atomi[i].s);
			}
			else if (wordDex[atomi[i].cod] == "INT") {
				printf(" %s:%d ", wordDex[atomi[i].cod], atomi[i].vi);
			}
			else if (wordDex[atomi[i].cod] == "REAL") {
				printf(" %s:%f ", wordDex[atomi[i].cod], atomi[i].vr);
			}
			else if (wordDex[atomi[i].cod] == "STR") {
				printf(" %s:%s ", wordDex[atomi[i].cod], atomi[i].s);
			}
			else {
				printf(" %s ", wordDex[atomi[i].cod]);
			}
		}
	}
}

//Analizator Sintactic

int idxCrtAtom = 0;

int consume(int cod) {
	if (atomi[idxCrtAtom].cod == cod) {
		idxCrtAtom++;
		return 1;
	}
	return 0;
}

void errPrint(const char* msg) {
	printf("\n\neroare la linia %d: %s", atomi[idxCrtAtom].linie-1, msg);
	exit(1);
}

//functiile pt. sintaxa
int program();
int defVar();
int baseType();
int defFunc();
int block();
int funcParams();
int funcParam();
int instr();
int expr();
int exprLogic();
int exprAssign();
int exprComp();
int exprAdd();
int exprMul();
int exprPrefix();
int factor();

// factor ::= INT | REAL | STR | LPAR expr RPAR | ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
int factor()
{
	int startIdx=idxCrtAtom;
	if(consume(INT) || consume(REAL) || consume(STR))
	{
		return 1;
	}
	// LPAR expr RPAR
	if(consume(LPAR))
	{
		if(expr())
		{
			if(consume(RPAR))
			{
				return 1;
			}else errPrint("lipseste ) dupa expresie\n");
		}else errPrint("expresie gresita dupa (\n");
	}
	// ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
	if(consume(ID))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				for (;;)
				{
					if (consume(COMMA))
					{
						if (expr())
						{
							;
						}
						else
						{
							errPrint("lipseste expresie dupa ,\n");
							idxCrtAtom=startIdx;
							return 0;
						}
					}
					else break;
				}
			}
			if (consume(RPAR))
			{
				return 1;
			}
			else
			{
				errPrint("lipseste )\n");
				idxCrtAtom=startIdx;
				return 0;
			}
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// baseType ::= TYPE_INT | TYPE_REAL | TYPE_STR
int baseType(){
	int startIdx=idxCrtAtom;
	if(consume(TYPE_INT) || consume(TYPE_REAL) || consume(TYPE_STR))
	{
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// defFunc ::= FUNCTION ID LPAR funcParams RPAR COLON baseType defVar* block END
int defFunc()
{
	int startIdx=idxCrtAtom;
	if(consume(FUNCTION))
	{
		if (consume(ID))
		{
			if (consume(LPAR))
			{
				if (funcParams())
				{
					if (consume(RPAR))
					{
						if (consume(COLON))
						{
							if (baseType())
							{
								for (;;)
								{
									if (defVar())
									{
										;
									}
									else break;
								}
								if (block())
								{
									if (consume(END))
									{
										return 1;
									}else errPrint("lipseste end dupa block\n");
								}else errPrint("lipseste block\n");
							}else errPrint("lipseste tipul de variabila pentru functie\n");
						}else errPrint("lipseste : dupa )\n");
					}else errPrint("lipseste ) dupa parametrii functiei\n");
				}
			}else errPrint("lipseste ( dupa numele functiei\n");
		}else errPrint("lipseste numele functiei\n");
	}
	idxCrtAtom=startIdx;
	return 0;
}

// expr ::= exprLogic
int expr()
{
	int startIdx=idxCrtAtom;
	if(exprLogic())
	{
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprAssign ::= ( ID ASSIGN )? exprComp
int exprAssign()
{
	int startIdx=idxCrtAtom;
	if(consume(ID))
	{
		if (consume(ASSIGN))
		{
			if (exprComp())
			{
				return 1;
			}else errPrint("lipseste valoarea variabilei\n");
		}
		else
		{
			idxCrtAtom=startIdx;
		}
	}
	if (exprComp())
	{
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprComp ::= exprAdd ( ( LESS | EQUAL ) exprAdd )?
int exprComp()
{
	int startIdx=idxCrtAtom;
	if(exprAdd())
	{
		if (consume(LESS) || consume(EQUAL))
		{
			if (exprAdd())
			{
				return 1;
			}else errPrint("lipseste valoarea dupa < sau dupa =\n");
			idxCrtAtom=startIdx;
			return 0;
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprLogic ::= exprAssign ( ( AND | OR ) exprAssign )*
int exprLogic()
{
	int startIdx=idxCrtAtom;
	if(exprAssign())
	{
		for (;;)
		{
			if (consume(AND) || consume(OR))
			{
				if (exprAssign())
				{
					;
				}
				else
				{
					errPrint("lipseste urmatoarea conditie dupa && sau dupa ||\n");
					idxCrtAtom=startIdx;
					return 0;
				}
			}
			else break;
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprAdd ::= exprMul ( ( ADD | SUB ) exprMul )*
int exprAdd()
{
	int startIdx=idxCrtAtom;
	if(exprMul())
	{
		for (;;)
		{
			if (consume(ADD))
			{
				if (exprMul())
				{
					;
				}
				else
				{
					errPrint("lipseste o valoare dupa +\n");
					idxCrtAtom=startIdx;
					return 0;
				}
			}
			else break;

			if (consume(SUB)) {
				if (exprMul()) {
					;
				}
				else {
					errPrint("lipseste o valoare dupa -\n");
					idxCrtAtom = startIdx;
					return 0;
				}
			}
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprMul ::= exprPrefix ( ( MUL | DIV ) exprPrefix )*
int exprMul()
{
	int startIdx=idxCrtAtom;
	if(exprPrefix())
	{
		for (;;)
		{
			if (consume(MUL) || consume(DIV))
			{
				if (exprPrefix())
				{
					;
				}
				else
				{
					errPrint("lipseste o valoare dupa * sau dupa /\n");
					idxCrtAtom=startIdx;
					return 0;
				}
			}
			else break;
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// exprPrefix ::= ( SUB | NOT )? factor
int exprPrefix()
{
	int startIdx=idxCrtAtom;
	if(consume(SUB) || consume(NOT))
	{
		if (factor())
		{
			return 1;
		}else errPrint("lipseste variabila dupa - sau !\n");
	}
	if (factor())
	{
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// instr ::= expr? SEMICOLON
//            | IF LPAR expr RPAR block ( ELSE block )? END
//            | RETURN expr SEMICOLON
//            | WHILE LPAR expr RPAR block END
int instr()
{
	int startIdx=idxCrtAtom;
	if(expr())
	{
		if (consume(SEMICOLON))
		{
			return 1;
		}else errPrint("lipseste ; dupa expr\n");
	}
	else if (consume(SEMICOLON))
	{
		return 1;
	}
//            | IF LPAR expr RPAR block ( ELSE block )? END
	else if (consume(IF))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (block())
					{
						if (consume(ELSE))
						{
							if (block())
							{
								;
							}
							else
							{
								errPrint("lipseste block dupa else\n");
								idxCrtAtom=startIdx;
								return 0;
							}
						}
						if (consume(END))
						{
							return 1;
						}else errPrint("lipseste end dupa block\n");
					}else errPrint("lipseste block dupa )\n");
				}else errPrint("lipseste ) dupa expr\n");
			}else errPrint("lipseste expr dupa (\n");
		}else errPrint("lipseste ( dupa if\n");
	}
//            | RETURN expr SEMICOLON
	else if (consume(RETURN))
	{
		if (expr())
		{
			if (consume(SEMICOLON))
			{
				return 1;
			}else errPrint("lipseste ; dupa expr\n");
		}else errPrint("lipseste expresie dupa return\n");
	}
//            | WHILE LPAR expr RPAR block END
	else if (consume(WHILE))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				if (consume(RPAR))
				{
					if (block())
					{
						if (consume(END))
						{
							return 1;
						}else errPrint("lipseste end dupa block\n");
					}else errPrint("lipseste block dupa )\n");
				}else errPrint("lipseste ) dupa expr\n");
			}else errPrint("lipseste expr dupa (\n");
		}else errPrint("lipseste ( dupa while\n");
	}
	idxCrtAtom=startIdx;
	return 0;
}

// block ::= instr+
int block()
{
	int startIdx=idxCrtAtom;
	if(instr())
	{
		for (;;)
		{
			if (instr())
			{
				;
			}
			else break;
		}
		return 1;
	}
	idxCrtAtom=startIdx;
	return 0;
}

// funcParam ::= ID COLON baseType
int funcParam()
{
	int startIdx=idxCrtAtom;
	if(consume(ID))
	{
		if (consume(COLON))
		{
			if (baseType())
			{
				return 1;
			}else errPrint("tip invalid de variabila\n");
		}else errPrint("lipseste : dupa numele parametrului\n");
	}
	idxCrtAtom=startIdx;
	return 0;
}

// funcParams ::= ( funcParam ( COMMA funcParam )* )?
int funcParams()
{
	int startIdx=idxCrtAtom;
	if(funcParam())
	{
		for (;;)
		{
			if (consume(COMMA))
			{
				if (funcParam())
				{
					;
				}
				else
				{
					errPrint("lipseste parametru dupa virgula\n");
					idxCrtAtom=startIdx;
					return 0;
				}
			}
			else break;
		}
		return 1;
	}
	return 1;
}

// defVar ::= VAR ID COLON baseType SEMICOLON
int defVar(){
	int startIdx=idxCrtAtom;
	if(consume(VAR)){
		if(consume(ID)){
			if(consume(COLON)){
				if(baseType()){
					if(consume(SEMICOLON)){
						return 1;
					}else errPrint("lipseste ; la finalul declaratiei de variabila\n");
				}else errPrint("tip invalid de variabila\n");
			}else errPrint("lipseste : dupa numele variabilei\n");
		}else errPrint("lipseste numele variabilei\n");
	}
	idxCrtAtom=startIdx;
	return 0;
}

// program ::= ( defVar | defFunc | block )* FINISH
int program(){
	int startIdx=idxCrtAtom;
	for(;;)
	{
		if(defVar()){}
		else if(defFunc()){}
		else if(block()){}
		else break;
	}
	if(consume(FINISH)){
		return 1;
	}else errPrint("lipseste FINISH\n");
	idxCrtAtom=startIdx;
	return 0;
}


int main() {
	system("COLOR 03");

	FILE* fis;
	fis = fopen("programQuick.q", "rb");
	if (fis == NULL) {
		printf("nu s-a putut deschide fisierul");
		return -1;
	}
	int n = fread(bufin, 1, 30000, fis);
	bufin[n] = '\0';
	fclose(fis);
	pch = bufin;

	//ALEX
	while (getNextTk() != FINISH) {
	}
	printAtomi();

	//ASIN
	if (program()) {
		printf("\n		sintaxa OK\n");
	}
	else { printf("\n		eroare de sintaxa\n"); }

	system("pause");
	return 0;
}