#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

/* ---------------IO-----------------------
 * Set correct file pointer                */
FILE *fpIn = NULL;
FILE *fpOut = NULL;
int currentLine = 1;

void printToken(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(fpOut, "%d:%d:", currentLine, 0);
    vfprintf(fpOut, fmt, args);
    va_end(args);
}

void ungetChar(char c) {
    if (c == '\n') {
        currentLine--;
    }
    ungetc(c, fpIn);
}


/* ---------------STATES--------------------
 * States represent different lexing modes */
typedef enum {
    STATE_INIT,
    STATE_IDENT,
    STATE_NUMBER,
    STATE_NEWLINE,
    STATE_SYMBOL,
    STATE_COMMENT,
    STATE_CHAR_LITERAL,
    STATE_STRING_LITERAL,
    STATE_IMPORT,
    STATE_C_EMBED
} State;
static State state = STATE_INIT;

/* ---------------C-EMBED--------------------
 * Count of braces                         */
static int braceCount = 0;

void pushBrace() {
    braceCount++;
}

bool popBrace() {
    assert(braceCount > 0);
    braceCount--;
}


/* ---------------STACK----------------------
 * Stack of chars used to build up lexemes */
#define STACK_LEN (1024)
int     stackLen = 0;
char    stack[STACK_LEN + 1] = {0};

void stackPush(char c) {
    assert(stackLen < STACK_LEN);
    stack[stackLen++] = c;
    stack[stackLen] = '\0';
}

char stackPop() {
    assert(stackLen > 0);
    char c = stack[--stackLen];
    stack[stackLen] = '\0';
    return c;
}

void stackClear() {
    stackLen = 0;
    stack[0] = '\0';
}


/* ----------------INDENT---------------------
 * Stack of strings stores current indentation */
#define MAX_INDENT_LEVEL (64)
#define MAX_TOKEN_LENGTH STACK_LEN
char indentStack[MAX_INDENT_LEVEL][MAX_TOKEN_LENGTH+1] = {0};
int  indentStackLen = 1;

void indent(const char *spaces) {
    assert(indentStackLen > 0);
    char *entry = indentStack[indentStackLen - 1];
    int lenSpaces = strlen(spaces);
    int lenEntry  = strlen(entry);
    for (int i = 0; i < lenSpaces && i < lenEntry; i++) {
        assert(spaces[i] == entry[i]);
    }

    if (lenSpaces == lenEntry) { // same indent
        printToken("newline:\n");
    } else if (lenSpaces > lenEntry) { // greater indent
        printToken("indent:\n");
        strcpy(indentStack[indentStackLen++], spaces);
    } else if (lenSpaces < lenEntry) { // lesser indent
        printToken("newline:\n");

        for (;;) {
            assert(indentStackLen > 0);
            char *entry   = indentStack[indentStackLen - 1];
            int lenEntry  = strlen(entry);
            for (int i = 0; i < lenSpaces && i < lenEntry; i++) {
                assert(spaces[i] == entry[i]);
            }

            if (lenSpaces < lenEntry) {
                printToken("dedent:\n");
                indentStackLen--;
            } else if (lenSpaces == lenEntry) {
                break;
            } else {
                assert(0);
            }
        }

    } else {
        assert(0); 
    }
}


void init() {
    fpIn = stdin;
    fpOut = stdout;
    currentLine = 1;
    state = STATE_INIT;

    stackLen = 0;
    memset(stack, 0, sizeof(stack));

    indentStackLen = 1;
    memset(indentStack, 0, sizeof(indentStack));

    braceCount = 0;
}


/* ------------------HELPERS--------------------*/
/* helper functions to validate chars and strings */
bool issymbol(char c) {
    char *symbols = "=+-*/%!&|(){}[].,:;_<>";
    for (int i = 0; symbols[i] != '\0'; i++) {
        if (c == symbols[i]) {
            return true;
        }
    }
    return false;
}

bool isDoubleSymbol(char *s) {
    char *doubles[] = { "&&", "||", "..", "::", "==", "!=", ">=", "<=", "->", "+=", NULL };
    for (int i = 0; doubles[i] != NULL; i++) {
        if (strcmp(s, doubles[i]) == 0) {
            return true;
        }
    }
    return false;
}

bool isValidCharLiteral(char c) {
    if (isalpha(c) || isdigit(c) || c == ' ') {
        return true;
    }
    char *chars = "!@#$%^&*()_+-={}[]:;\"<>?.,/|";
    for (int i = 0; chars[i] != '\0'; i++) {
        if (c == chars[i]) {
            return true;
        }
    }
    return false;
}

bool isKeyword(char *s) {
    char *keywords[] = {
        "module",
        "import",
        "include",
        "link",
        "const",
        "for",
        "while",
        "if",
        "else",
        "fn",
        "return",
        "switch",
        "bool",
        "string",
        "i64",
        "i32",
        "i16",
        "i8",
        "u8",
        "f64",
        "f32",
        "char",
        "data",
        "let",
        "null",
        "true",
        "false",
        "type",
        NULL
    };
    for (int i = 0; keywords[i] != NULL; i++) {
        if (strcmp(s, keywords[i]) == 0) {
            return true;
        }
    }
    return false;
}

bool lex() { // returns false for EOF
    char c = fgetc(fpIn);
    if (c == '\n') {
        currentLine++;
    }

    switch (state) {
    case STATE_INIT:
        if (isalpha(c)) {
            state = STATE_IDENT;
            stackPush(c);
        } else if (isdigit(c)) {
            state = STATE_NUMBER;
            stackPush(c);
        } else if (issymbol(c)) {
            state = STATE_SYMBOL;
            stackPush(c);
        } else if (c == '\'') {
            state = STATE_CHAR_LITERAL;
            stackPush(c);
        } else if (c == '\n') {
            state = STATE_NEWLINE;
            stackPush(c);
        } else if (c == '"') {
            state = STATE_STRING_LITERAL;
        } else if (c == ' ' || c == '\t') {
            // do nothing
        } else if (c == '$') {
            state = STATE_C_EMBED;
        } else if (c == EOF) {
            return false;
        } else {
            assert(0);
        }
        break;

    case STATE_IDENT:
        if (isalpha(c) || isdigit(c) || c == '_') {
            stackPush(c);
        } else {
            // print ident
            if (strcmp(stack, "import") == 0 || strcmp(stack, "include") == 0 || strcmp(stack, "link") == 0) {
                assert(c == ' ');
                stackPush(':');
                stackPush(' ');
                state = STATE_IMPORT;
            } else {
                if (isKeyword(stack)) {
                    printToken("keyword: %s\n", stack);
                } else {
                    printToken("ident: %s\n", stack);
                }
                stackClear();

                state = STATE_INIT;
                ungetChar(c);
            }
        } 
        break;

    case STATE_IMPORT:
        if (c == '\n') {
            printToken("%s\n", stack);
            stackClear();
            ungetChar(c);
            state = STATE_INIT;
        } else {
            stackPush(c);
        }
        break;

    case STATE_NUMBER:
        if (c == '.' && stack[stackLen - 1] == '.') { // .. symbol
            stackPop();
            if (strchr(stack, '.') == NULL) {
                printToken("integer: %s\n", stack);
            } else {
                printToken("float: %s\n", stack);
            }
            printToken("symbol: ..\n");
            stackClear();
            state = STATE_INIT;

        } else if (isdigit(c) || c == '.') {
            stackPush(c);
        } else {
            if (strchr(stack, '.') == NULL) {
                printToken("integer: %s\n", stack);
            } else {
                printToken("float: %s\n", stack);
            }
            stackClear();

            state = STATE_INIT;
            ungetChar(c);
        }
        break;

    case STATE_SYMBOL:
        stackPush(c);
        if (strcmp(stack, "//") == 0) {
            state = STATE_COMMENT;
        } else {
            if (!isDoubleSymbol(stack)) {
                ungetChar(stackPop());
            }

            printToken("symbol: %s\n", stack);
            stackClear();
            state = STATE_INIT;
        }
        break;

    case STATE_COMMENT:
        if (c == '\n') {
            stackClear();
            ungetChar(c);
            state = STATE_INIT;
        } else {
            stackPush(c);
        }
        break;

    case STATE_STRING_LITERAL:
        if (c == '"' && stackLen > 0 && stack[stackLen - 1] == '\\') {
            stackPush(c);
        } else if (c == '"') {
            printToken("string: %s\n", stack);
            stackClear();
            state = STATE_INIT;
        } else {
            stackPush(c);
        }

        break;

    case STATE_CHAR_LITERAL:
        if (strcmp(stack, "'") == 0) {
            if (c == '\\' || isValidCharLiteral(c)) {
                stackPush(c);
            } else {
                assert(0);
            }
        } else if (strcmp(stack, "'\\") == 0) { // quote and slash
            assert(c == 'n' || c == 't' || c == '\'' || c == '0' || c == '\\');
            stackPush(c);
        } else if (stackLen == 2) { // 'c'
            assert(c == '\'');

            printToken("char: '%c'\n", stack[1]);
            stackClear();
            state = STATE_INIT;
        } else if (stackLen == 3) { // '\n'
            switch (stack[2]) {
                case 'n': printToken("char: '\\n'\n"); break;
                case 't': printToken("char: '\\t'\n"); break;
                case '0': printToken("char: '\\0'\n"); break;
                case '\'': printToken("char: '\\''\n"); break;
                case '\\': printToken("char: '\\\\'\n"); break;
                default: assert(0); break;
            }
            stackClear();
            state = STATE_INIT;
        } else {
            assert(0);
        }
        break;

    case STATE_NEWLINE:
        /* newline needs to be able to skip comments, otherwise multiple newline tokens get
         * genereted. */
        if (stack[stackLen - 1] == '/') {
            if (c == '/') {
                stackPop();
                stackPush('#');
            } else {
                assert(false); // would not expect a different character after first /
            }
        } else if (stack[stackLen - 1] == '#') { // # means comment mode started
            if (c == '\n') {
                stackPop();
                ungetChar(c);
            }
        } else {
            if (c == '\n' || c == ' ' || c == '\t' || c == '/') {
                stackPush(c);
            } else {
                indent(strrchr(stack, '\n') + 1); // advance to character after last newline
                stackClear();
                state = STATE_INIT;
                ungetChar(c);
            }
        }
        break;

    case STATE_C_EMBED:
        if (c == '{') {
            pushBrace();
            stackPush(c);
        } else if (c == '}') {
            popBrace();
            stackPush(c);
            if (braceCount == 0) {
                // replace newlines with ascii 31
                for (int i = 0; stack[i] != '\0'; i++) {
                    if (stack[i] == '\n') {
                        stack[i] = 31;
                    }
                }

                printToken("cembed: %s\n", stack);
                stackClear();
                state = STATE_INIT;
            }
        } else {
            stackPush(c);
        }
        break;
            
    default:
        assert(0);
        break;
    }

    return true;
}

void lexFile(const char *filenameIn, const char *filenameOut) {
    init();

    FILE *pin = fopen(filenameIn, "r");
    FILE *pout = fopen(filenameOut, "w");
    assert(pin != NULL);
    assert(pout != NULL);

    fpIn = pin;
    fpOut = pout;

    while (lex()) {
    }

    fclose(pin);
    fclose(pout);
}
