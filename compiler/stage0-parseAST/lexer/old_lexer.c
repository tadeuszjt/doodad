#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#define MAX_INDENT_LENGTH (1024)
#define MAX_KEYWORD_LENGTH (128)

/* ---------------IO----------------------- */
typedef struct {
    char c;
    int line;
    int column;
} IoChar;

typedef struct {
    int idx;
    int len;
    int rwd;
    IoChar ring[MAX_INDENT_LENGTH];
} IoCharRing;


IoCharRing IoCharRing_init() {
    IoCharRing ioCharQueue = {0};
    return ioCharQueue;
}


void IoCharRing_push(IoCharRing *cr, IoChar ioc) {
    assert(cr != NULL && cr->idx == cr->rwd);
    cr->ring[cr->idx] = ioc;
    cr->idx = (cr->idx + 1) % MAX_INDENT_LENGTH;
    if (cr->len < MAX_INDENT_LENGTH) {
        cr->len++;
    }
    cr->rwd = cr->idx;
}

IoChar IoCharRing_pop(IoCharRing *cr) {
    assert(cr != NULL && cr->idx != cr->rwd && cr->len > 0);
    IoChar c = cr->ring[cr->rwd];
    cr->rwd = (cr->rwd + 1) % MAX_INDENT_LENGTH;
    return c;
}

void IoCharRing_rewind(IoCharRing *cr) {
    assert(cr != NULL && cr->len > 0);
    cr->rwd--;
    if (cr->rwd < 0) {
        cr->rwd = MAX_INDENT_LENGTH - 1;
    } 
    assert(cr->rwd != cr->idx);
}


typedef struct {
    FILE *fpIn;
    FILE *fpOut;
    int currentLine;
    int currentColumn;
    int rewindCount;
    IoCharRing ring;
} Io;


Io Io_init(FILE *fpIn, FILE *fpOut) {
    assert(fpIn != NULL && fpOut != NULL);

    Io io = {0};
    io.fpIn = fpIn;
    io.fpOut = fpOut;
    io.currentLine = 1;
    io.currentColumn = 1;
    io.rewindCount = 0;
    io.ring = IoCharRing_init();
    return io;
}


IoChar Io_getChar(Io *io) {
    assert(io != NULL);

    if (io->rewindCount == 0) {
        char c = fgetc(io->fpIn);

        IoChar ioChar = {0};
        ioChar.c      = c;
        ioChar.line   = io->currentLine;
        ioChar.column = io->currentColumn;

        if (c == '\n') {
            io->currentLine++;
            io->currentColumn = 0;
        } else {
            io->currentColumn++;
        }

        IoCharRing_push(&io->ring, ioChar);
        return ioChar;
    } else {
        io->rewindCount--;
        return IoCharRing_pop(&io->ring);
    }
}

void Io_rewind(Io *io, int n) { 
    assert(io != NULL);
    for (int i = 0; i < n; i++) {
        IoCharRing_rewind(&io->ring);
    }
    io->rewindCount += n;
}

void Io_printf(Io *io, const char *fmt, ...) {
    assert(io != NULL);
    va_list args;
    va_start(args, fmt);
    vfprintf(io->fpOut, fmt, args);
    va_end(args);
}

void Io_putchar(Io *io, char c) {
    assert(io != NULL);
    fprintf(io->fpOut, "%c", c);
}


/* ------------------HELPERS--------------------*/
/* helper functions to validate chars and strings */
void printSpaces(const char *spaces) {
    printf("print: ");
    for (int i = 0; spaces[i] != '\0'; i++) {
        if (spaces[i] == '\t') {
            printf("\\t");
        } else if (spaces[i] == ' ') {
            printf("\\s");
        } else if (spaces[i] == '\n') {
            printf("\\n");
        } else {
            assert(false);
        }
    }

    printf("\n");
}


bool isLetter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}


bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

bool isPrefixOf(const char *left, const char *right) {
    assert(left != NULL && right != NULL);
    for (int i = 0;; i++) {
        if (left[i] == '\0') {
            return true;
        } else if (right[i] == '\0') {
            return false;
        } else if (left[i] != right[i]) {
            return false;
        }
    }
}


bool isSymbol(char c) {
    char *symbols = "=+-*/%!&|(){}[].,:;_<>&@";
    for (int i = 0; symbols[i] != '\0'; i++) {
        if (c == symbols[i]) {
            return true;
        }
    }
    return false;
}

bool isDoubleSymbol(char *s) {
    char *doubles[] = { "&&", "||", "..", "::", "==", "!=", ">=", "<=", "->", "+=", "++", NULL };
    for (int i = 0; doubles[i] != NULL; i++) {
        if (strcmp(s, doubles[i]) == 0) {
            return true;
        }
    }
    return false;
}

int isAnyBracket(char c) {
    const char *increasing = "([{";
    const char *decreasing = ")]}";
    for (int i = 0; increasing[i] != '\0'; i++) {
        if (increasing[i] == c) {
            return 1;
        }
    }
    for (int i = 0; decreasing[i] != '\0'; i++) {
        if (decreasing[i] == c) {
            return -1;
        }
    }
    return 0;
}

bool isValidCharLiteral(char c) {
    if (isLetter(c) || isDigit(c) || c == ' ') {
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
        "fn",
        "if",
        "in",
        "for",
        "let",
        "else",
        "enum",
        "true",
        "type",
        "func",
        "inst",
        "with",
        "false",
        "tuple",
        "while",
        "return",
        "switch",
        "derives",
        NULL
    };
    for (int i = 0; keywords[i] != NULL; i++) {
        if (strcmp(s, keywords[i]) == 0) {
            return true;
        }
    }
    return false;
}


/* ----------------INDENT---------------------
 * Stack of strings stores current indentation */

typedef struct {
    // \t and \s separated by \n
    char stack[MAX_INDENT_LENGTH + 1];
} IndentStack;


IndentStack IndentStack_init() {
    IndentStack indentStack = {0};
    return indentStack;
}


void IndentStack_pop(IndentStack *is) {
    assert(is != NULL);
    assert(is->stack[0] != '\0');
    char *lastNewline = strrchr(is->stack, '\n');
    if (lastNewline == NULL) {
        is->stack[0] = '\0';
    } else {
        *lastNewline = '\0';
    }
}


void IndentStack_push(IndentStack *is, const char *spaces) {
    assert(is != NULL && spaces != NULL);
    assert( (1 + strlen(spaces) + strlen(is->stack)) < MAX_INDENT_LENGTH);

    int len = strlen(is->stack);

    if (len > 0) {
        strcat(is->stack, "\n");
    }
    strcat(is->stack, spaces);
}


int IndentStack_processIndent(IndentStack *is, const char *spaces) {
    assert(is != NULL && spaces != NULL);

    char curIndent[MAX_INDENT_LENGTH + 1] = {0};
    int j = 0;
    for (int i = 0; is->stack[i] != '\0'; i++) {
        if (is->stack[i] != '\n') {
            curIndent[j++] = is->stack[i];
        }
    }

    if (strcmp(spaces, curIndent) == 0) { // same indent
        return 0;
    } else if (isPrefixOf(curIndent, spaces)) { // greater indent
        IndentStack_push(is, spaces + strlen(curIndent));
        return 1;
    } else if (isPrefixOf(spaces, curIndent)) { // lesser indent
        IndentStack_pop(is);
        int result = IndentStack_processIndent(is, spaces);
        assert(result <= 0);
        return result - 1;
    }

    assert(false);
}


/* lex functions ------------------------------------------------*/
bool lexInteger(Io *io) {
    IoChar first = Io_getChar(io);
    Io_rewind(io, 1);

    if (!isDigit(first.c)) {
        return false;
    }

    Io_printf(io, "%d:%d:integer: ", first.line, first.column);

    for (;;) {
        IoChar ioc = Io_getChar(io);
        if (!isDigit(ioc.c)) {
            Io_rewind(io, 1);
            break;
        }
        Io_putchar(io, ioc.c);
    }

    Io_putchar(io, '\n');
    return true;
}

bool lexFloating(Io *io) {
    IoChar first = Io_getChar(io);
    Io_rewind(io, 1);
    if (!isDigit(first.c)) {
        return false;
    }

    int prefixLen = 0;
    for (;; prefixLen++) {
        if (!isDigit(Io_getChar(io).c)) {
            Io_rewind(io, 1);
            break;
        }
    }

    if (Io_getChar(io).c != '.') {
        Io_rewind(io, 1 + prefixLen);
        return false;
    }

    // prevent '3..' from lexing as float
    if (Io_getChar(io).c == '.') {
        Io_rewind(io, 2 + prefixLen);
        return false;
    } else {
        Io_rewind(io, 1);
    }


    int postfixLen = 0;
    for (;; postfixLen++) {
        if (!isDigit(Io_getChar(io).c)) {
            Io_rewind(io, 1);
            break;
        }
    }

    // prevent parse error for 'tuple.0.func'
    if (postfixLen == 0 && isLetter(Io_getChar(io).c)) {
        Io_rewind(io, 2 + prefixLen + postfixLen);
        return false;
    }

    Io_rewind(io, prefixLen + 1 + postfixLen);

    Io_printf(io, "%d:%d:floating: ", first.line, first.column);
    for (int i = 0; i < (prefixLen + 1 + postfixLen); i++) {
        Io_printf(io, "%c", Io_getChar(io).c);
    }
    Io_printf(io, "\n");
    return true;
}

bool lexIdent(Io *io) {
    IoChar ioc = Io_getChar(io);
    Io_rewind(io, 1);

    if (!isLetter(ioc.c)) {
        return false;
    }

    Io_printf(io, "%d:%d:%cdent: ", ioc.line, ioc.column, isupper(ioc.c) ? 'I' : 'i');

    for (;;) {
        ioc = Io_getChar(io);
        if (!isLetter(ioc.c) && !isDigit(ioc.c) && !(ioc.c == '_')) {
            Io_rewind(io, 1);
            break;
        }

        Io_putchar(io, ioc.c);
    }

    Io_putchar(io, '\n');
    return true;
}


bool lexKeyword(Io *io) {
    IoChar first = Io_getChar(io);
    Io_rewind(io, 1);

    if (!isLetter(first.c)) {
        return false;
    }

    int  keywordLen = 0;
    char keyword[MAX_KEYWORD_LENGTH + 1] = {0};

    for (;;) {
        IoChar ioc = Io_getChar(io);
        if (!isLetter(ioc.c) && !(ioc.c == '_') && !isDigit(ioc.c)) {
            Io_rewind(io, 1);
            break;
        }

        assert(keywordLen < MAX_KEYWORD_LENGTH);
        keyword[keywordLen++] = ioc.c;
    }


    if (!isKeyword(keyword)) {
        Io_rewind(io, keywordLen);
        return false;
    }

    Io_printf(io, "%d:%d:keyword: %s\n", first.line, first.column, keyword);
    return true;
}


bool lexImport(Io *io) {
    IoChar first = Io_getChar(io);
    Io_rewind(io, 1);
    if (!isLetter(first.c)) {
        return false;
    }

    int  keywordLen = 0;
    char keyword[MAX_KEYWORD_LENGTH + 1] = {0};

    for (;;) {
        IoChar ioc = Io_getChar(io);
        if (!isLetter(ioc.c) && !(ioc.c == '_')) {
            Io_rewind(io, 1);
            break;
        }

        assert(keywordLen < MAX_KEYWORD_LENGTH);
        keyword[keywordLen++] = ioc.c;
    }

    if (strcmp(keyword, "import") != 0  &&
        strcmp(keyword, "export") != 0  &&
        strcmp(keyword, "include") != 0 &&
        strcmp(keyword, "link") != 0    &&
        strcmp(keyword, "module") != 0) {
        Io_rewind(io, keywordLen);
        return false;
    }

    IoChar space = Io_getChar(io);
    Io_rewind(io, 1);
    if (space.c != ' ' && space.c != '\t') {
        Io_rewind(io, keywordLen);
        return false;
    }

    Io_printf(io, "%d:%d:%s:", first.line, first.column, keyword);

    for (;;) {
        IoChar ioc = Io_getChar(io);
        if (ioc.c == '\n' || ioc.c == EOF) {
            Io_rewind(io, 1);
            break;
        }

        Io_printf(io, "%c", ioc.c);
    }

    Io_printf(io, "\n");
    return true;
}


bool lexSymbol(Io *io, int *anyBracketCount) {
    assert(io != NULL);

    IoChar ioc = Io_getChar(io);
    if (!isSymbol(ioc.c)) {
        Io_rewind(io, 1);
        return false;
    }

    *anyBracketCount += isAnyBracket(ioc.c);

    Io_printf(io, "%d:%d:symbol: %c\n", ioc.line, ioc.column, ioc.c);
    return true;
}


bool lexDoubleSymbol(Io *io) {
    IoChar first = Io_getChar(io);
    IoChar second = Io_getChar(io);

    char symbol[3] = {first.c, second.c, '\0'};

    if (!isDoubleSymbol(symbol)) {
        Io_rewind(io, 2);
        return false;
    }

    Io_printf(io, "%d:%d:symbol: %s\n", first.line, first.column, symbol);
    return true;
}

bool lexComment(Io *io) {
    IoChar first = Io_getChar(io);
    IoChar second = Io_getChar(io);

    if (first.c != '/' || second.c != '/') {
        Io_rewind(io, 2);
        return false;
    }

    for (;;) {
        IoChar ioc = Io_getChar(io);
        if (ioc.c == '\n' || ioc.c == EOF) {
            Io_rewind(io, 1);
            break;
        }
    }

    return true;
}


bool lexStringLiteral(Io *io) {
    IoChar first = Io_getChar(io);
    if (first.c != '"') {
        Io_rewind(io, 1);
        return false;
    }

    Io_printf(io, "%d:%d:string: ");

    IoChar prev = first;
    for (;;) {
        IoChar ioc = Io_getChar(io);
        assert(ioc.c != '\n');
        assert(ioc.c != EOF);

        if (ioc.c == '"' && prev.c != '\\') {
            break;
        }

        Io_putchar(io, ioc.c);

        prev = ioc;
    }

    Io_putchar(io, '\n');
    return true;
}

bool lexCharLiteralEscaped(Io *io) {
    IoChar first = Io_getChar(io);
    if (first.c != '\'') {
        Io_rewind(io, 1);
        return false;
    }

    IoChar second = Io_getChar(io);
    if (second.c != '\\') {
        Io_rewind(io, 2);
        return false;
    }

    IoChar third = Io_getChar(io);

    IoChar fourth = Io_getChar(io);
    if (fourth.c != '\'') {
        Io_rewind(io, 4);
        return false;
    }

    switch (third.c) { 
    case 'n': Io_printf(io, "%d:%d:char: '\\n'\n", first.line, first.column); break;
    case 't': Io_printf(io, "%d:%d:char: '\\t'\n", first.line, first.column); break;
    case '0': Io_printf(io, "%d:%d:char: '\\0'\n", first.line, first.column); break;
    case '\'': Io_printf(io, "%d:%d:char: '\\''\n", first.line, first.column); break;
    case '\\': Io_printf(io, "%d:%d:char: '\\\\'\n", first.line, first.column); break;
    default:
        Io_rewind(io, 4);
        return false;
        break;
    }

    return true;
}


bool lexCharLiteral(Io *io) {
    IoChar first = Io_getChar(io);
    if (first.c != '\'') {
        Io_rewind(io, 1);
        return false;
    }

    IoChar second = Io_getChar(io);
    if (!isValidCharLiteral(second.c)) { 
        Io_rewind(io, 2);
        return false;
    }

    IoChar third = Io_getChar(io);
    if (third.c != '\'') {
        Io_rewind(io, 3);
        return false;
    }

    Io_printf(io, "%d:%d:char: '%c'\n", first.line, first.column, second.c);

    return true;
}

bool lexNewline(Io *io, IndentStack *is, int *bracketLevelCount) {
    assert(io != NULL && is != NULL);
    IoChar first = Io_getChar(io);
    Io_rewind(io, 1);
    if (first.c != '\n') {
        return false;
    }

    bool commentMode = false;
    int  spacesLen = 0;
    char spaces[MAX_INDENT_LENGTH + 1] = {0};

    for (;;) {
        if (commentMode) {
            IoChar ioc = Io_getChar(io);
            if (ioc.c == '\n' || ioc.c == EOF) {
                commentMode = false;
                Io_rewind(io, 1);
            }
        } else {
            IoChar ioc0 = Io_getChar(io);
            IoChar ioc1 = Io_getChar(io);

            if (ioc0.c == '/' && ioc1.c == '/') {
                commentMode = true;
            } else if (ioc0.c == '\n' || ioc0.c == ' ' || ioc0.c == '\t') {
                Io_rewind(io, 1);
                assert(spacesLen < MAX_INDENT_LENGTH);
                spaces[spacesLen++] = ioc0.c;
            } else {
                Io_rewind(io, 2);
                break;
            }
        }
    }

    if (*bracketLevelCount == 0) {
        char *afterN = strrchr(spaces, '\n') + 1;
        int result = IndentStack_processIndent(is, afterN);

        if (result == 0) {
            Io_printf(io, "%d:%d:newline:\n", first.line, first.column);
        } else if (result == 1) {
            Io_printf(io, "%d:%d:indent:\n", first.line, first.column);
        } else if (result < 0) {
            Io_printf(io, "%d:%d:newline:\n", first.line, first.column);
            for (int i = 0; i > result; i--) { 
                Io_printf(io, "%d:%d:dedent:\n", first.line, first.column);
            }
        }
    }

    return true;
}


bool lexCEmbed(Io *io) {
    IoChar first = Io_getChar(io);
    if (first.c != '$') {
        Io_rewind(io, 1);
        return false;
    }

    IoChar second = Io_getChar(io);
    if (second.c != '{') {
        Io_rewind(io, 2);
        return false;
    }

    int braceCount = 1;

    Io_printf(io, "%d:%d:cembed: {", first.line, first.column);

    while (braceCount > 0) {
        IoChar ioc = Io_getChar(io);
        if (ioc.c == EOF) {
            assert(false); 
        } else if (ioc.c == '{') {
            braceCount++;
        } else if (ioc.c == '}') {
            braceCount--;
        }

        if (ioc.c == '\n') {
            Io_printf(io, "%c", (char)31);
        } else {
            Io_printf(io, "%c", ioc.c);
        }
    }

    Io_printf(io, "\n");
    return true;
}

void lexFile(const char *filenameIn, const char *filenameOut) {
    FILE *pin = fopen(filenameIn, "r");
    FILE *pout = fopen(filenameOut, "w");
    assert(pin != NULL);
    assert(pout != NULL);

    Io io = Io_init(pin, pout);
    IndentStack indentStack = IndentStack_init();

    int bracketLevelCount = 0;

    for (;;) {
        if (lexComment(&io)) {
        } else if (lexNewline(&io, &indentStack, &bracketLevelCount)) {
        } else if (lexStringLiteral(&io)) {
        } else if (lexCEmbed(&io)) {
        } else if (lexFloating(&io)) {
        } else if (lexInteger(&io)) {
        } else if (lexDoubleSymbol(&io)) {
        } else if (lexSymbol(&io, &bracketLevelCount)) {
        } else if (lexCharLiteralEscaped(&io)) {
        } else if (lexCharLiteral(&io)) {
        } else if (lexImport(&io)) {
        } else if (lexKeyword(&io)) {
        } else if (lexIdent(&io)) {
        } else {
            IoChar ioc = Io_getChar(&io);
            if (ioc.c == EOF) {
                break;
            } else if (ioc.c == ' ' || ioc.c == '\t') {
            } else {
                fprintf(stderr, "unknown char: %c\n", ioc.c);
                assert(false);
            }
        }
    }

    fclose(pin);
    fclose(pout);
}
