/* Doodad Module: lexer */
#include "gc.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

char *doodad_string_plus(char *a, char *b);
bool doodad_string_eqeq(char *a, char *b);
char *doodad_string_copy(char *s);
char *doodad_string_alloc(size_t len);
char *doodad_string_char(char c);
char *doodad_string_i64(int64_t n);
typedef int64_t io_FileKey;

typedef int64_t io_FilePtr;

typedef struct { int64_t len; int64_t cap; io_FileKey* r0; io_FilePtr* r1; } table0;

typedef table0 io_Files;

typedef struct { io_FileKey m0; io_Files m1; } tuple0;

typedef tuple0 io_Io;

typedef struct { int64_t m0; int64_t m1; } tuple1;

typedef tuple1 lexer_Pos;

typedef struct { char* m0; lexer_Pos m1; } tuple2;

typedef tuple2 lexer_PosString;

typedef struct { } tuple3;

typedef struct { int64_t en; union { lexer_PosString u0; lexer_PosString u1; lexer_PosString u2; lexer_PosString u3; lexer_PosString u4; lexer_PosString u5; lexer_PosString u6; lexer_PosString u7; lexer_PosString u8; lexer_PosString u9; lexer_PosString u10; lexer_PosString u11; tuple3 u12; } ; } adt0;

typedef adt0 lexer_Token;

typedef struct { lexer_Token m0; lexer_PosString m1; } tuple4;

typedef struct { int64_t en; union { tuple3 u0; tuple4 u1; } ; } adt1;

typedef adt1 lexer_Result;
extern void assert_assert(bool);
extern void io_closeFile(io_Io*, io_FileKey);
extern void io_delete(io_Files*, int64_t);
extern void io_fPutStr(io_Io*, io_FileKey, char*);
extern void io_fPutStrLn(io_Io*, io_FileKey, char*);
extern char io_getChar(io_Io*);

typedef struct { int64_t en; union { int8_t u0; char* u1; } ; } adt2;
extern adt2 io_getStrLn(io_Io*);
extern io_FileKey io_newKey(io_Io*);
extern io_FileKey io_openFile(io_Io*, char*);
extern void io_putChar(io_Io*, char);
extern void io_putStr(io_Io*, char*);
extern void io_putStrLn(io_Io*, char*);
extern char* io_readFile(io_Io*, char*);

typedef struct { int64_t en; union { int8_t u0; char u1; } ; } adt3;
extern adt3 strings_at(char**, int64_t);
extern char* strings_drop(char*, int64_t);
extern bool strings_isAlpha(char);
extern bool strings_isDigit(char);
extern bool strings_isPrefix(char*, char*);
extern bool strings_isSpace(char);

typedef struct { int64_t en; union { int8_t u0; int64_t u1; } ; } adt4;

typedef struct { adt4 m0; char* m1; } tuple5;
extern tuple5 strings_readI64(char*);
extern char* strings_take(char*, int64_t);
extern adt3 lexer_At(lexer_PosString*, int64_t);
extern lexer_PosString lexer_Drop(lexer_PosString, int64_t);
extern lexer_PosString lexer_Take(lexer_PosString, int64_t);

typedef struct { int64_t len; int64_t cap; char** r0; } table1;
extern void lexer_indent(io_Io*, table1*, io_FileKey, lexer_PosString);
extern adt4 lexer_lex(lexer_PosString, int64_t);
extern lexer_Result lexer_lex_1(lexer_PosString);
extern lexer_Result lexer_lexCEmbed(lexer_PosString);
extern lexer_Result lexer_lexCharLiteral(lexer_PosString);
extern lexer_Result lexer_lexComment(lexer_PosString);
extern adt4 lexer_lexComment_1(lexer_PosString, int64_t);
extern void lexer_lexFile(char*, char*);
extern lexer_Result lexer_lexFloating(lexer_PosString);
extern lexer_Result lexer_lexIdent(lexer_PosString);
extern lexer_Result lexer_lexImport(lexer_PosString);
extern lexer_Result lexer_lexInclude(lexer_PosString);
extern lexer_Result lexer_lexInteger(lexer_PosString);
extern lexer_Result lexer_lexKeyword(lexer_PosString);
extern lexer_Result lexer_lexLink(lexer_PosString);
extern lexer_Result lexer_lexNewline(lexer_PosString);
extern adt4 lexer_lexSpaceChar(lexer_PosString, int64_t);
extern lexer_Result lexer_lexStringLiteral(lexer_PosString);
extern lexer_Result lexer_lexSymbol(lexer_PosString);
extern void lexer_main();
extern void lexer_pop(table1*);

adt3 lexer_At(lexer_PosString* lexer_str_2, int64_t lexer_idx) {
    int64_t call0 = strlen((*lexer_str_2).m0);
    if (((lexer_idx >= 0) && (lexer_idx < call0))) {
        adt3 adt5 = {1};
        adt5.u1 = (*lexer_str_2).m0[lexer_idx];
        return adt5;
    }
    adt3 adt6 = {0};
    return adt6;
}

lexer_PosString lexer_Drop(lexer_PosString lexer_str, int64_t lexer_num) {
    
    for (; ; ) {
        adt3 call1 = strings_at(&(lexer_str.m0), 0);
        bool matchNull0 = (1 == call1.en);
        if (!matchNull0) {
            goto matchSkip0;
        }
        char lexer_c = call1.u1;
        matchNull0 = true;
        matchSkip0:;
        bool match0 = matchNull0;
        if (match0) {
            match0 = (lexer_num > 0);
        }
        if (!match0) {
            break;
        }
        if ((lexer_c == '\n')) {
            lexer_str.m1.m1 = 0;
            lexer_str.m1.m0 = (lexer_str.m1.m0 + 1);
        }
        else {
            lexer_str.m1.m1 = (lexer_str.m1.m1 + 1);
        }
        { lexer_str.m0++; }
        lexer_num = (lexer_num - 1);
    }
    return lexer_str;
}

lexer_PosString lexer_Take(lexer_PosString lexer_str_1, int64_t lexer_num_1) {
    char* call2 = strings_take(lexer_str_1.m0, lexer_num_1);
    lexer_str_1.m0 = call2;
    return lexer_str_1;
}

void lexer_table_append0(table1* a, table1* b) {
    if (((a->len + b->len) >= a->cap)) {
        a->cap = ((a->len + b->len) * 2);
        void* mem0 = GC_malloc((a->cap * sizeof((*a->r0))));
        memcpy(mem0, a->r0, (a->len * sizeof((*a->r0))));
        a->r0 = mem0;
    }
    int64_t idx0 = 0;
    
    for (; (idx0 < b->len); idx0++) {
        a->r0[a->len] = b->r0[idx0];
        a->len++;
    }
}

typedef struct { char* arr[1]; } array1;

void lexer_indent(io_Io* lexer_io, table1* lexer_stack, io_FileKey lexer_key, lexer_PosString lexer_str_20) {
    assert_assert(((*lexer_stack).len > 0));
    char* lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
    int64_t call3 = strlen(lexer_entry);
    int64_t lexer_minLen = call3;
    int64_t call4 = strlen(lexer_str_20.m0);
    if ((call4 < lexer_minLen)) {
        int64_t call5 = strlen(lexer_str_20.m0);
        lexer_minLen = call5;
    }
    char* call6 = strings_take(lexer_str_20.m0, lexer_minLen);
    char* call7 = strings_take(lexer_entry, lexer_minLen);
    bool call8 = doodad_string_eqeq(call6, call7);
    assert_assert(call8);
    char* call9 = doodad_string_i64(lexer_str_20.m1.m0);
    char* call10 = doodad_string_plus(call9, ":");
    char* call11 = doodad_string_i64(lexer_str_20.m1.m1);
    char* call12 = doodad_string_plus(call10, call11);
    char* call13 = doodad_string_plus(call12, ":");
    char* lexer_posStr = call13;
    bool call14 = doodad_string_eqeq(lexer_str_20.m0, lexer_entry);
    if (call14) {
        char* call15 = doodad_string_plus(lexer_posStr, "newline:");
        io_fPutStrLn(lexer_io, lexer_key, call15);
    }
    else {
        int64_t call16 = strlen(lexer_str_20.m0);
        int64_t call17 = strlen(lexer_entry);
        if ((call16 > call17)) {
            array1 array0 = {lexer_str_20.m0};
            table1 table2 = {1, 1, array0.arr};
            lexer_table_append0(lexer_stack, &(table2));
            char* call18 = doodad_string_plus(lexer_posStr, "indent:");
            io_fPutStrLn(lexer_io, lexer_key, call18);
        }
        else {
            int64_t call19 = strlen(lexer_entry);
            int64_t call20 = strlen(lexer_str_20.m0);
            if ((call19 > call20)) {
                char* call21 = doodad_string_plus(lexer_posStr, "newline:");
                io_fPutStrLn(lexer_io, lexer_key, call21);
                
                for (; ; ) {
                    int64_t call22 = strlen(lexer_entry);
                    int64_t call23 = strlen(lexer_str_20.m0);
                    if (!(call22 > call23)) {
                        break;
                    }
                    char* call24 = doodad_string_plus(lexer_posStr, "dedent:");
                    io_fPutStrLn(lexer_io, lexer_key, call24);
                    lexer_pop(lexer_stack);
                    assert_assert(((*lexer_stack).len > 0));
                    lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
                }
                bool call25 = doodad_string_eqeq(lexer_str_20.m0, lexer_entry);
                assert_assert(call25);
            }
        }
    }
}

adt4 lexer_lex(lexer_PosString lexer_str_15, int64_t lexer_idx_9) {
    adt4 call26 = lexer_lexComment_1(lexer_str_15, lexer_idx_9);
    bool matchNull1 = (1 == call26.en);
    if (!matchNull1) {
        goto matchSkip1;
    }
    int64_t lexer_i = call26.u1;
    matchNull1 = true;
    matchSkip1:;
    if (matchNull1) {
        adt4 adt7 = {1};
        adt7.u1 = lexer_i;
        return adt7;
    }
    adt4 call27 = lexer_lexSpaceChar(lexer_str_15, lexer_idx_9);
    bool matchNull2 = (1 == call27.en);
    if (!matchNull2) {
        goto matchSkip2;
    }
    int64_t lexer_i_1 = call27.u1;
    matchNull2 = true;
    matchSkip2:;
    if (matchNull2) {
        adt4 adt8 = {1};
        adt8.u1 = lexer_i_1;
        return adt8;
    }
    adt4 adt9 = {0};
    return adt9;
}

lexer_Result lexer_lex_1(lexer_PosString lexer_str_19) {
    
    for (; ; ) {
        adt3 call28 = lexer_At(&(lexer_str_19), 0);
        bool matchNull3 = (1 == call28.en);
        if (!matchNull3) {
            goto matchSkip3;
        }
        char lexer_c_20 = call28.u1;
        matchNull3 = true;
        matchSkip3:;
        bool match1 = matchNull3;
        if (match1) {
            match1 = ((lexer_c_20 == ' ') || (lexer_c_20 == '\t'));
        }
        if (!match1) {
            break;
        }
        lexer_PosString call29 = lexer_Drop(lexer_str_19, 1);
        lexer_str_19.m0 = call29.m0;
        lexer_str_19.m1 = call29.m1;
    }
    lexer_Result call30 = lexer_lexNewline(lexer_str_19);
    bool match2 = (1 == call30.en);
    if (!match2) {
        goto skipMatch0;
    }
    match2 = false;
    lexer_Token lexer_token = call30.u1.m0;
    lexer_PosString lexer_rest_4 = call30.u1.m1;
    match2 = true;
    skipMatch0:;
    if (match2) {
        lexer_Result adt10 = {1};
        adt10.u1.m0 = lexer_token;
        adt10.u1.m1.m0 = lexer_rest_4.m0;
        adt10.u1.m1.m1 = lexer_rest_4.m1;
        return adt10;
    }
    lexer_Result call31 = lexer_lexImport(lexer_str_19);
    bool match3 = (1 == call31.en);
    if (!match3) {
        goto skipMatch1;
    }
    match3 = false;
    lexer_Token lexer_token_1 = call31.u1.m0;
    lexer_PosString lexer_rest_5 = call31.u1.m1;
    match3 = true;
    skipMatch1:;
    if (match3) {
        lexer_Result adt11 = {1};
        adt11.u1.m0 = lexer_token_1;
        adt11.u1.m1.m0 = lexer_rest_5.m0;
        adt11.u1.m1.m1 = lexer_rest_5.m1;
        return adt11;
    }
    lexer_Result call32 = lexer_lexInclude(lexer_str_19);
    bool match4 = (1 == call32.en);
    if (!match4) {
        goto skipMatch2;
    }
    match4 = false;
    lexer_Token lexer_token_2 = call32.u1.m0;
    lexer_PosString lexer_rest_6 = call32.u1.m1;
    match4 = true;
    skipMatch2:;
    if (match4) {
        lexer_Result adt12 = {1};
        adt12.u1.m0 = lexer_token_2;
        adt12.u1.m1.m0 = lexer_rest_6.m0;
        adt12.u1.m1.m1 = lexer_rest_6.m1;
        return adt12;
    }
    lexer_Result call33 = lexer_lexLink(lexer_str_19);
    bool match5 = (1 == call33.en);
    if (!match5) {
        goto skipMatch3;
    }
    match5 = false;
    lexer_Token lexer_token_3 = call33.u1.m0;
    lexer_PosString lexer_rest_7 = call33.u1.m1;
    match5 = true;
    skipMatch3:;
    if (match5) {
        lexer_Result adt13 = {1};
        adt13.u1.m0 = lexer_token_3;
        adt13.u1.m1.m0 = lexer_rest_7.m0;
        adt13.u1.m1.m1 = lexer_rest_7.m1;
        return adt13;
    }
    lexer_Result call34 = lexer_lexCharLiteral(lexer_str_19);
    bool match6 = (1 == call34.en);
    if (!match6) {
        goto skipMatch4;
    }
    match6 = false;
    lexer_Token lexer_token_4 = call34.u1.m0;
    lexer_PosString lexer_rest_8 = call34.u1.m1;
    match6 = true;
    skipMatch4:;
    if (match6) {
        lexer_Result adt14 = {1};
        adt14.u1.m0 = lexer_token_4;
        adt14.u1.m1.m0 = lexer_rest_8.m0;
        adt14.u1.m1.m1 = lexer_rest_8.m1;
        return adt14;
    }
    lexer_Result call35 = lexer_lexStringLiteral(lexer_str_19);
    bool match7 = (1 == call35.en);
    if (!match7) {
        goto skipMatch5;
    }
    match7 = false;
    lexer_Token lexer_token_5 = call35.u1.m0;
    lexer_PosString lexer_rest_9 = call35.u1.m1;
    match7 = true;
    skipMatch5:;
    if (match7) {
        lexer_Result adt15 = {1};
        adt15.u1.m0 = lexer_token_5;
        adt15.u1.m1.m0 = lexer_rest_9.m0;
        adt15.u1.m1.m1 = lexer_rest_9.m1;
        return adt15;
    }
    lexer_Result call36 = lexer_lexFloating(lexer_str_19);
    bool match8 = (1 == call36.en);
    if (!match8) {
        goto skipMatch6;
    }
    match8 = false;
    lexer_Token lexer_token_6 = call36.u1.m0;
    lexer_PosString lexer_rest_10 = call36.u1.m1;
    match8 = true;
    skipMatch6:;
    if (match8) {
        lexer_Result adt16 = {1};
        adt16.u1.m0 = lexer_token_6;
        adt16.u1.m1.m0 = lexer_rest_10.m0;
        adt16.u1.m1.m1 = lexer_rest_10.m1;
        return adt16;
    }
    lexer_Result call37 = lexer_lexInteger(lexer_str_19);
    bool match9 = (1 == call37.en);
    if (!match9) {
        goto skipMatch7;
    }
    match9 = false;
    lexer_Token lexer_token_7 = call37.u1.m0;
    lexer_PosString lexer_rest_11 = call37.u1.m1;
    match9 = true;
    skipMatch7:;
    if (match9) {
        lexer_Result adt17 = {1};
        adt17.u1.m0 = lexer_token_7;
        adt17.u1.m1.m0 = lexer_rest_11.m0;
        adt17.u1.m1.m1 = lexer_rest_11.m1;
        return adt17;
    }
    lexer_Result call38 = lexer_lexKeyword(lexer_str_19);
    bool match10 = (1 == call38.en);
    if (!match10) {
        goto skipMatch8;
    }
    match10 = false;
    lexer_Token lexer_token_8 = call38.u1.m0;
    lexer_PosString lexer_rest_12 = call38.u1.m1;
    match10 = true;
    skipMatch8:;
    if (match10) {
        lexer_Result adt18 = {1};
        adt18.u1.m0 = lexer_token_8;
        adt18.u1.m1.m0 = lexer_rest_12.m0;
        adt18.u1.m1.m1 = lexer_rest_12.m1;
        return adt18;
    }
    lexer_Result call39 = lexer_lexIdent(lexer_str_19);
    bool match11 = (1 == call39.en);
    if (!match11) {
        goto skipMatch9;
    }
    match11 = false;
    lexer_Token lexer_token_9 = call39.u1.m0;
    lexer_PosString lexer_rest_13 = call39.u1.m1;
    match11 = true;
    skipMatch9:;
    if (match11) {
        lexer_Result adt19 = {1};
        adt19.u1.m0 = lexer_token_9;
        adt19.u1.m1.m0 = lexer_rest_13.m0;
        adt19.u1.m1.m1 = lexer_rest_13.m1;
        return adt19;
    }
    lexer_Result call40 = lexer_lexComment(lexer_str_19);
    bool match12 = (1 == call40.en);
    if (!match12) {
        goto skipMatch10;
    }
    match12 = false;
    lexer_Token lexer_token_10 = call40.u1.m0;
    lexer_PosString lexer_rest_14 = call40.u1.m1;
    match12 = true;
    skipMatch10:;
    if (match12) {
        lexer_Result adt20 = {1};
        adt20.u1.m0 = lexer_token_10;
        adt20.u1.m1.m0 = lexer_rest_14.m0;
        adt20.u1.m1.m1 = lexer_rest_14.m1;
        return adt20;
    }
    lexer_Result call41 = lexer_lexSymbol(lexer_str_19);
    bool match13 = (1 == call41.en);
    if (!match13) {
        goto skipMatch11;
    }
    match13 = false;
    lexer_Token lexer_token_11 = call41.u1.m0;
    lexer_PosString lexer_rest_15 = call41.u1.m1;
    match13 = true;
    skipMatch11:;
    if (match13) {
        lexer_Result adt21 = {1};
        adt21.u1.m0 = lexer_token_11;
        adt21.u1.m1.m0 = lexer_rest_15.m0;
        adt21.u1.m1.m1 = lexer_rest_15.m1;
        return adt21;
    }
    lexer_Result call42 = lexer_lexCEmbed(lexer_str_19);
    bool match14 = (1 == call42.en);
    if (!match14) {
        goto skipMatch12;
    }
    match14 = false;
    lexer_Token lexer_token_12 = call42.u1.m0;
    lexer_PosString lexer_rest_16 = call42.u1.m1;
    match14 = true;
    skipMatch12:;
    if (match14) {
        lexer_Result adt22 = {1};
        adt22.u1.m0 = lexer_token_12;
        adt22.u1.m1.m0 = lexer_rest_16.m0;
        adt22.u1.m1.m1 = lexer_rest_16.m1;
        return adt22;
    }
    lexer_Result adt23 = {0};
    return adt23;
}

typedef struct { int64_t len; int64_t cap; char* r0; } table3;

void lexer_table_append1(table3* a, table3* b) {
    if (((a->len + b->len) >= a->cap)) {
        a->cap = ((a->len + b->len) * 2);
        void* mem0 = GC_malloc((a->cap * sizeof((*a->r0))));
        memcpy(mem0, a->r0, (a->len * sizeof((*a->r0))));
        a->r0 = mem0;
    }
    int64_t idx2 = 0;
    
    for (; (idx2 < b->len); idx2++) {
        a->r0[a->len] = b->r0[idx2];
        a->len++;
    }
}

typedef struct { char arr[1]; } array3;

lexer_Result lexer_lexCEmbed(lexer_PosString lexer_str_16) {
    adt3 call43 = lexer_At(&(lexer_str_16), 0);
    bool matchNull4 = (1 == call43.en);
    if (!matchNull4) {
        goto matchSkip4;
    }
    matchNull4 = (call43.u1 == '$');
    matchSkip4:;
    bool match15 = matchNull4;
    if (match15) {
        adt3 call44 = lexer_At(&(lexer_str_16), 1);
        bool matchNull5 = (1 == call44.en);
        if (!matchNull5) {
            goto matchSkip5;
        }
        matchNull5 = (call44.u1 == '{');
        matchSkip5:;
        match15 = matchNull5;
    }
    if (match15) {
    }
    else {
        lexer_Result adt24 = {0};
        return adt24;
    }
    lexer_PosString call45 = lexer_Drop(lexer_str_16, 1);
    lexer_str_16.m0 = call45.m0;
    lexer_str_16.m1 = call45.m1;
    int64_t lexer_idx_11 = 1;
    int64_t lexer_level = 1;
    
    for (; ; ) {
        adt3 call46 = lexer_At(&(lexer_str_16), lexer_idx_11);
        bool matchNull6 = (1 == call46.en);
        if (!matchNull6) {
            goto matchSkip6;
        }
        char lexer_c_14 = call46.u1;
        matchNull6 = true;
        matchSkip6:;
        bool match16 = matchNull6;
        if (match16) {
            match16 = (lexer_level > 0);
        }
        if (!match16) {
            break;
        }
        char switchExpr0 = lexer_c_14;
        switch(0) {
            case 0: {
                if ((switchExpr0 == '{')) {
                    lexer_level = (lexer_level + 1);
                    break;
                }
                if ((switchExpr0 == '}')) {
                    lexer_level = (lexer_level - 1);
                    break;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
        lexer_idx_11 = (lexer_idx_11 + 1);
    }
    if ((lexer_level != 0)) {
        lexer_Result adt25 = {0};
        return adt25;
    }
    table3 lexer_result = {0};
    int64_t idx1 = 0;
    bool first0 = true;
    
    for (; ; idx1++) {
        lexer_PosString call47 = lexer_Take(lexer_str_16, lexer_idx_11);
        if ((idx1 >= strlen(call47.m0))) {
            break;
        }
        char lexer_c_15 = call47.m0[idx1];
        char switchExpr1 = lexer_c_15;
        switch(0) {
            case 0: {
                if ((switchExpr1 == '\n')) {
                    array3 array2 = {(char)(31)};
                    table3 table4 = {1, 1, array2.arr};
                    lexer_table_append1(&(lexer_result), &(table4));
                    break;
                }
                if (true) {
                    array3 array4 = {lexer_c_15};
                    table3 table5 = {1, 1, array4.arr};
                    lexer_table_append1(&(lexer_result), &(table5));
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString lexer_str2 = lexer_str_16;
    char* call48 = doodad_string_copy(lexer_result.r0);
    lexer_str2.m0 = call48;
    lexer_Token adt26 = {8};
    adt26.u8.m0 = lexer_str2.m0;
    adt26.u8.m1 = lexer_str2.m1;
    lexer_PosString call49 = lexer_Drop(lexer_str_16, lexer_idx_11);
    lexer_Result adt27 = {1};
    adt27.u1.m0 = adt26;
    adt27.u1.m1.m0 = call49.m0;
    adt27.u1.m1.m1 = call49.m1;
    return adt27;
}

lexer_Result lexer_lexCharLiteral(lexer_PosString lexer_str_17) {
    lexer_PosString call50 = lexer_Take(lexer_str_17, 4);
    bool match17 = false;
    if ((4 != strlen(call50.m0))) {
        goto end0;
    }
    if ((call50.m0[0] != '\'')) {
        goto end0;
    }
    if ((call50.m0[1] != '\\')) {
        goto end0;
    }
    char lexer_c_16 = call50.m0[2];
    if ((call50.m0[3] != '\'')) {
        goto end0;
    }
    match17 = true;
    end0:;
    if (match17) {
        char switchExpr2 = lexer_c_16;
        switch(0) {
            case 0: {
                if ((switchExpr2 == 'n')) {
                    lexer_PosString call51 = lexer_Take(lexer_str_17, 4);
                    lexer_Token adt28 = {6};
                    adt28.u6.m0 = call51.m0;
                    adt28.u6.m1 = call51.m1;
                    lexer_PosString call52 = lexer_Drop(lexer_str_17, 4);
                    lexer_Result adt29 = {1};
                    adt29.u1.m0 = adt28;
                    adt29.u1.m1.m0 = call52.m0;
                    adt29.u1.m1.m1 = call52.m1;
                    return adt29;
                }
                if ((switchExpr2 == 't')) {
                    lexer_PosString call53 = lexer_Take(lexer_str_17, 4);
                    lexer_Token adt30 = {6};
                    adt30.u6.m0 = call53.m0;
                    adt30.u6.m1 = call53.m1;
                    lexer_PosString call54 = lexer_Drop(lexer_str_17, 4);
                    lexer_Result adt31 = {1};
                    adt31.u1.m0 = adt30;
                    adt31.u1.m1.m0 = call54.m0;
                    adt31.u1.m1.m1 = call54.m1;
                    return adt31;
                }
                if ((switchExpr2 == '0')) {
                    lexer_PosString call55 = lexer_Take(lexer_str_17, 4);
                    lexer_Token adt32 = {6};
                    adt32.u6.m0 = call55.m0;
                    adt32.u6.m1 = call55.m1;
                    lexer_PosString call56 = lexer_Drop(lexer_str_17, 4);
                    lexer_Result adt33 = {1};
                    adt33.u1.m0 = adt32;
                    adt33.u1.m1.m0 = call56.m0;
                    adt33.u1.m1.m1 = call56.m1;
                    return adt33;
                }
                if ((switchExpr2 == '\\')) {
                    lexer_PosString call57 = lexer_Take(lexer_str_17, 4);
                    lexer_Token adt34 = {6};
                    adt34.u6.m0 = call57.m0;
                    adt34.u6.m1 = call57.m1;
                    lexer_PosString call58 = lexer_Drop(lexer_str_17, 4);
                    lexer_Result adt35 = {1};
                    adt35.u1.m0 = adt34;
                    adt35.u1.m1.m0 = call58.m0;
                    adt35.u1.m1.m1 = call58.m1;
                    return adt35;
                }
                if ((switchExpr2 == '\'')) {
                    lexer_PosString call59 = lexer_Take(lexer_str_17, 4);
                    lexer_Token adt36 = {6};
                    adt36.u6.m0 = call59.m0;
                    adt36.u6.m1 = call59.m1;
                    lexer_PosString call60 = lexer_Drop(lexer_str_17, 4);
                    lexer_Result adt37 = {1};
                    adt37.u1.m0 = adt36;
                    adt37.u1.m1.m0 = call60.m0;
                    adt37.u1.m1.m1 = call60.m1;
                    return adt37;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString call61 = lexer_Take(lexer_str_17, 3);
    char* switchExpr3 = call61.m0;
    switch(0) {
        case 0: {
            bool match18 = false;
            if ((3 != strlen(switchExpr3))) {
                goto end1;
            }
            if ((switchExpr3[0] != '\'')) {
                goto end1;
            }
            if ((switchExpr3[1] != '\'')) {
                goto end1;
            }
            if ((switchExpr3[2] != '\'')) {
                goto end1;
            }
            match18 = true;
            end1:;
            if (match18) {
                break;
            }
            bool match19 = false;
            if ((3 != strlen(switchExpr3))) {
                goto end2;
            }
            if ((switchExpr3[0] != '\'')) {
                goto end2;
            }
            char lexer_c_17 = switchExpr3[1];
            if ((switchExpr3[2] != '\'')) {
                goto end2;
            }
            match19 = true;
            end2:;
            if (match19) {
                lexer_PosString call62 = lexer_Take(lexer_str_17, 3);
                lexer_Token adt38 = {6};
                adt38.u6.m0 = call62.m0;
                adt38.u6.m1 = call62.m1;
                lexer_PosString call63 = lexer_Drop(lexer_str_17, 3);
                lexer_Result adt39 = {1};
                adt39.u1.m0 = adt38;
                adt39.u1.m1.m0 = call63.m0;
                adt39.u1.m1.m1 = call63.m1;
                return adt39;
            }
            if (true) {
                break;
            }
            assert(false);
        }
    }
    lexer_Result adt40 = {0};
    return adt40;
}

lexer_Result lexer_lexComment(lexer_PosString lexer_str_7) {
    adt3 call64 = lexer_At(&(lexer_str_7), 0);
    bool matchNull7 = (1 == call64.en);
    if (!matchNull7) {
        goto matchSkip7;
    }
    matchNull7 = (call64.u1 == '/');
    matchSkip7:;
    bool match20 = matchNull7;
    if (match20) {
        adt3 call65 = lexer_At(&(lexer_str_7), 1);
        bool matchNull8 = (1 == call65.en);
        if (!matchNull8) {
            goto matchSkip8;
        }
        matchNull8 = (call65.u1 == '/');
        matchSkip8:;
        match20 = matchNull8;
    }
    if (match20) {
        
        for (; ; ) {
            adt3 call66 = lexer_At(&(lexer_str_7), 0);
            bool matchNull9 = (1 == call66.en);
            if (!matchNull9) {
                goto matchSkip9;
            }
            char lexer_c_6 = call66.u1;
            matchNull9 = true;
            matchSkip9:;
            bool match21 = matchNull9;
            if (match21) {
                match21 = (lexer_c_6 != '\n');
            }
            if (!match21) {
                break;
            }
            lexer_PosString call67 = lexer_Drop(lexer_str_7, 1);
            lexer_str_7.m0 = call67.m0;
            lexer_str_7.m1 = call67.m1;
        }
        lexer_Token adt41 = {12};
        lexer_Result adt42 = {1};
        adt42.u1.m0 = adt41;
        adt42.u1.m1.m0 = lexer_str_7.m0;
        adt42.u1.m1.m1 = lexer_str_7.m1;
        return adt42;
    }
    lexer_Result adt43 = {0};
    return adt43;
}

adt4 lexer_lexComment_1(lexer_PosString lexer_str_13, int64_t lexer_idx_7) {
    adt3 call68 = lexer_At(&(lexer_str_13), lexer_idx_7);
    bool matchNull10 = (1 == call68.en);
    if (!matchNull10) {
        goto matchSkip10;
    }
    matchNull10 = (call68.u1 == '/');
    matchSkip10:;
    bool match22 = matchNull10;
    if (match22) {
        adt3 call69 = lexer_At(&(lexer_str_13), (lexer_idx_7 + 1));
        bool matchNull11 = (1 == call69.en);
        if (!matchNull11) {
            goto matchSkip11;
        }
        matchNull11 = (call69.u1 == '/');
        matchSkip11:;
        match22 = matchNull11;
    }
    if (match22) {
        lexer_idx_7 = (lexer_idx_7 + 2);
    }
    else {
        adt4 adt44 = {0};
        return adt44;
    }
    
    for (; ; ) {
        adt3 call70 = lexer_At(&(lexer_str_13), lexer_idx_7);
        bool matchNull12 = (1 == call70.en);
        if (!matchNull12) {
            goto matchSkip12;
        }
        char lexer_c_12 = call70.u1;
        matchNull12 = true;
        matchSkip12:;
        bool match23 = matchNull12;
        if (match23) {
            match23 = (lexer_c_12 != '\n');
        }
        if (!match23) {
            break;
        }
        lexer_idx_7 = (lexer_idx_7 + 1);
    }
    adt4 adt45 = {1};
    adt45.u1 = lexer_idx_7;
    return adt45;
}

void lexer_lexFile(char* lexer_fileNameIn, char* lexer_fileNameOut) {
    io_Io lexer_io_1 = {0};
    io_FileKey call71 = io_openFile(&(lexer_io_1), lexer_fileNameOut);
    io_FileKey lexer_key_1 = call71;
    char* call72 = io_readFile(&(lexer_io_1), lexer_fileNameIn);
    lexer_Pos tuple6 = {1, 1};
    lexer_PosString tuple7 = {call72, tuple6};
    lexer_PosString lexer_str_21 = tuple7;
    table1 lexer_indentStack = {0};
    array1 array5 = {""};
    table1 table6 = {1, 1, array5.arr};
    lexer_table_append0(&(lexer_indentStack), &(table6));
    
    for (; ; ) {
        lexer_Result call73 = lexer_lex_1(lexer_str_21);
        bool match24 = (1 == call73.en);
        if (!match24) {
            goto skipMatch13;
        }
        match24 = false;
        lexer_Token lexer_token_13 = call73.u1.m0;
        lexer_PosString lexer_rest_17 = call73.u1.m1;
        match24 = true;
        skipMatch13:;
        if (!match24) {
            break;
        }
        lexer_Token switchExpr4 = lexer_token_13;
        switch(0) {
            case 0: {
                bool match25 = (0 == switchExpr4.en);
                if (!match25) {
                    goto skipMatch14;
                }
                match25 = false;
                lexer_PosString lexer_s_1 = switchExpr4.u0;
                match25 = true;
                skipMatch14:;
                if (match25) {
                    char* call74 = doodad_string_i64(lexer_s_1.m1.m0);
                    char* call75 = doodad_string_plus(call74, ":");
                    char* call76 = doodad_string_i64(lexer_s_1.m1.m1);
                    char* call77 = doodad_string_plus(call75, call76);
                    char* call78 = doodad_string_plus(call77, ":ident: ");
                    char* call79 = doodad_string_plus(call78, lexer_s_1.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call79);
                    break;
                }
                bool match26 = (1 == switchExpr4.en);
                if (!match26) {
                    goto skipMatch15;
                }
                match26 = false;
                lexer_PosString lexer_s_2 = switchExpr4.u1;
                match26 = true;
                skipMatch15:;
                if (match26) {
                    char* call80 = doodad_string_i64(lexer_s_2.m1.m0);
                    char* call81 = doodad_string_plus(call80, ":");
                    char* call82 = doodad_string_i64(lexer_s_2.m1.m1);
                    char* call83 = doodad_string_plus(call81, call82);
                    char* call84 = doodad_string_plus(call83, ":keyword: ");
                    char* call85 = doodad_string_plus(call84, lexer_s_2.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call85);
                    break;
                }
                bool match27 = (2 == switchExpr4.en);
                if (!match27) {
                    goto skipMatch16;
                }
                match27 = false;
                lexer_PosString lexer_s_3 = switchExpr4.u2;
                match27 = true;
                skipMatch16:;
                if (match27) {
                    char* call86 = doodad_string_i64(lexer_s_3.m1.m0);
                    char* call87 = doodad_string_plus(call86, ":");
                    char* call88 = doodad_string_i64(lexer_s_3.m1.m1);
                    char* call89 = doodad_string_plus(call87, call88);
                    char* call90 = doodad_string_plus(call89, ":integer: ");
                    char* call91 = doodad_string_plus(call90, lexer_s_3.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call91);
                    break;
                }
                bool match28 = (3 == switchExpr4.en);
                if (!match28) {
                    goto skipMatch17;
                }
                match28 = false;
                lexer_PosString lexer_s_4 = switchExpr4.u3;
                match28 = true;
                skipMatch17:;
                if (match28) {
                    char* call92 = doodad_string_i64(lexer_s_4.m1.m0);
                    char* call93 = doodad_string_plus(call92, ":");
                    char* call94 = doodad_string_i64(lexer_s_4.m1.m1);
                    char* call95 = doodad_string_plus(call93, call94);
                    char* call96 = doodad_string_plus(call95, ":floating: ");
                    char* call97 = doodad_string_plus(call96, lexer_s_4.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call97);
                    break;
                }
                bool match29 = (5 == switchExpr4.en);
                if (!match29) {
                    goto skipMatch18;
                }
                match29 = false;
                lexer_PosString lexer_s_5 = switchExpr4.u5;
                match29 = true;
                skipMatch18:;
                if (match29) {
                    char* call98 = doodad_string_i64(lexer_s_5.m1.m0);
                    char* call99 = doodad_string_plus(call98, ":");
                    char* call100 = doodad_string_i64(lexer_s_5.m1.m1);
                    char* call101 = doodad_string_plus(call99, call100);
                    char* call102 = doodad_string_plus(call101, ":symbol: ");
                    char* call103 = doodad_string_plus(call102, lexer_s_5.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call103);
                    break;
                }
                bool match30 = (6 == switchExpr4.en);
                if (!match30) {
                    goto skipMatch19;
                }
                match30 = false;
                lexer_PosString lexer_s_6 = switchExpr4.u6;
                match30 = true;
                skipMatch19:;
                if (match30) {
                    char* call104 = doodad_string_i64(lexer_s_6.m1.m0);
                    char* call105 = doodad_string_plus(call104, ":");
                    char* call106 = doodad_string_i64(lexer_s_6.m1.m1);
                    char* call107 = doodad_string_plus(call105, call106);
                    char* call108 = doodad_string_plus(call107, ":char: ");
                    char* call109 = doodad_string_plus(call108, lexer_s_6.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call109);
                    break;
                }
                bool match31 = (7 == switchExpr4.en);
                if (!match31) {
                    goto skipMatch20;
                }
                match31 = false;
                lexer_PosString lexer_s_7 = switchExpr4.u7;
                match31 = true;
                skipMatch20:;
                if (match31) {
                    char* call110 = doodad_string_i64(lexer_s_7.m1.m0);
                    char* call111 = doodad_string_plus(call110, ":");
                    char* call112 = doodad_string_i64(lexer_s_7.m1.m1);
                    char* call113 = doodad_string_plus(call111, call112);
                    char* call114 = doodad_string_plus(call113, ":string: ");
                    char* call115 = doodad_string_plus(call114, lexer_s_7.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call115);
                    break;
                }
                bool match32 = (8 == switchExpr4.en);
                if (!match32) {
                    goto skipMatch21;
                }
                match32 = false;
                lexer_PosString lexer_s_8 = switchExpr4.u8;
                match32 = true;
                skipMatch21:;
                if (match32) {
                    char* call116 = doodad_string_i64(lexer_s_8.m1.m0);
                    char* call117 = doodad_string_plus(call116, ":");
                    char* call118 = doodad_string_i64(lexer_s_8.m1.m1);
                    char* call119 = doodad_string_plus(call117, call118);
                    char* call120 = doodad_string_plus(call119, ":cembed: ");
                    char* call121 = doodad_string_plus(call120, lexer_s_8.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call121);
                    break;
                }
                bool match33 = (9 == switchExpr4.en);
                if (!match33) {
                    goto skipMatch22;
                }
                match33 = false;
                lexer_PosString lexer_s_9 = switchExpr4.u9;
                match33 = true;
                skipMatch22:;
                if (match33) {
                    char* call122 = doodad_string_i64(lexer_s_9.m1.m0);
                    char* call123 = doodad_string_plus(call122, ":");
                    char* call124 = doodad_string_i64(lexer_s_9.m1.m1);
                    char* call125 = doodad_string_plus(call123, call124);
                    char* call126 = doodad_string_plus(call125, ":import: ");
                    char* call127 = doodad_string_plus(call126, lexer_s_9.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call127);
                    break;
                }
                bool match34 = (11 == switchExpr4.en);
                if (!match34) {
                    goto skipMatch23;
                }
                match34 = false;
                lexer_PosString lexer_s_10 = switchExpr4.u11;
                match34 = true;
                skipMatch23:;
                if (match34) {
                    char* call128 = doodad_string_i64(lexer_s_10.m1.m0);
                    char* call129 = doodad_string_plus(call128, ":");
                    char* call130 = doodad_string_i64(lexer_s_10.m1.m1);
                    char* call131 = doodad_string_plus(call129, call130);
                    char* call132 = doodad_string_plus(call131, ":link:");
                    char* call133 = doodad_string_plus(call132, lexer_s_10.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call133);
                    break;
                }
                bool match35 = (10 == switchExpr4.en);
                if (!match35) {
                    goto skipMatch24;
                }
                match35 = false;
                lexer_PosString lexer_s_11 = switchExpr4.u10;
                match35 = true;
                skipMatch24:;
                if (match35) {
                    char* call134 = doodad_string_i64(lexer_s_11.m1.m0);
                    char* call135 = doodad_string_plus(call134, ":");
                    char* call136 = doodad_string_i64(lexer_s_11.m1.m1);
                    char* call137 = doodad_string_plus(call135, call136);
                    char* call138 = doodad_string_plus(call137, ":include: ");
                    char* call139 = doodad_string_plus(call138, lexer_s_11.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call139);
                    break;
                }
                bool match36 = (4 == switchExpr4.en);
                if (!match36) {
                    goto skipMatch25;
                }
                match36 = false;
                lexer_PosString lexer_s_12 = switchExpr4.u4;
                match36 = true;
                skipMatch25:;
                if (match36) {
                    lexer_indent(&(lexer_io_1), &(lexer_indentStack), lexer_key_1, lexer_s_12);
                    break;
                }
                bool match37 = (12 == switchExpr4.en);
                if (!match37) {
                    goto skipMatch26;
                }
                match37 = false;
                match37 = true;
                skipMatch26:;
                if (match37) {
                    break;
                }
                assert(false);
            }
        }
        lexer_str_21.m0 = lexer_rest_17.m0;
        lexer_str_21.m1 = lexer_rest_17.m1;
    }
    io_closeFile(&(lexer_io_1), lexer_key_1);
}

lexer_Result lexer_lexFloating(lexer_PosString lexer_str_11) {
    adt3 call140 = lexer_At(&(lexer_str_11), 0);
    bool matchNull13 = (1 == call140.en);
    if (!matchNull13) {
        goto matchSkip13;
    }
    char lexer_c_10 = call140.u1;
    matchNull13 = true;
    matchSkip13:;
    bool match38 = matchNull13;
    if (match38) {
        bool call141 = strings_isDigit(lexer_c_10);
        match38 = call141;
    }
    if (match38) {
    }
    else {
        lexer_Result adt46 = {0};
        return adt46;
    }
    int64_t lexer_idx_6 = 0;
    bool lexer_dot = false;
    
    for (; ; ) {
        adt3 call142 = lexer_At(&(lexer_str_11), lexer_idx_6);
        bool matchNull14 = (1 == call142.en);
        if (!matchNull14) {
            goto matchSkip14;
        }
        char lexer_c_11 = call142.u1;
        matchNull14 = true;
        matchSkip14:;
        bool match39 = matchNull14;
        if (match39) {
            bool call143 = strings_isDigit(lexer_c_11);
            match39 = (call143 || ((lexer_c_11 == '.') && !lexer_dot));
        }
        if (!match39) {
            break;
        }
        lexer_idx_6 = (lexer_idx_6 + 1);
        if ((lexer_c_11 == '.')) {
            lexer_dot = true;
        }
    }
    adt3 call144 = lexer_At(&(lexer_str_11), lexer_idx_6);
    bool matchNull15 = (1 == call144.en);
    if (!matchNull15) {
        goto matchSkip15;
    }
    matchNull15 = (call144.u1 == '.');
    matchSkip15:;
    if (matchNull15) {
        lexer_Result adt47 = {0};
        return adt47;
    }
    if (lexer_dot) {
        lexer_PosString call145 = lexer_Take(lexer_str_11, lexer_idx_6);
        lexer_Token adt48 = {3};
        adt48.u3.m0 = call145.m0;
        adt48.u3.m1 = call145.m1;
        lexer_PosString call146 = lexer_Drop(lexer_str_11, lexer_idx_6);
        lexer_Result adt49 = {1};
        adt49.u1.m0 = adt48;
        adt49.u1.m1.m0 = call146.m0;
        adt49.u1.m1.m1 = call146.m1;
        return adt49;
    }
    lexer_Result adt50 = {0};
    return adt50;
}

lexer_Result lexer_lexIdent(lexer_PosString lexer_str_3) {
    adt3 call147 = lexer_At(&(lexer_str_3), 0);
    bool matchNull16 = (1 == call147.en);
    if (!matchNull16) {
        goto matchSkip16;
    }
    char lexer_c_1 = call147.u1;
    matchNull16 = true;
    matchSkip16:;
    bool match40 = matchNull16;
    if (match40) {
        bool call148 = strings_isAlpha(lexer_c_1);
        match40 = call148;
    }
    if (match40) {
        int64_t lexer_idx_1 = 0;
        
        for (; ; ) {
            adt3 call149 = lexer_At(&(lexer_str_3), lexer_idx_1);
            bool matchNull17 = (1 == call149.en);
            if (!matchNull17) {
                goto matchSkip17;
            }
            char lexer_c_2 = call149.u1;
            matchNull17 = true;
            matchSkip17:;
            bool match41 = matchNull17;
            if (match41) {
                bool call150 = strings_isAlpha(lexer_c_2);
                bool call151 = strings_isDigit(lexer_c_2);
                match41 = ((call150 || call151) || (lexer_c_2 == '_'));
            }
            if (!match41) {
                break;
            }
            lexer_idx_1 = (lexer_idx_1 + 1);
        }
        lexer_PosString call152 = lexer_Take(lexer_str_3, lexer_idx_1);
        lexer_Token adt51 = {0};
        adt51.u0.m0 = call152.m0;
        adt51.u0.m1 = call152.m1;
        lexer_PosString call153 = lexer_Drop(lexer_str_3, lexer_idx_1);
        lexer_Result adt52 = {1};
        adt52.u1.m0 = adt51;
        adt52.u1.m1.m0 = call153.m0;
        adt52.u1.m1.m1 = call153.m1;
        return adt52;
    }
    lexer_Result adt53 = {0};
    return adt53;
}

lexer_Result lexer_lexImport(lexer_PosString lexer_str_4) {
    lexer_Result call154 = lexer_lexIdent(lexer_str_4);
    bool match42 = (1 == call154.en);
    if (!match42) {
        goto skipMatch27;
    }
    match42 = false;
    bool match43 = (0 == call154.u1.m0.en);
    if (!match43) {
        goto skipMatch28;
    }
    match43 = false;
    lexer_PosString lexer_ps = call154.u1.m0.u0;
    match43 = true;
    skipMatch28:;
    if (!match43) {
        goto skipMatch27;
    }
    lexer_PosString lexer_rest = call154.u1.m1;
    match42 = true;
    skipMatch27:;
    bool match44 = match42;
    if (match44) {
        bool call155 = doodad_string_eqeq(lexer_ps.m0, "import");
        match44 = call155;
    }
    if (match44) {
        lexer_str_4.m0 = lexer_rest.m0;
        lexer_str_4.m1 = lexer_rest.m1;
    }
    else {
        lexer_Result adt54 = {0};
        return adt54;
    }
    int64_t lexer_idx_2 = 0;
    
    for (; ; ) {
        adt3 call156 = lexer_At(&(lexer_str_4), lexer_idx_2);
        bool matchNull18 = (1 == call156.en);
        if (!matchNull18) {
            goto matchSkip18;
        }
        char lexer_c_3 = call156.u1;
        matchNull18 = true;
        matchSkip18:;
        bool match45 = matchNull18;
        if (match45) {
            match45 = (lexer_c_3 != '\n');
        }
        if (!match45) {
            break;
        }
        lexer_idx_2 = (lexer_idx_2 + 1);
    }
    lexer_PosString call157 = lexer_Take(lexer_str_4, lexer_idx_2);
    lexer_Token adt55 = {9};
    adt55.u9.m0 = call157.m0;
    adt55.u9.m1 = call157.m1;
    lexer_PosString call158 = lexer_Drop(lexer_str_4, lexer_idx_2);
    lexer_Result adt56 = {1};
    adt56.u1.m0 = adt55;
    adt56.u1.m1.m0 = call158.m0;
    adt56.u1.m1.m1 = call158.m1;
    return adt56;
}

lexer_Result lexer_lexInclude(lexer_PosString lexer_str_5) {
    lexer_Result call159 = lexer_lexIdent(lexer_str_5);
    bool match46 = (1 == call159.en);
    if (!match46) {
        goto skipMatch29;
    }
    match46 = false;
    bool match47 = (0 == call159.u1.m0.en);
    if (!match47) {
        goto skipMatch30;
    }
    match47 = false;
    lexer_PosString lexer_ps_1 = call159.u1.m0.u0;
    match47 = true;
    skipMatch30:;
    if (!match47) {
        goto skipMatch29;
    }
    lexer_PosString lexer_rest_1 = call159.u1.m1;
    match46 = true;
    skipMatch29:;
    bool match48 = match46;
    if (match48) {
        bool call160 = doodad_string_eqeq(lexer_ps_1.m0, "include");
        match48 = call160;
    }
    if (match48) {
        lexer_str_5.m0 = lexer_rest_1.m0;
        lexer_str_5.m1 = lexer_rest_1.m1;
    }
    else {
        lexer_Result adt57 = {0};
        return adt57;
    }
    int64_t lexer_idx_3 = 0;
    
    for (; ; ) {
        adt3 call161 = lexer_At(&(lexer_str_5), lexer_idx_3);
        bool matchNull19 = (1 == call161.en);
        if (!matchNull19) {
            goto matchSkip19;
        }
        char lexer_c_4 = call161.u1;
        matchNull19 = true;
        matchSkip19:;
        bool match49 = matchNull19;
        if (match49) {
            match49 = (lexer_c_4 != '\n');
        }
        if (!match49) {
            break;
        }
        lexer_idx_3 = (lexer_idx_3 + 1);
    }
    lexer_PosString call162 = lexer_Take(lexer_str_5, lexer_idx_3);
    lexer_Token adt58 = {10};
    adt58.u10.m0 = call162.m0;
    adt58.u10.m1 = call162.m1;
    lexer_PosString call163 = lexer_Drop(lexer_str_5, lexer_idx_3);
    lexer_Result adt59 = {1};
    adt59.u1.m0 = adt58;
    adt59.u1.m1.m0 = call163.m0;
    adt59.u1.m1.m1 = call163.m1;
    return adt59;
}

lexer_Result lexer_lexInteger(lexer_PosString lexer_str_10) {
    adt3 call164 = lexer_At(&(lexer_str_10), 0);
    bool matchNull20 = (1 == call164.en);
    if (!matchNull20) {
        goto matchSkip20;
    }
    char lexer_c_8 = call164.u1;
    matchNull20 = true;
    matchSkip20:;
    bool match50 = matchNull20;
    if (match50) {
        bool call165 = strings_isDigit(lexer_c_8);
        match50 = call165;
    }
    if (match50) {
        int64_t lexer_idx_5 = 0;
        
        for (; ; ) {
            adt3 call166 = lexer_At(&(lexer_str_10), lexer_idx_5);
            bool matchNull21 = (1 == call166.en);
            if (!matchNull21) {
                goto matchSkip21;
            }
            char lexer_c_9 = call166.u1;
            matchNull21 = true;
            matchSkip21:;
            bool match51 = matchNull21;
            if (match51) {
                bool call167 = strings_isDigit(lexer_c_9);
                match51 = call167;
            }
            if (!match51) {
                break;
            }
            lexer_idx_5 = (lexer_idx_5 + 1);
        }
        lexer_PosString call168 = lexer_Take(lexer_str_10, lexer_idx_5);
        lexer_Token adt60 = {2};
        adt60.u2.m0 = call168.m0;
        adt60.u2.m1 = call168.m1;
        lexer_PosString call169 = lexer_Drop(lexer_str_10, lexer_idx_5);
        lexer_Result adt61 = {1};
        adt61.u1.m0 = adt60;
        adt61.u1.m1.m0 = call169.m0;
        adt61.u1.m1.m1 = call169.m1;
        return adt61;
    }
    lexer_Result adt62 = {0};
    return adt62;
}

typedef struct { char* arr[27]; } array7;

lexer_Result lexer_lexKeyword(lexer_PosString lexer_str_8) {
    array7 array6 = {"module", "type", "fn", "for", "if", "else", "switch", "while", "return", "data", "const", "let", "null", "true", "false", "string", "bool", "char", "i8", "i16", "i32", "i64", "u8", "f32", "f64", "table", "tuple"};
    array7 lexer_keywords = array6;
    lexer_Result call170 = lexer_lexIdent(lexer_str_8);
    bool match52 = (1 == call170.en);
    if (!match52) {
        goto skipMatch31;
    }
    match52 = false;
    bool match53 = (0 == call170.u1.m0.en);
    if (!match53) {
        goto skipMatch32;
    }
    match53 = false;
    lexer_PosString lexer_ps_3 = call170.u1.m0.u0;
    match53 = true;
    skipMatch32:;
    if (!match53) {
        goto skipMatch31;
    }
    lexer_PosString lexer_rest_3 = call170.u1.m1;
    match52 = true;
    skipMatch31:;
    if (match52) {
        int64_t idx3 = 0;
        bool first1 = true;
        
        for (; ; idx3++) {
            if ((idx3 >= 27)) {
                break;
            }
            char* lexer_keyword = lexer_keywords.arr[idx3];
            bool call171 = doodad_string_eqeq(lexer_keyword, lexer_ps_3.m0);
            if (call171) {
                lexer_Token adt63 = {1};
                adt63.u1.m0 = lexer_ps_3.m0;
                adt63.u1.m1 = lexer_ps_3.m1;
                lexer_Result adt64 = {1};
                adt64.u1.m0 = adt63;
                adt64.u1.m1.m0 = lexer_rest_3.m0;
                adt64.u1.m1.m1 = lexer_rest_3.m1;
                return adt64;
            }
        }
    }
    lexer_Result adt65 = {0};
    return adt65;
}

lexer_Result lexer_lexLink(lexer_PosString lexer_str_6) {
    lexer_Result call172 = lexer_lexIdent(lexer_str_6);
    bool match54 = (1 == call172.en);
    if (!match54) {
        goto skipMatch33;
    }
    match54 = false;
    bool match55 = (0 == call172.u1.m0.en);
    if (!match55) {
        goto skipMatch34;
    }
    match55 = false;
    lexer_PosString lexer_ps_2 = call172.u1.m0.u0;
    match55 = true;
    skipMatch34:;
    if (!match55) {
        goto skipMatch33;
    }
    lexer_PosString lexer_rest_2 = call172.u1.m1;
    match54 = true;
    skipMatch33:;
    bool match56 = match54;
    if (match56) {
        bool call173 = doodad_string_eqeq(lexer_ps_2.m0, "link");
        match56 = call173;
    }
    if (match56) {
        lexer_str_6.m0 = lexer_rest_2.m0;
        lexer_str_6.m1 = lexer_rest_2.m1;
    }
    else {
        lexer_Result adt66 = {0};
        return adt66;
    }
    int64_t lexer_idx_4 = 0;
    
    for (; ; ) {
        adt3 call174 = lexer_At(&(lexer_str_6), lexer_idx_4);
        bool matchNull22 = (1 == call174.en);
        if (!matchNull22) {
            goto matchSkip22;
        }
        char lexer_c_5 = call174.u1;
        matchNull22 = true;
        matchSkip22:;
        bool match57 = matchNull22;
        if (match57) {
            match57 = (lexer_c_5 != '\n');
        }
        if (!match57) {
            break;
        }
        lexer_idx_4 = (lexer_idx_4 + 1);
    }
    lexer_PosString call175 = lexer_Take(lexer_str_6, lexer_idx_4);
    lexer_Token adt67 = {11};
    adt67.u11.m0 = call175.m0;
    adt67.u11.m1 = call175.m1;
    lexer_PosString call176 = lexer_Drop(lexer_str_6, lexer_idx_4);
    lexer_Result adt68 = {1};
    adt68.u1.m0 = adt67;
    adt68.u1.m1.m0 = call176.m0;
    adt68.u1.m1.m1 = call176.m1;
    return adt68;
}

typedef struct { int64_t min; int64_t max; } range1;

lexer_Result lexer_lexNewline(lexer_PosString lexer_str_12) {
    adt3 call177 = lexer_At(&(lexer_str_12), 0);
    bool matchNull23 = (1 == call177.en);
    if (!matchNull23) {
        goto matchSkip23;
    }
    matchNull23 = (call177.u1 == '\n');
    matchSkip23:;
    if (matchNull23) {
    }
    else {
        lexer_Result adt69 = {0};
        return adt69;
    }
    int64_t lexer_idx_10 = 0;
    
    for (; ; ) {
        adt4 call178 = lexer_lex(lexer_str_12, lexer_idx_10);
        bool matchNull24 = (1 == call178.en);
        if (!matchNull24) {
            goto matchSkip24;
        }
        int64_t lexer_i_2 = call178.u1;
        matchNull24 = true;
        matchSkip24:;
        if (!matchNull24) {
            break;
        }
        lexer_idx_10 = lexer_i_2;
    }
    lexer_PosString call179 = lexer_Take(lexer_str_12, lexer_idx_10);
    lexer_PosString lexer_spaces = call179;
    int64_t lexer_lastNewline = (0 - 1);
    int64_t idx4 = 0;
    bool first2 = true;
    
    for (; ; idx4++) {
        range1 range0 = {0, strlen(lexer_spaces.m0)};
        if (first2) {
            idx4 = range0.min;
            first2 = false;
        }
        if ((idx4 >= range0.max)) {
            break;
        }
        int64_t lexer_i_3 = idx4;
        if ((lexer_spaces.m0[lexer_i_3] == '\n')) {
            lexer_lastNewline = lexer_i_3;
        }
    }
    assert_assert((lexer_lastNewline > (0 - 1)));
    lexer_PosString call180 = lexer_Drop(lexer_spaces, (lexer_lastNewline + 1));
    lexer_Token adt70 = {4};
    adt70.u4.m0 = call180.m0;
    adt70.u4.m1 = call180.m1;
    lexer_PosString call181 = lexer_Drop(lexer_str_12, lexer_idx_10);
    lexer_Result adt71 = {1};
    adt71.u1.m0 = adt70;
    adt71.u1.m1.m0 = call181.m0;
    adt71.u1.m1.m1 = call181.m1;
    return adt71;
}

adt4 lexer_lexSpaceChar(lexer_PosString lexer_str_14, int64_t lexer_idx_8) {
    adt3 call182 = lexer_At(&(lexer_str_14), lexer_idx_8);
    bool matchNull25 = (1 == call182.en);
    if (!matchNull25) {
        goto matchSkip25;
    }
    char lexer_c_13 = call182.u1;
    matchNull25 = true;
    matchSkip25:;
    bool match58 = matchNull25;
    if (match58) {
        match58 = (((lexer_c_13 == '\n') || (lexer_c_13 == '\t')) || (lexer_c_13 == ' '));
    }
    if (match58) {
        adt4 adt72 = {1};
        adt72.u1 = (lexer_idx_8 + 1);
        return adt72;
    }
    adt4 adt73 = {0};
    return adt73;
}

lexer_Result lexer_lexStringLiteral(lexer_PosString lexer_str_18) {
    adt3 call183 = lexer_At(&(lexer_str_18), 0);
    bool matchNull26 = (1 == call183.en);
    if (!matchNull26) {
        goto matchSkip26;
    }
    matchNull26 = (call183.u1 == '"');
    matchSkip26:;
    if (matchNull26) {
        int64_t lexer_idx_12 = 1;
        
        for (; ; ) {
            adt3 call184 = lexer_At(&(lexer_str_18), lexer_idx_12);
            bool matchNull27 = (1 == call184.en);
            if (!matchNull27) {
                goto matchSkip27;
            }
            char lexer_c_18 = call184.u1;
            matchNull27 = true;
            matchSkip27:;
            bool match59 = matchNull27;
            if (match59) {
                match59 = ((lexer_c_18 != '"') && (lexer_c_18 != '\n'));
            }
            if (!match59) {
                break;
            }
            lexer_idx_12 = (lexer_idx_12 + 1);
        }
        adt3 call185 = lexer_At(&(lexer_str_18), lexer_idx_12);
        adt3 switchExpr5 = call185;
        switch(0) {
            case 0: {
                bool matchNull28 = (0 == switchExpr5.en);
                if (matchNull28) {
                    lexer_Result adt74 = {0};
                    return adt74;
                }
                bool matchNull29 = (1 == switchExpr5.en);
                if (!matchNull29) {
                    goto matchSkip28;
                }
                char lexer_c_19 = switchExpr5.u1;
                matchNull29 = true;
                matchSkip28:;
                bool match60 = matchNull29;
                if (match60) {
                    match60 = (lexer_c_19 != '"');
                }
                if (match60) {
                    lexer_Result adt75 = {0};
                    return adt75;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
        lexer_PosString call186 = lexer_Take(lexer_str_18, lexer_idx_12);
        lexer_PosString call187 = lexer_Drop(call186, 1);
        lexer_Token adt76 = {7};
        adt76.u7.m0 = call187.m0;
        adt76.u7.m1 = call187.m1;
        lexer_PosString call188 = lexer_Drop(lexer_str_18, (lexer_idx_12 + 1));
        lexer_Result adt77 = {1};
        adt77.u1.m0 = adt76;
        adt77.u1.m1.m0 = call188.m0;
        adt77.u1.m1.m1 = call188.m1;
        return adt77;
    }
    lexer_Result adt78 = {0};
    return adt78;
}

typedef struct { char* arr[10]; } array9;

lexer_Result lexer_lexSymbol(lexer_PosString lexer_str_9) {
    array9 array8 = {"..", "->", "==", "!=", "<=", ">=", "+=", "||", "&&", "::"};
    array9 lexer_symbols = array8;
    char* lexer_singles = "[]{}()<>|.,=+-*/%_:;!";
    int64_t idx5 = 0;
    bool first3 = true;
    
    for (; ; idx5++) {
        if ((idx5 >= 10)) {
            break;
        }
        char* lexer_symbol = lexer_symbols.arr[idx5];
        int64_t call189 = strlen(lexer_symbol);
        lexer_PosString call190 = lexer_Take(lexer_str_9, call189);
        lexer_PosString lexer_x = call190;
        bool call191 = doodad_string_eqeq(lexer_x.m0, lexer_symbol);
        if (call191) {
            lexer_Token adt79 = {5};
            adt79.u5.m0 = lexer_x.m0;
            adt79.u5.m1 = lexer_x.m1;
            int64_t call192 = strlen(lexer_symbol);
            lexer_PosString call193 = lexer_Drop(lexer_str_9, call192);
            lexer_Result adt80 = {1};
            adt80.u1.m0 = adt79;
            adt80.u1.m1.m0 = call193.m0;
            adt80.u1.m1.m1 = call193.m1;
            return adt80;
        }
    }
    int64_t idx6 = 0;
    bool first4 = true;
    
    for (; ; idx6++) {
        if ((idx6 >= strlen(lexer_singles))) {
            break;
        }
        char lexer_symbol_1 = lexer_singles[idx6];
        adt3 call194 = lexer_At(&(lexer_str_9), 0);
        bool matchNull30 = (1 == call194.en);
        if (!matchNull30) {
            goto matchSkip29;
        }
        char lexer_c_7 = call194.u1;
        matchNull30 = true;
        matchSkip29:;
        bool match61 = matchNull30;
        if (match61) {
            match61 = (lexer_c_7 == lexer_symbol_1);
        }
        if (match61) {
            lexer_PosString call195 = lexer_Take(lexer_str_9, 1);
            lexer_Token adt81 = {5};
            adt81.u5.m0 = call195.m0;
            adt81.u5.m1 = call195.m1;
            lexer_PosString call196 = lexer_Drop(lexer_str_9, 1);
            lexer_Result adt82 = {1};
            adt82.u1.m0 = adt81;
            adt82.u1.m1.m0 = call196.m0;
            adt82.u1.m1.m1 = call196.m1;
            return adt82;
        }
    }
    lexer_Result adt83 = {0};
    return adt83;
}

void lexer_pop(table1* lexer_t) {
    {
            lexer_t->len--;
        }
}

