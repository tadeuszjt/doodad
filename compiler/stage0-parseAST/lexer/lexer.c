/* Doodad Module: lexer */
#include "doodad.h"

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

typedef struct { int64_t en; union { lexer_PosString u0; lexer_PosString u1; lexer_PosString u2; lexer_PosString u3; lexer_PosString u4; lexer_PosString u5; lexer_PosString u6; lexer_PosString u7; lexer_PosString u8; lexer_PosString u9; lexer_PosString u10; } ; } adt0;

typedef adt0 lexer_Token;

typedef struct { lexer_Token m0; lexer_PosString m1; } tuple3;

typedef struct { int64_t en; union { int8_t u0; tuple3 u1; } ; } adt1;

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

typedef struct { adt4 m0; char* m1; } tuple4;
extern tuple4 strings_readI64(char*);
extern char* strings_take(char*, int64_t);
extern adt3 lexer_At(lexer_PosString*, int64_t);
extern lexer_PosString lexer_Drop(lexer_PosString, int64_t);
extern lexer_PosString lexer_Take(lexer_PosString, int64_t);

typedef struct { int64_t len; int64_t cap; char** r0; } table1;
extern void lexer_indent(io_Io*, table1*, io_FileKey, char*);
extern lexer_Result lexer_lex(lexer_PosString);
extern lexer_Result lexer_lexCEmbed(lexer_PosString);
extern lexer_Result lexer_lexCharLiteral(lexer_PosString);
extern void lexer_lexFile(char*, char*);
extern lexer_Result lexer_lexFloating(lexer_PosString);
extern lexer_Result lexer_lexIdent(lexer_PosString);
extern lexer_Result lexer_lexImport(lexer_PosString);
extern lexer_Result lexer_lexInclude(lexer_PosString);
extern lexer_Result lexer_lexInteger(lexer_PosString);
extern lexer_Result lexer_lexKeyword(lexer_PosString);
extern lexer_Result lexer_lexNewline(lexer_PosString);
extern lexer_Result lexer_lexStringLiteral(lexer_PosString);
extern lexer_Result lexer_lexSymbol(lexer_PosString);
extern void lexer_main();
extern void lexer_pop_1(table1*);

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

void lexer_indent(io_Io* lexer_io, table1* lexer_stack, io_FileKey lexer_key, char* lexer_s_1) {
    assert_assert(((*lexer_stack).len > 0));
    char* lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
    int64_t call3 = strlen(lexer_entry);
    int64_t lexer_minLen = call3;
    int64_t call4 = strlen(lexer_s_1);
    if ((call4 < lexer_minLen)) {
        int64_t call5 = strlen(lexer_s_1);
        lexer_minLen = call5;
    }
    char* call6 = strings_take(lexer_s_1, lexer_minLen);
    char* call7 = strings_take(lexer_entry, lexer_minLen);
    bool call8 = doodad_string_eqeq(call6, call7);
    assert_assert(call8);
    bool call9 = doodad_string_eqeq(lexer_s_1, lexer_entry);
    if (call9) {
        io_fPutStrLn(lexer_io, lexer_key, "0:0:newline:");
    }
    else {
        int64_t call10 = strlen(lexer_s_1);
        int64_t call11 = strlen(lexer_entry);
        if ((call10 > call11)) {
            array1 array0 = {lexer_s_1};
            table1 table2 = {1, 1, array0.arr};
            lexer_table_append0(lexer_stack, &(table2));
            io_fPutStrLn(lexer_io, lexer_key, "0:0:indent:");
        }
        else {
            int64_t call12 = strlen(lexer_entry);
            int64_t call13 = strlen(lexer_s_1);
            if ((call12 > call13)) {
                io_fPutStrLn(lexer_io, lexer_key, "0:0:newline:");
                
                for (; ; ) {
                    int64_t call14 = strlen(lexer_entry);
                    int64_t call15 = strlen(lexer_s_1);
                    if (!(call14 > call15)) {
                        break;
                    }
                    io_fPutStrLn(lexer_io, lexer_key, "0:0:dedent:");
                    lexer_pop_1(lexer_stack);
                    assert_assert(((*lexer_stack).len > 0));
                    lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
                }
                bool call16 = doodad_string_eqeq(lexer_s_1, lexer_entry);
                assert_assert(call16);
            }
        }
    }
}

lexer_Result lexer_lex(lexer_PosString lexer_str_14) {
    
    for (; ; ) {
        adt3 call17 = lexer_At(&(lexer_str_14), 0);
        bool matchNull1 = (1 == call17.en);
        if (!matchNull1) {
            goto matchSkip1;
        }
        char lexer_c_17 = call17.u1;
        matchNull1 = true;
        matchSkip1:;
        bool match1 = matchNull1;
        if (match1) {
            match1 = ((lexer_c_17 == ' ') || (lexer_c_17 == '\t'));
        }
        if (!match1) {
            break;
        }
        lexer_PosString call18 = lexer_Drop(lexer_str_14, 1);
        lexer_str_14.m0 = call18.m0;
        lexer_str_14.m1 = call18.m1;
    }
    lexer_Result call19 = lexer_lexNewline(lexer_str_14);
    bool match2 = (1 == call19.en);
    if (!match2) {
        goto skipMatch0;
    }
    match2 = false;
    lexer_Token lexer_token = call19.u1.m0;
    lexer_PosString lexer_rest_3 = call19.u1.m1;
    match2 = true;
    skipMatch0:;
    if (match2) {
        lexer_Result adt7 = {1};
        adt7.u1.m0 = lexer_token;
        adt7.u1.m1.m0 = lexer_rest_3.m0;
        adt7.u1.m1.m1 = lexer_rest_3.m1;
        return adt7;
    }
    lexer_Result call20 = lexer_lexImport(lexer_str_14);
    bool match3 = (1 == call20.en);
    if (!match3) {
        goto skipMatch1;
    }
    match3 = false;
    lexer_Token lexer_token_1 = call20.u1.m0;
    lexer_PosString lexer_rest_4 = call20.u1.m1;
    match3 = true;
    skipMatch1:;
    if (match3) {
        lexer_Result adt8 = {1};
        adt8.u1.m0 = lexer_token_1;
        adt8.u1.m1.m0 = lexer_rest_4.m0;
        adt8.u1.m1.m1 = lexer_rest_4.m1;
        return adt8;
    }
    lexer_Result call21 = lexer_lexInclude(lexer_str_14);
    bool match4 = (1 == call21.en);
    if (!match4) {
        goto skipMatch2;
    }
    match4 = false;
    lexer_Token lexer_token_2 = call21.u1.m0;
    lexer_PosString lexer_rest_5 = call21.u1.m1;
    match4 = true;
    skipMatch2:;
    if (match4) {
        lexer_Result adt9 = {1};
        adt9.u1.m0 = lexer_token_2;
        adt9.u1.m1.m0 = lexer_rest_5.m0;
        adt9.u1.m1.m1 = lexer_rest_5.m1;
        return adt9;
    }
    lexer_Result call22 = lexer_lexCharLiteral(lexer_str_14);
    bool match5 = (1 == call22.en);
    if (!match5) {
        goto skipMatch3;
    }
    match5 = false;
    lexer_Token lexer_token_3 = call22.u1.m0;
    lexer_PosString lexer_rest_6 = call22.u1.m1;
    match5 = true;
    skipMatch3:;
    if (match5) {
        lexer_Result adt10 = {1};
        adt10.u1.m0 = lexer_token_3;
        adt10.u1.m1.m0 = lexer_rest_6.m0;
        adt10.u1.m1.m1 = lexer_rest_6.m1;
        return adt10;
    }
    lexer_Result call23 = lexer_lexStringLiteral(lexer_str_14);
    bool match6 = (1 == call23.en);
    if (!match6) {
        goto skipMatch4;
    }
    match6 = false;
    lexer_Token lexer_token_4 = call23.u1.m0;
    lexer_PosString lexer_rest_7 = call23.u1.m1;
    match6 = true;
    skipMatch4:;
    if (match6) {
        lexer_Result adt11 = {1};
        adt11.u1.m0 = lexer_token_4;
        adt11.u1.m1.m0 = lexer_rest_7.m0;
        adt11.u1.m1.m1 = lexer_rest_7.m1;
        return adt11;
    }
    lexer_Result call24 = lexer_lexFloating(lexer_str_14);
    bool match7 = (1 == call24.en);
    if (!match7) {
        goto skipMatch5;
    }
    match7 = false;
    lexer_Token lexer_token_5 = call24.u1.m0;
    lexer_PosString lexer_rest_8 = call24.u1.m1;
    match7 = true;
    skipMatch5:;
    if (match7) {
        lexer_Result adt12 = {1};
        adt12.u1.m0 = lexer_token_5;
        adt12.u1.m1.m0 = lexer_rest_8.m0;
        adt12.u1.m1.m1 = lexer_rest_8.m1;
        return adt12;
    }
    lexer_Result call25 = lexer_lexInteger(lexer_str_14);
    bool match8 = (1 == call25.en);
    if (!match8) {
        goto skipMatch6;
    }
    match8 = false;
    lexer_Token lexer_token_6 = call25.u1.m0;
    lexer_PosString lexer_rest_9 = call25.u1.m1;
    match8 = true;
    skipMatch6:;
    if (match8) {
        lexer_Result adt13 = {1};
        adt13.u1.m0 = lexer_token_6;
        adt13.u1.m1.m0 = lexer_rest_9.m0;
        adt13.u1.m1.m1 = lexer_rest_9.m1;
        return adt13;
    }
    lexer_Result call26 = lexer_lexKeyword(lexer_str_14);
    bool match9 = (1 == call26.en);
    if (!match9) {
        goto skipMatch7;
    }
    match9 = false;
    lexer_Token lexer_token_7 = call26.u1.m0;
    lexer_PosString lexer_rest_10 = call26.u1.m1;
    match9 = true;
    skipMatch7:;
    if (match9) {
        lexer_Result adt14 = {1};
        adt14.u1.m0 = lexer_token_7;
        adt14.u1.m1.m0 = lexer_rest_10.m0;
        adt14.u1.m1.m1 = lexer_rest_10.m1;
        return adt14;
    }
    lexer_Result call27 = lexer_lexIdent(lexer_str_14);
    bool match10 = (1 == call27.en);
    if (!match10) {
        goto skipMatch8;
    }
    match10 = false;
    lexer_Token lexer_token_8 = call27.u1.m0;
    lexer_PosString lexer_rest_11 = call27.u1.m1;
    match10 = true;
    skipMatch8:;
    if (match10) {
        lexer_Result adt15 = {1};
        adt15.u1.m0 = lexer_token_8;
        adt15.u1.m1.m0 = lexer_rest_11.m0;
        adt15.u1.m1.m1 = lexer_rest_11.m1;
        return adt15;
    }
    lexer_Result call28 = lexer_lexSymbol(lexer_str_14);
    bool match11 = (1 == call28.en);
    if (!match11) {
        goto skipMatch9;
    }
    match11 = false;
    lexer_Token lexer_token_9 = call28.u1.m0;
    lexer_PosString lexer_rest_12 = call28.u1.m1;
    match11 = true;
    skipMatch9:;
    if (match11) {
        lexer_Result adt16 = {1};
        adt16.u1.m0 = lexer_token_9;
        adt16.u1.m1.m0 = lexer_rest_12.m0;
        adt16.u1.m1.m1 = lexer_rest_12.m1;
        return adt16;
    }
    lexer_Result call29 = lexer_lexCEmbed(lexer_str_14);
    bool match12 = (1 == call29.en);
    if (!match12) {
        goto skipMatch10;
    }
    match12 = false;
    lexer_Token lexer_token_10 = call29.u1.m0;
    lexer_PosString lexer_rest_13 = call29.u1.m1;
    match12 = true;
    skipMatch10:;
    if (match12) {
        lexer_Result adt17 = {1};
        adt17.u1.m0 = lexer_token_10;
        adt17.u1.m1.m0 = lexer_rest_13.m0;
        adt17.u1.m1.m1 = lexer_rest_13.m1;
        return adt17;
    }
    lexer_Result adt18 = {0};
    return adt18;
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

lexer_Result lexer_lexCEmbed(lexer_PosString lexer_str_11) {
    adt3 call30 = lexer_At(&(lexer_str_11), 0);
    bool matchNull2 = (1 == call30.en);
    if (!matchNull2) {
        goto matchSkip2;
    }
    matchNull2 = (call30.u1 == '$');
    matchSkip2:;
    bool match13 = matchNull2;
    if (!match13) {
        goto end0;
    }
    adt3 call31 = lexer_At(&(lexer_str_11), 1);
    bool matchNull3 = (1 == call31.en);
    if (!matchNull3) {
        goto matchSkip3;
    }
    matchNull3 = (call31.u1 == '{');
    matchSkip3:;
    match13 = matchNull3;
    end0:;
    if (match13) {
    }
    else {
        lexer_Result adt19 = {0};
        return adt19;
    }
    lexer_PosString call32 = lexer_Drop(lexer_str_11, 1);
    lexer_str_11.m0 = call32.m0;
    lexer_str_11.m1 = call32.m1;
    int64_t lexer_idx_7 = 1;
    int64_t lexer_level = 1;
    
    for (; ; ) {
        adt3 call33 = lexer_At(&(lexer_str_11), lexer_idx_7);
        bool matchNull4 = (1 == call33.en);
        if (!matchNull4) {
            goto matchSkip4;
        }
        char lexer_c_11 = call33.u1;
        matchNull4 = true;
        matchSkip4:;
        bool match14 = matchNull4;
        if (match14) {
            match14 = (lexer_level > 0);
        }
        if (!match14) {
            break;
        }
        char switchExpr0 = lexer_c_11;
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
        lexer_idx_7 = (lexer_idx_7 + 1);
    }
    if ((lexer_level != 0)) {
        lexer_Result adt20 = {0};
        return adt20;
    }
    table3 lexer_result = {0};
    int64_t idx1 = 0;
    bool first0 = true;
    
    for (; ; idx1++) {
        lexer_PosString call34 = lexer_Take(lexer_str_11, lexer_idx_7);
        if ((idx1 >= strlen(call34.m0))) {
            break;
        }
        char lexer_c_12 = call34.m0[idx1];
        char switchExpr1 = lexer_c_12;
        switch(0) {
            case 0: {
                if ((switchExpr1 == '\n')) {
                    array3 array2 = {(char)(31)};
                    table3 table4 = {1, 1, array2.arr};
                    lexer_table_append1(&(lexer_result), &(table4));
                    break;
                }
                if (true) {
                    array3 array4 = {lexer_c_12};
                    table3 table5 = {1, 1, array4.arr};
                    lexer_table_append1(&(lexer_result), &(table5));
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString lexer_str2 = lexer_str_11;
    char* call35 = doodad_string_copy(lexer_result.r0);
    lexer_str2.m0 = call35;
    lexer_Token adt21 = {8};
    adt21.u8.m0 = lexer_str2.m0;
    adt21.u8.m1 = lexer_str2.m1;
    lexer_PosString call36 = lexer_Drop(lexer_str_11, lexer_idx_7);
    lexer_Result adt22 = {1};
    adt22.u1.m0 = adt21;
    adt22.u1.m1.m0 = call36.m0;
    adt22.u1.m1.m1 = call36.m1;
    return adt22;
}

lexer_Result lexer_lexCharLiteral(lexer_PosString lexer_str_12) {
    lexer_PosString call37 = lexer_Take(lexer_str_12, 4);
    bool match15 = false;
    if ((4 != strlen(call37.m0))) {
        goto end1;
    }
    if ((call37.m0[0] != '\'')) {
        goto end1;
    }
    if ((call37.m0[1] != '\\')) {
        goto end1;
    }
    char lexer_c_13 = call37.m0[2];
    if ((call37.m0[3] != '\'')) {
        goto end1;
    }
    match15 = true;
    end1:;
    if (match15) {
        char switchExpr2 = lexer_c_13;
        switch(0) {
            case 0: {
                if ((switchExpr2 == 'n')) {
                    lexer_PosString call38 = lexer_Take(lexer_str_12, 4);
                    lexer_Token adt23 = {6};
                    adt23.u6.m0 = call38.m0;
                    adt23.u6.m1 = call38.m1;
                    lexer_PosString call39 = lexer_Drop(lexer_str_12, 4);
                    lexer_Result adt24 = {1};
                    adt24.u1.m0 = adt23;
                    adt24.u1.m1.m0 = call39.m0;
                    adt24.u1.m1.m1 = call39.m1;
                    return adt24;
                }
                if ((switchExpr2 == 't')) {
                    lexer_PosString call40 = lexer_Take(lexer_str_12, 4);
                    lexer_Token adt25 = {6};
                    adt25.u6.m0 = call40.m0;
                    adt25.u6.m1 = call40.m1;
                    lexer_PosString call41 = lexer_Drop(lexer_str_12, 4);
                    lexer_Result adt26 = {1};
                    adt26.u1.m0 = adt25;
                    adt26.u1.m1.m0 = call41.m0;
                    adt26.u1.m1.m1 = call41.m1;
                    return adt26;
                }
                if ((switchExpr2 == '0')) {
                    lexer_PosString call42 = lexer_Take(lexer_str_12, 4);
                    lexer_Token adt27 = {6};
                    adt27.u6.m0 = call42.m0;
                    adt27.u6.m1 = call42.m1;
                    lexer_PosString call43 = lexer_Drop(lexer_str_12, 4);
                    lexer_Result adt28 = {1};
                    adt28.u1.m0 = adt27;
                    adt28.u1.m1.m0 = call43.m0;
                    adt28.u1.m1.m1 = call43.m1;
                    return adt28;
                }
                if ((switchExpr2 == '\\')) {
                    lexer_PosString call44 = lexer_Take(lexer_str_12, 4);
                    lexer_Token adt29 = {6};
                    adt29.u6.m0 = call44.m0;
                    adt29.u6.m1 = call44.m1;
                    lexer_PosString call45 = lexer_Drop(lexer_str_12, 4);
                    lexer_Result adt30 = {1};
                    adt30.u1.m0 = adt29;
                    adt30.u1.m1.m0 = call45.m0;
                    adt30.u1.m1.m1 = call45.m1;
                    return adt30;
                }
                if ((switchExpr2 == '\'')) {
                    lexer_PosString call46 = lexer_Take(lexer_str_12, 4);
                    lexer_Token adt31 = {6};
                    adt31.u6.m0 = call46.m0;
                    adt31.u6.m1 = call46.m1;
                    lexer_PosString call47 = lexer_Drop(lexer_str_12, 4);
                    lexer_Result adt32 = {1};
                    adt32.u1.m0 = adt31;
                    adt32.u1.m1.m0 = call47.m0;
                    adt32.u1.m1.m1 = call47.m1;
                    return adt32;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString call48 = lexer_Take(lexer_str_12, 3);
    char* switchExpr3 = call48.m0;
    switch(0) {
        case 0: {
            bool match16 = false;
            if ((3 != strlen(switchExpr3))) {
                goto end2;
            }
            if ((switchExpr3[0] != '\'')) {
                goto end2;
            }
            if ((switchExpr3[1] != '\'')) {
                goto end2;
            }
            if ((switchExpr3[2] != '\'')) {
                goto end2;
            }
            match16 = true;
            end2:;
            if (match16) {
                break;
            }
            bool match17 = false;
            if ((3 != strlen(switchExpr3))) {
                goto end3;
            }
            if ((switchExpr3[0] != '\'')) {
                goto end3;
            }
            char lexer_c_14 = switchExpr3[1];
            if ((switchExpr3[2] != '\'')) {
                goto end3;
            }
            match17 = true;
            end3:;
            if (match17) {
                lexer_PosString call49 = lexer_Take(lexer_str_12, 3);
                lexer_Token adt33 = {6};
                adt33.u6.m0 = call49.m0;
                adt33.u6.m1 = call49.m1;
                lexer_PosString call50 = lexer_Drop(lexer_str_12, 3);
                lexer_Result adt34 = {1};
                adt34.u1.m0 = adt33;
                adt34.u1.m1.m0 = call50.m0;
                adt34.u1.m1.m1 = call50.m1;
                return adt34;
            }
            if (true) {
                break;
            }
            assert(false);
        }
    }
    lexer_Result adt35 = {0};
    return adt35;
}

void lexer_lexFile(char* lexer_fileNameIn, char* lexer_fileNameOut) {
    io_Io lexer_io_1 = {0};
    io_FileKey call51 = io_openFile(&(lexer_io_1), lexer_fileNameOut);
    io_FileKey lexer_key_1 = call51;
    lexer_PosString zero0 = {0};
    lexer_PosString lexer_str_15 = zero0;
    char* call52 = io_readFile(&(lexer_io_1), lexer_fileNameIn);
    lexer_str_15.m0 = call52;
    table1 lexer_indentStack = {0};
    array1 array5 = {""};
    table1 table6 = {1, 1, array5.arr};
    lexer_table_append0(&(lexer_indentStack), &(table6));
    
    for (; ; ) {
        lexer_Result call53 = lexer_lex(lexer_str_15);
        bool match18 = (1 == call53.en);
        if (!match18) {
            goto skipMatch11;
        }
        match18 = false;
        lexer_Token lexer_token_11 = call53.u1.m0;
        lexer_PosString lexer_rest_14 = call53.u1.m1;
        match18 = true;
        skipMatch11:;
        if (!match18) {
            break;
        }
        lexer_Token switchExpr4 = lexer_token_11;
        switch(0) {
            case 0: {
                bool match19 = (0 == switchExpr4.en);
                if (!match19) {
                    goto skipMatch12;
                }
                match19 = false;
                lexer_PosString lexer_s_2 = switchExpr4.u0;
                match19 = true;
                skipMatch12:;
                if (match19) {
                    char* call54 = doodad_string_i64(lexer_s_2.m1.m0);
                    char* call55 = doodad_string_plus(call54, ":");
                    char* call56 = doodad_string_i64(lexer_s_2.m1.m1);
                    char* call57 = doodad_string_plus(call55, call56);
                    char* call58 = doodad_string_plus(call57, ":ident: ");
                    char* call59 = doodad_string_plus(call58, lexer_s_2.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call59);
                    break;
                }
                bool match20 = (1 == switchExpr4.en);
                if (!match20) {
                    goto skipMatch13;
                }
                match20 = false;
                lexer_PosString lexer_s_3 = switchExpr4.u1;
                match20 = true;
                skipMatch13:;
                if (match20) {
                    char* call60 = doodad_string_i64(lexer_s_3.m1.m0);
                    char* call61 = doodad_string_plus(call60, ":");
                    char* call62 = doodad_string_i64(lexer_s_3.m1.m1);
                    char* call63 = doodad_string_plus(call61, call62);
                    char* call64 = doodad_string_plus(call63, ":keyword: ");
                    char* call65 = doodad_string_plus(call64, lexer_s_3.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call65);
                    break;
                }
                bool match21 = (2 == switchExpr4.en);
                if (!match21) {
                    goto skipMatch14;
                }
                match21 = false;
                lexer_PosString lexer_s_4 = switchExpr4.u2;
                match21 = true;
                skipMatch14:;
                if (match21) {
                    char* call66 = doodad_string_i64(lexer_s_4.m1.m0);
                    char* call67 = doodad_string_plus(call66, ":");
                    char* call68 = doodad_string_i64(lexer_s_4.m1.m1);
                    char* call69 = doodad_string_plus(call67, call68);
                    char* call70 = doodad_string_plus(call69, ":integer: ");
                    char* call71 = doodad_string_plus(call70, lexer_s_4.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call71);
                    break;
                }
                bool match22 = (3 == switchExpr4.en);
                if (!match22) {
                    goto skipMatch15;
                }
                match22 = false;
                lexer_PosString lexer_s_5 = switchExpr4.u3;
                match22 = true;
                skipMatch15:;
                if (match22) {
                    char* call72 = doodad_string_i64(lexer_s_5.m1.m0);
                    char* call73 = doodad_string_plus(call72, ":");
                    char* call74 = doodad_string_i64(lexer_s_5.m1.m1);
                    char* call75 = doodad_string_plus(call73, call74);
                    char* call76 = doodad_string_plus(call75, ":floating: ");
                    char* call77 = doodad_string_plus(call76, lexer_s_5.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call77);
                    break;
                }
                bool match23 = (5 == switchExpr4.en);
                if (!match23) {
                    goto skipMatch16;
                }
                match23 = false;
                lexer_PosString lexer_s_6 = switchExpr4.u5;
                match23 = true;
                skipMatch16:;
                if (match23) {
                    char* call78 = doodad_string_i64(lexer_s_6.m1.m0);
                    char* call79 = doodad_string_plus(call78, ":");
                    char* call80 = doodad_string_i64(lexer_s_6.m1.m1);
                    char* call81 = doodad_string_plus(call79, call80);
                    char* call82 = doodad_string_plus(call81, ":symbol: ");
                    char* call83 = doodad_string_plus(call82, lexer_s_6.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call83);
                    break;
                }
                bool match24 = (6 == switchExpr4.en);
                if (!match24) {
                    goto skipMatch17;
                }
                match24 = false;
                lexer_PosString lexer_s_7 = switchExpr4.u6;
                match24 = true;
                skipMatch17:;
                if (match24) {
                    char* call84 = doodad_string_i64(lexer_s_7.m1.m0);
                    char* call85 = doodad_string_plus(call84, ":");
                    char* call86 = doodad_string_i64(lexer_s_7.m1.m1);
                    char* call87 = doodad_string_plus(call85, call86);
                    char* call88 = doodad_string_plus(call87, ":char: ");
                    char* call89 = doodad_string_plus(call88, lexer_s_7.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call89);
                    break;
                }
                bool match25 = (7 == switchExpr4.en);
                if (!match25) {
                    goto skipMatch18;
                }
                match25 = false;
                lexer_PosString lexer_s_8 = switchExpr4.u7;
                match25 = true;
                skipMatch18:;
                if (match25) {
                    char* call90 = doodad_string_i64(lexer_s_8.m1.m0);
                    char* call91 = doodad_string_plus(call90, ":");
                    char* call92 = doodad_string_i64(lexer_s_8.m1.m1);
                    char* call93 = doodad_string_plus(call91, call92);
                    char* call94 = doodad_string_plus(call93, ":string: ");
                    char* call95 = doodad_string_plus(call94, lexer_s_8.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call95);
                    break;
                }
                bool match26 = (4 == switchExpr4.en);
                if (!match26) {
                    goto skipMatch19;
                }
                match26 = false;
                lexer_PosString lexer_s_9 = switchExpr4.u4;
                match26 = true;
                skipMatch19:;
                if (match26) {
                    lexer_indent(&(lexer_io_1), &(lexer_indentStack), lexer_key_1, lexer_s_9.m0);
                    break;
                }
                bool match27 = (8 == switchExpr4.en);
                if (!match27) {
                    goto skipMatch20;
                }
                match27 = false;
                lexer_PosString lexer_s_10 = switchExpr4.u8;
                match27 = true;
                skipMatch20:;
                if (match27) {
                    char* call96 = doodad_string_i64(lexer_s_10.m1.m0);
                    char* call97 = doodad_string_plus(call96, ":");
                    char* call98 = doodad_string_i64(lexer_s_10.m1.m1);
                    char* call99 = doodad_string_plus(call97, call98);
                    char* call100 = doodad_string_plus(call99, ":cembed: ");
                    char* call101 = doodad_string_plus(call100, lexer_s_10.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call101);
                    break;
                }
                bool match28 = (9 == switchExpr4.en);
                if (!match28) {
                    goto skipMatch21;
                }
                match28 = false;
                lexer_PosString lexer_s_11 = switchExpr4.u9;
                match28 = true;
                skipMatch21:;
                if (match28) {
                    char* call102 = doodad_string_i64(lexer_s_11.m1.m0);
                    char* call103 = doodad_string_plus(call102, ":");
                    char* call104 = doodad_string_i64(lexer_s_11.m1.m1);
                    char* call105 = doodad_string_plus(call103, call104);
                    char* call106 = doodad_string_plus(call105, ":import: ");
                    char* call107 = doodad_string_plus(call106, lexer_s_11.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call107);
                    break;
                }
                bool match29 = (10 == switchExpr4.en);
                if (!match29) {
                    goto skipMatch22;
                }
                match29 = false;
                lexer_PosString lexer_s_12 = switchExpr4.u10;
                match29 = true;
                skipMatch22:;
                if (match29) {
                    char* call108 = doodad_string_i64(lexer_s_12.m1.m0);
                    char* call109 = doodad_string_plus(call108, ":");
                    char* call110 = doodad_string_i64(lexer_s_12.m1.m1);
                    char* call111 = doodad_string_plus(call109, call110);
                    char* call112 = doodad_string_plus(call111, ":include: ");
                    char* call113 = doodad_string_plus(call112, lexer_s_12.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call113);
                    break;
                }
                assert(false);
            }
        }
        lexer_str_15.m0 = lexer_rest_14.m0;
        lexer_str_15.m1 = lexer_rest_14.m1;
    }
    io_closeFile(&(lexer_io_1), lexer_key_1);
}

lexer_Result lexer_lexFloating(lexer_PosString lexer_str_9) {
    adt3 call114 = lexer_At(&(lexer_str_9), 0);
    bool matchNull5 = (1 == call114.en);
    if (!matchNull5) {
        goto matchSkip5;
    }
    char lexer_c_8 = call114.u1;
    matchNull5 = true;
    matchSkip5:;
    bool match30 = matchNull5;
    if (match30) {
        bool call115 = strings_isDigit(lexer_c_8);
        match30 = call115;
    }
    if (match30) {
    }
    else {
        lexer_Result adt36 = {0};
        return adt36;
    }
    int64_t lexer_idx_5 = 0;
    bool lexer_dot = false;
    
    for (; ; ) {
        adt3 call116 = lexer_At(&(lexer_str_9), lexer_idx_5);
        bool matchNull6 = (1 == call116.en);
        if (!matchNull6) {
            goto matchSkip6;
        }
        char lexer_c_9 = call116.u1;
        matchNull6 = true;
        matchSkip6:;
        bool match31 = matchNull6;
        if (match31) {
            bool call117 = strings_isDigit(lexer_c_9);
            match31 = (call117 || ((lexer_c_9 == '.') && !lexer_dot));
        }
        if (!match31) {
            break;
        }
        lexer_idx_5 = (lexer_idx_5 + 1);
        if ((lexer_c_9 == '.')) {
            lexer_dot = true;
        }
    }
    adt3 call118 = lexer_At(&(lexer_str_9), lexer_idx_5);
    bool matchNull7 = (1 == call118.en);
    if (!matchNull7) {
        goto matchSkip7;
    }
    matchNull7 = (call118.u1 == '.');
    matchSkip7:;
    if (matchNull7) {
        lexer_Result adt37 = {0};
        return adt37;
    }
    if (lexer_dot) {
        lexer_PosString call119 = lexer_Take(lexer_str_9, lexer_idx_5);
        lexer_Token adt38 = {3};
        adt38.u3.m0 = call119.m0;
        adt38.u3.m1 = call119.m1;
        lexer_PosString call120 = lexer_Drop(lexer_str_9, lexer_idx_5);
        lexer_Result adt39 = {1};
        adt39.u1.m0 = adt38;
        adt39.u1.m1.m0 = call120.m0;
        adt39.u1.m1.m1 = call120.m1;
        return adt39;
    }
    lexer_Result adt40 = {0};
    return adt40;
}

lexer_Result lexer_lexIdent(lexer_PosString lexer_str_3) {
    adt3 call121 = lexer_At(&(lexer_str_3), 0);
    bool matchNull8 = (1 == call121.en);
    if (!matchNull8) {
        goto matchSkip8;
    }
    char lexer_c_1 = call121.u1;
    matchNull8 = true;
    matchSkip8:;
    bool match32 = matchNull8;
    if (match32) {
        bool call122 = strings_isAlpha(lexer_c_1);
        match32 = call122;
    }
    if (match32) {
        int64_t lexer_idx_1 = 0;
        
        for (; ; ) {
            adt3 call123 = lexer_At(&(lexer_str_3), lexer_idx_1);
            bool matchNull9 = (1 == call123.en);
            if (!matchNull9) {
                goto matchSkip9;
            }
            char lexer_c_2 = call123.u1;
            matchNull9 = true;
            matchSkip9:;
            bool match33 = matchNull9;
            if (match33) {
                bool call124 = strings_isAlpha(lexer_c_2);
                bool call125 = strings_isDigit(lexer_c_2);
                match33 = ((call124 || call125) || (lexer_c_2 == '_'));
            }
            if (!match33) {
                break;
            }
            lexer_idx_1 = (lexer_idx_1 + 1);
        }
        lexer_PosString call126 = lexer_Take(lexer_str_3, lexer_idx_1);
        lexer_Token adt41 = {0};
        adt41.u0.m0 = call126.m0;
        adt41.u0.m1 = call126.m1;
        lexer_PosString call127 = lexer_Drop(lexer_str_3, lexer_idx_1);
        lexer_Result adt42 = {1};
        adt42.u1.m0 = adt41;
        adt42.u1.m1.m0 = call127.m0;
        adt42.u1.m1.m1 = call127.m1;
        return adt42;
    }
    lexer_Result adt43 = {0};
    return adt43;
}

lexer_Result lexer_lexImport(lexer_PosString lexer_str_4) {
    lexer_Result call128 = lexer_lexIdent(lexer_str_4);
    bool match34 = (1 == call128.en);
    if (!match34) {
        goto skipMatch23;
    }
    match34 = false;
    bool match35 = (0 == call128.u1.m0.en);
    if (!match35) {
        goto skipMatch24;
    }
    match35 = false;
    lexer_PosString lexer_ps = call128.u1.m0.u0;
    match35 = true;
    skipMatch24:;
    if (!match35) {
        goto skipMatch23;
    }
    lexer_PosString lexer_rest = call128.u1.m1;
    match34 = true;
    skipMatch23:;
    bool match36 = match34;
    if (match36) {
        bool call129 = doodad_string_eqeq(lexer_ps.m0, "import");
        match36 = call129;
    }
    if (match36) {
        lexer_str_4.m0 = lexer_rest.m0;
        lexer_str_4.m1 = lexer_rest.m1;
    }
    else {
        lexer_Result adt44 = {0};
        return adt44;
    }
    int64_t lexer_idx_2 = 0;
    
    for (; ; ) {
        adt3 call130 = lexer_At(&(lexer_str_4), lexer_idx_2);
        bool matchNull10 = (1 == call130.en);
        if (!matchNull10) {
            goto matchSkip10;
        }
        char lexer_c_3 = call130.u1;
        matchNull10 = true;
        matchSkip10:;
        bool match37 = matchNull10;
        if (match37) {
            match37 = (lexer_c_3 != '\n');
        }
        if (!match37) {
            break;
        }
        lexer_idx_2 = (lexer_idx_2 + 1);
    }
    lexer_PosString call131 = lexer_Take(lexer_str_4, lexer_idx_2);
    lexer_Token adt45 = {9};
    adt45.u9.m0 = call131.m0;
    adt45.u9.m1 = call131.m1;
    lexer_PosString call132 = lexer_Drop(lexer_str_4, lexer_idx_2);
    lexer_Result adt46 = {1};
    adt46.u1.m0 = adt45;
    adt46.u1.m1.m0 = call132.m0;
    adt46.u1.m1.m1 = call132.m1;
    return adt46;
}

lexer_Result lexer_lexInclude(lexer_PosString lexer_str_5) {
    lexer_Result call133 = lexer_lexIdent(lexer_str_5);
    bool match38 = (1 == call133.en);
    if (!match38) {
        goto skipMatch25;
    }
    match38 = false;
    bool match39 = (0 == call133.u1.m0.en);
    if (!match39) {
        goto skipMatch26;
    }
    match39 = false;
    lexer_PosString lexer_ps_1 = call133.u1.m0.u0;
    match39 = true;
    skipMatch26:;
    if (!match39) {
        goto skipMatch25;
    }
    lexer_PosString lexer_rest_1 = call133.u1.m1;
    match38 = true;
    skipMatch25:;
    bool match40 = match38;
    if (match40) {
        bool call134 = doodad_string_eqeq(lexer_ps_1.m0, "include");
        match40 = call134;
    }
    if (match40) {
        lexer_str_5.m0 = lexer_rest_1.m0;
        lexer_str_5.m1 = lexer_rest_1.m1;
    }
    else {
        lexer_Result adt47 = {0};
        return adt47;
    }
    int64_t lexer_idx_3 = 0;
    
    for (; ; ) {
        adt3 call135 = lexer_At(&(lexer_str_5), lexer_idx_3);
        bool matchNull11 = (1 == call135.en);
        if (!matchNull11) {
            goto matchSkip11;
        }
        char lexer_c_4 = call135.u1;
        matchNull11 = true;
        matchSkip11:;
        bool match41 = matchNull11;
        if (match41) {
            match41 = (lexer_c_4 != '\n');
        }
        if (!match41) {
            break;
        }
        lexer_idx_3 = (lexer_idx_3 + 1);
    }
    lexer_PosString call136 = lexer_Take(lexer_str_5, lexer_idx_3);
    lexer_Token adt48 = {10};
    adt48.u10.m0 = call136.m0;
    adt48.u10.m1 = call136.m1;
    lexer_PosString call137 = lexer_Drop(lexer_str_5, lexer_idx_3);
    lexer_Result adt49 = {1};
    adt49.u1.m0 = adt48;
    adt49.u1.m1.m0 = call137.m0;
    adt49.u1.m1.m1 = call137.m1;
    return adt49;
}

lexer_Result lexer_lexInteger(lexer_PosString lexer_str_8) {
    adt3 call138 = lexer_At(&(lexer_str_8), 0);
    bool matchNull12 = (1 == call138.en);
    if (!matchNull12) {
        goto matchSkip12;
    }
    char lexer_c_6 = call138.u1;
    matchNull12 = true;
    matchSkip12:;
    bool match42 = matchNull12;
    if (match42) {
        bool call139 = strings_isDigit(lexer_c_6);
        match42 = call139;
    }
    if (match42) {
        int64_t lexer_idx_4 = 0;
        
        for (; ; ) {
            adt3 call140 = lexer_At(&(lexer_str_8), lexer_idx_4);
            bool matchNull13 = (1 == call140.en);
            if (!matchNull13) {
                goto matchSkip13;
            }
            char lexer_c_7 = call140.u1;
            matchNull13 = true;
            matchSkip13:;
            bool match43 = matchNull13;
            if (match43) {
                bool call141 = strings_isDigit(lexer_c_7);
                match43 = call141;
            }
            if (!match43) {
                break;
            }
            lexer_idx_4 = (lexer_idx_4 + 1);
        }
        lexer_PosString call142 = lexer_Take(lexer_str_8, lexer_idx_4);
        lexer_Token adt50 = {2};
        adt50.u2.m0 = call142.m0;
        adt50.u2.m1 = call142.m1;
        lexer_PosString call143 = lexer_Drop(lexer_str_8, lexer_idx_4);
        lexer_Result adt51 = {1};
        adt51.u1.m0 = adt50;
        adt51.u1.m1.m0 = call143.m0;
        adt51.u1.m1.m1 = call143.m1;
        return adt51;
    }
    lexer_Result adt52 = {0};
    return adt52;
}

typedef struct { char* arr[25]; } array7;

lexer_Result lexer_lexKeyword(lexer_PosString lexer_str_6) {
    array7 array6 = {"module", "type", "fn", "for", "if", "else", "switch", "while", "return", "data", "const", "let", "null", "true", "false", "string", "bool", "char", "i8", "i16", "i32", "i64", "u8", "f32", "f64"};
    array7 lexer_keywords = array6;
    lexer_Result call144 = lexer_lexIdent(lexer_str_6);
    bool match44 = (1 == call144.en);
    if (!match44) {
        goto skipMatch27;
    }
    match44 = false;
    bool match45 = (0 == call144.u1.m0.en);
    if (!match45) {
        goto skipMatch28;
    }
    match45 = false;
    lexer_PosString lexer_ps_2 = call144.u1.m0.u0;
    match45 = true;
    skipMatch28:;
    if (!match45) {
        goto skipMatch27;
    }
    lexer_PosString lexer_rest_2 = call144.u1.m1;
    match44 = true;
    skipMatch27:;
    if (match44) {
        int64_t idx3 = 0;
        bool first1 = true;
        
        for (; ; idx3++) {
            if ((idx3 >= 25)) {
                break;
            }
            char* lexer_keyword = lexer_keywords.arr[idx3];
            bool call145 = doodad_string_eqeq(lexer_keyword, lexer_ps_2.m0);
            if (call145) {
                lexer_Token adt53 = {1};
                adt53.u1.m0 = lexer_ps_2.m0;
                adt53.u1.m1 = lexer_ps_2.m1;
                lexer_Result adt54 = {1};
                adt54.u1.m0 = adt53;
                adt54.u1.m1.m0 = lexer_rest_2.m0;
                adt54.u1.m1.m1 = lexer_rest_2.m1;
                return adt54;
            }
        }
    }
    lexer_Result adt55 = {0};
    return adt55;
}

typedef struct { int64_t min; int64_t max; } range1;

lexer_Result lexer_lexNewline(lexer_PosString lexer_str_10) {
    adt3 call146 = lexer_At(&(lexer_str_10), 0);
    bool matchNull14 = (1 == call146.en);
    if (!matchNull14) {
        goto matchSkip14;
    }
    matchNull14 = (call146.u1 == '\n');
    matchSkip14:;
    if (matchNull14) {
        int64_t lexer_idx_6 = 0;
        
        for (; ; ) {
            adt3 call147 = lexer_At(&(lexer_str_10), lexer_idx_6);
            bool matchNull15 = (1 == call147.en);
            if (!matchNull15) {
                goto matchSkip15;
            }
            char lexer_c_10 = call147.u1;
            matchNull15 = true;
            matchSkip15:;
            bool match46 = matchNull15;
            if (match46) {
                match46 = (((lexer_c_10 == '\n') || (lexer_c_10 == '\t')) || (lexer_c_10 == ' '));
            }
            if (!match46) {
                break;
            }
            lexer_idx_6 = (lexer_idx_6 + 1);
        }
        lexer_PosString call148 = lexer_Take(lexer_str_10, lexer_idx_6);
        lexer_PosString lexer_spaces = call148;
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
            int64_t lexer_i = idx4;
            if ((lexer_spaces.m0[lexer_i] == '\n')) {
                lexer_lastNewline = lexer_i;
            }
        }
        assert_assert((lexer_lastNewline > (0 - 1)));
        lexer_PosString call149 = lexer_Drop(lexer_spaces, (lexer_lastNewline + 1));
        lexer_Token adt56 = {4};
        adt56.u4.m0 = call149.m0;
        adt56.u4.m1 = call149.m1;
        lexer_PosString call150 = lexer_Drop(lexer_str_10, lexer_idx_6);
        lexer_Result adt57 = {1};
        adt57.u1.m0 = adt56;
        adt57.u1.m1.m0 = call150.m0;
        adt57.u1.m1.m1 = call150.m1;
        return adt57;
    }
    lexer_Result adt58 = {0};
    return adt58;
}

lexer_Result lexer_lexStringLiteral(lexer_PosString lexer_str_13) {
    adt3 call151 = lexer_At(&(lexer_str_13), 0);
    bool matchNull16 = (1 == call151.en);
    if (!matchNull16) {
        goto matchSkip16;
    }
    matchNull16 = (call151.u1 == '"');
    matchSkip16:;
    if (matchNull16) {
        int64_t lexer_idx_8 = 1;
        
        for (; ; ) {
            adt3 call152 = lexer_At(&(lexer_str_13), lexer_idx_8);
            bool matchNull17 = (1 == call152.en);
            if (!matchNull17) {
                goto matchSkip17;
            }
            char lexer_c_15 = call152.u1;
            matchNull17 = true;
            matchSkip17:;
            bool match47 = matchNull17;
            if (match47) {
                match47 = ((lexer_c_15 != '"') && (lexer_c_15 != '\n'));
            }
            if (!match47) {
                break;
            }
            lexer_idx_8 = (lexer_idx_8 + 1);
        }
        adt3 call153 = lexer_At(&(lexer_str_13), lexer_idx_8);
        adt3 switchExpr5 = call153;
        switch(0) {
            case 0: {
                bool matchNull18 = (0 == switchExpr5.en);
                if (matchNull18) {
                    lexer_Result adt59 = {0};
                    return adt59;
                }
                bool matchNull19 = (1 == switchExpr5.en);
                if (!matchNull19) {
                    goto matchSkip18;
                }
                char lexer_c_16 = switchExpr5.u1;
                matchNull19 = true;
                matchSkip18:;
                bool match48 = matchNull19;
                if (match48) {
                    match48 = (lexer_c_16 != '"');
                }
                if (match48) {
                    lexer_Result adt60 = {0};
                    return adt60;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
        lexer_PosString call154 = lexer_Take(lexer_str_13, lexer_idx_8);
        lexer_PosString call155 = lexer_Drop(call154, 1);
        lexer_Token adt61 = {7};
        adt61.u7.m0 = call155.m0;
        adt61.u7.m1 = call155.m1;
        lexer_PosString call156 = lexer_Drop(lexer_str_13, (lexer_idx_8 + 1));
        lexer_Result adt62 = {1};
        adt62.u1.m0 = adt61;
        adt62.u1.m1.m0 = call156.m0;
        adt62.u1.m1.m1 = call156.m1;
        return adt62;
    }
    lexer_Result adt63 = {0};
    return adt63;
}

typedef struct { char* arr[10]; } array9;

lexer_Result lexer_lexSymbol(lexer_PosString lexer_str_7) {
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
        int64_t call157 = strlen(lexer_symbol);
        lexer_PosString call158 = lexer_Take(lexer_str_7, call157);
        lexer_PosString lexer_x = call158;
        bool call159 = doodad_string_eqeq(lexer_x.m0, lexer_symbol);
        if (call159) {
            lexer_Token adt64 = {5};
            adt64.u5.m0 = lexer_x.m0;
            adt64.u5.m1 = lexer_x.m1;
            int64_t call160 = strlen(lexer_symbol);
            lexer_PosString call161 = lexer_Drop(lexer_str_7, call160);
            lexer_Result adt65 = {1};
            adt65.u1.m0 = adt64;
            adt65.u1.m1.m0 = call161.m0;
            adt65.u1.m1.m1 = call161.m1;
            return adt65;
        }
    }
    int64_t idx6 = 0;
    bool first4 = true;
    
    for (; ; idx6++) {
        if ((idx6 >= strlen(lexer_singles))) {
            break;
        }
        char lexer_symbol_1 = lexer_singles[idx6];
        adt3 call162 = lexer_At(&(lexer_str_7), 0);
        bool matchNull20 = (1 == call162.en);
        if (!matchNull20) {
            goto matchSkip19;
        }
        char lexer_c_5 = call162.u1;
        matchNull20 = true;
        matchSkip19:;
        bool match49 = matchNull20;
        if (match49) {
            match49 = (lexer_c_5 == lexer_symbol_1);
        }
        if (match49) {
            lexer_PosString call163 = lexer_Take(lexer_str_7, 1);
            lexer_Token adt66 = {5};
            adt66.u5.m0 = call163.m0;
            adt66.u5.m1 = call163.m1;
            lexer_PosString call164 = lexer_Drop(lexer_str_7, 1);
            lexer_Result adt67 = {1};
            adt67.u1.m0 = adt66;
            adt67.u1.m1.m0 = call164.m0;
            adt67.u1.m1.m1 = call164.m1;
            return adt67;
        }
    }
    lexer_Result adt68 = {0};
    return adt68;
}

void lexer_pop_1(table1* lexer_t) {
    {
            lexer_t->len--;
        }
}

