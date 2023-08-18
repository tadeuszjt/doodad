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

typedef struct { } tuple3;

typedef struct { int64_t en; union { lexer_PosString u0; lexer_PosString u1; lexer_PosString u2; lexer_PosString u3; lexer_PosString u4; lexer_PosString u5; lexer_PosString u6; lexer_PosString u7; lexer_PosString u8; lexer_PosString u9; lexer_PosString u10; tuple3 u11; } ; } adt0;

typedef adt0 lexer_Token;

typedef struct { lexer_Token m0; lexer_PosString m1; } tuple4;

typedef struct { int64_t en; union { int8_t u0; tuple4 u1; } ; } adt1;

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

void lexer_indent(io_Io* lexer_io, table1* lexer_stack, io_FileKey lexer_key, lexer_PosString lexer_str_19) {
    assert_assert(((*lexer_stack).len > 0));
    char* lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
    int64_t call3 = strlen(lexer_entry);
    int64_t lexer_minLen = call3;
    int64_t call4 = strlen(lexer_str_19.m0);
    if ((call4 < lexer_minLen)) {
        int64_t call5 = strlen(lexer_str_19.m0);
        lexer_minLen = call5;
    }
    char* call6 = strings_take(lexer_str_19.m0, lexer_minLen);
    char* call7 = strings_take(lexer_entry, lexer_minLen);
    bool call8 = doodad_string_eqeq(call6, call7);
    assert_assert(call8);
    char* call9 = doodad_string_i64(lexer_str_19.m1.m0);
    char* call10 = doodad_string_plus(call9, ":");
    char* call11 = doodad_string_i64(lexer_str_19.m1.m1);
    char* call12 = doodad_string_plus(call10, call11);
    char* call13 = doodad_string_plus(call12, ":");
    char* lexer_posStr = call13;
    bool call14 = doodad_string_eqeq(lexer_str_19.m0, lexer_entry);
    if (call14) {
        char* call15 = doodad_string_plus(lexer_posStr, "newline:");
        io_fPutStrLn(lexer_io, lexer_key, call15);
    }
    else {
        int64_t call16 = strlen(lexer_str_19.m0);
        int64_t call17 = strlen(lexer_entry);
        if ((call16 > call17)) {
            array1 array0 = {lexer_str_19.m0};
            table1 table2 = {1, 1, array0.arr};
            lexer_table_append0(lexer_stack, &(table2));
            char* call18 = doodad_string_plus(lexer_posStr, "indent:");
            io_fPutStrLn(lexer_io, lexer_key, call18);
        }
        else {
            int64_t call19 = strlen(lexer_entry);
            int64_t call20 = strlen(lexer_str_19.m0);
            if ((call19 > call20)) {
                char* call21 = doodad_string_plus(lexer_posStr, "newline:");
                io_fPutStrLn(lexer_io, lexer_key, call21);
                
                for (; ; ) {
                    int64_t call22 = strlen(lexer_entry);
                    int64_t call23 = strlen(lexer_str_19.m0);
                    if (!(call22 > call23)) {
                        break;
                    }
                    char* call24 = doodad_string_plus(lexer_posStr, "dedent:");
                    io_fPutStrLn(lexer_io, lexer_key, call24);
                    lexer_pop(lexer_stack);
                    assert_assert(((*lexer_stack).len > 0));
                    lexer_entry = (*lexer_stack).r0[((*lexer_stack).len - 1)];
                }
                bool call25 = doodad_string_eqeq(lexer_str_19.m0, lexer_entry);
                assert_assert(call25);
            }
        }
    }
}

adt4 lexer_lex(lexer_PosString lexer_str_14, int64_t lexer_idx_8) {
    adt4 call26 = lexer_lexComment_1(lexer_str_14, lexer_idx_8);
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
    adt4 call27 = lexer_lexSpaceChar(lexer_str_14, lexer_idx_8);
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

lexer_Result lexer_lex_1(lexer_PosString lexer_str_18) {
    
    for (; ; ) {
        adt3 call28 = lexer_At(&(lexer_str_18), 0);
        bool matchNull3 = (1 == call28.en);
        if (!matchNull3) {
            goto matchSkip3;
        }
        char lexer_c_19 = call28.u1;
        matchNull3 = true;
        matchSkip3:;
        bool match1 = matchNull3;
        if (match1) {
            match1 = ((lexer_c_19 == ' ') || (lexer_c_19 == '\t'));
        }
        if (!match1) {
            break;
        }
        lexer_PosString call29 = lexer_Drop(lexer_str_18, 1);
        lexer_str_18.m0 = call29.m0;
        lexer_str_18.m1 = call29.m1;
    }
    lexer_Result call30 = lexer_lexNewline(lexer_str_18);
    bool match2 = (1 == call30.en);
    if (!match2) {
        goto skipMatch0;
    }
    match2 = false;
    lexer_Token lexer_token = call30.u1.m0;
    lexer_PosString lexer_rest_3 = call30.u1.m1;
    match2 = true;
    skipMatch0:;
    if (match2) {
        lexer_Result adt10 = {1};
        adt10.u1.m0 = lexer_token;
        adt10.u1.m1.m0 = lexer_rest_3.m0;
        adt10.u1.m1.m1 = lexer_rest_3.m1;
        return adt10;
    }
    lexer_Result call31 = lexer_lexImport(lexer_str_18);
    bool match3 = (1 == call31.en);
    if (!match3) {
        goto skipMatch1;
    }
    match3 = false;
    lexer_Token lexer_token_1 = call31.u1.m0;
    lexer_PosString lexer_rest_4 = call31.u1.m1;
    match3 = true;
    skipMatch1:;
    if (match3) {
        lexer_Result adt11 = {1};
        adt11.u1.m0 = lexer_token_1;
        adt11.u1.m1.m0 = lexer_rest_4.m0;
        adt11.u1.m1.m1 = lexer_rest_4.m1;
        return adt11;
    }
    lexer_Result call32 = lexer_lexInclude(lexer_str_18);
    bool match4 = (1 == call32.en);
    if (!match4) {
        goto skipMatch2;
    }
    match4 = false;
    lexer_Token lexer_token_2 = call32.u1.m0;
    lexer_PosString lexer_rest_5 = call32.u1.m1;
    match4 = true;
    skipMatch2:;
    if (match4) {
        lexer_Result adt12 = {1};
        adt12.u1.m0 = lexer_token_2;
        adt12.u1.m1.m0 = lexer_rest_5.m0;
        adt12.u1.m1.m1 = lexer_rest_5.m1;
        return adt12;
    }
    lexer_Result call33 = lexer_lexCharLiteral(lexer_str_18);
    bool match5 = (1 == call33.en);
    if (!match5) {
        goto skipMatch3;
    }
    match5 = false;
    lexer_Token lexer_token_3 = call33.u1.m0;
    lexer_PosString lexer_rest_6 = call33.u1.m1;
    match5 = true;
    skipMatch3:;
    if (match5) {
        lexer_Result adt13 = {1};
        adt13.u1.m0 = lexer_token_3;
        adt13.u1.m1.m0 = lexer_rest_6.m0;
        adt13.u1.m1.m1 = lexer_rest_6.m1;
        return adt13;
    }
    lexer_Result call34 = lexer_lexStringLiteral(lexer_str_18);
    bool match6 = (1 == call34.en);
    if (!match6) {
        goto skipMatch4;
    }
    match6 = false;
    lexer_Token lexer_token_4 = call34.u1.m0;
    lexer_PosString lexer_rest_7 = call34.u1.m1;
    match6 = true;
    skipMatch4:;
    if (match6) {
        lexer_Result adt14 = {1};
        adt14.u1.m0 = lexer_token_4;
        adt14.u1.m1.m0 = lexer_rest_7.m0;
        adt14.u1.m1.m1 = lexer_rest_7.m1;
        return adt14;
    }
    lexer_Result call35 = lexer_lexFloating(lexer_str_18);
    bool match7 = (1 == call35.en);
    if (!match7) {
        goto skipMatch5;
    }
    match7 = false;
    lexer_Token lexer_token_5 = call35.u1.m0;
    lexer_PosString lexer_rest_8 = call35.u1.m1;
    match7 = true;
    skipMatch5:;
    if (match7) {
        lexer_Result adt15 = {1};
        adt15.u1.m0 = lexer_token_5;
        adt15.u1.m1.m0 = lexer_rest_8.m0;
        adt15.u1.m1.m1 = lexer_rest_8.m1;
        return adt15;
    }
    lexer_Result call36 = lexer_lexInteger(lexer_str_18);
    bool match8 = (1 == call36.en);
    if (!match8) {
        goto skipMatch6;
    }
    match8 = false;
    lexer_Token lexer_token_6 = call36.u1.m0;
    lexer_PosString lexer_rest_9 = call36.u1.m1;
    match8 = true;
    skipMatch6:;
    if (match8) {
        lexer_Result adt16 = {1};
        adt16.u1.m0 = lexer_token_6;
        adt16.u1.m1.m0 = lexer_rest_9.m0;
        adt16.u1.m1.m1 = lexer_rest_9.m1;
        return adt16;
    }
    lexer_Result call37 = lexer_lexKeyword(lexer_str_18);
    bool match9 = (1 == call37.en);
    if (!match9) {
        goto skipMatch7;
    }
    match9 = false;
    lexer_Token lexer_token_7 = call37.u1.m0;
    lexer_PosString lexer_rest_10 = call37.u1.m1;
    match9 = true;
    skipMatch7:;
    if (match9) {
        lexer_Result adt17 = {1};
        adt17.u1.m0 = lexer_token_7;
        adt17.u1.m1.m0 = lexer_rest_10.m0;
        adt17.u1.m1.m1 = lexer_rest_10.m1;
        return adt17;
    }
    lexer_Result call38 = lexer_lexIdent(lexer_str_18);
    bool match10 = (1 == call38.en);
    if (!match10) {
        goto skipMatch8;
    }
    match10 = false;
    lexer_Token lexer_token_8 = call38.u1.m0;
    lexer_PosString lexer_rest_11 = call38.u1.m1;
    match10 = true;
    skipMatch8:;
    if (match10) {
        lexer_Result adt18 = {1};
        adt18.u1.m0 = lexer_token_8;
        adt18.u1.m1.m0 = lexer_rest_11.m0;
        adt18.u1.m1.m1 = lexer_rest_11.m1;
        return adt18;
    }
    lexer_Result call39 = lexer_lexComment(lexer_str_18);
    bool match11 = (1 == call39.en);
    if (!match11) {
        goto skipMatch9;
    }
    match11 = false;
    lexer_Token lexer_token_9 = call39.u1.m0;
    lexer_PosString lexer_rest_12 = call39.u1.m1;
    match11 = true;
    skipMatch9:;
    if (match11) {
        lexer_Result adt19 = {1};
        adt19.u1.m0 = lexer_token_9;
        adt19.u1.m1.m0 = lexer_rest_12.m0;
        adt19.u1.m1.m1 = lexer_rest_12.m1;
        return adt19;
    }
    lexer_Result call40 = lexer_lexSymbol(lexer_str_18);
    bool match12 = (1 == call40.en);
    if (!match12) {
        goto skipMatch10;
    }
    match12 = false;
    lexer_Token lexer_token_10 = call40.u1.m0;
    lexer_PosString lexer_rest_13 = call40.u1.m1;
    match12 = true;
    skipMatch10:;
    if (match12) {
        lexer_Result adt20 = {1};
        adt20.u1.m0 = lexer_token_10;
        adt20.u1.m1.m0 = lexer_rest_13.m0;
        adt20.u1.m1.m1 = lexer_rest_13.m1;
        return adt20;
    }
    lexer_Result call41 = lexer_lexCEmbed(lexer_str_18);
    bool match13 = (1 == call41.en);
    if (!match13) {
        goto skipMatch11;
    }
    match13 = false;
    lexer_Token lexer_token_11 = call41.u1.m0;
    lexer_PosString lexer_rest_14 = call41.u1.m1;
    match13 = true;
    skipMatch11:;
    if (match13) {
        lexer_Result adt21 = {1};
        adt21.u1.m0 = lexer_token_11;
        adt21.u1.m1.m0 = lexer_rest_14.m0;
        adt21.u1.m1.m1 = lexer_rest_14.m1;
        return adt21;
    }
    lexer_Result adt22 = {0};
    return adt22;
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

lexer_Result lexer_lexCEmbed(lexer_PosString lexer_str_15) {
    adt3 call42 = lexer_At(&(lexer_str_15), 0);
    bool matchNull4 = (1 == call42.en);
    if (!matchNull4) {
        goto matchSkip4;
    }
    matchNull4 = (call42.u1 == '$');
    matchSkip4:;
    bool match14 = matchNull4;
    if (match14) {
        adt3 call43 = lexer_At(&(lexer_str_15), 1);
        bool matchNull5 = (1 == call43.en);
        if (!matchNull5) {
            goto matchSkip5;
        }
        matchNull5 = (call43.u1 == '{');
        matchSkip5:;
        match14 = matchNull5;
    }
    if (match14) {
    }
    else {
        lexer_Result adt23 = {0};
        return adt23;
    }
    lexer_PosString call44 = lexer_Drop(lexer_str_15, 1);
    lexer_str_15.m0 = call44.m0;
    lexer_str_15.m1 = call44.m1;
    int64_t lexer_idx_10 = 1;
    int64_t lexer_level = 1;
    
    for (; ; ) {
        adt3 call45 = lexer_At(&(lexer_str_15), lexer_idx_10);
        bool matchNull6 = (1 == call45.en);
        if (!matchNull6) {
            goto matchSkip6;
        }
        char lexer_c_13 = call45.u1;
        matchNull6 = true;
        matchSkip6:;
        bool match15 = matchNull6;
        if (match15) {
            match15 = (lexer_level > 0);
        }
        if (!match15) {
            break;
        }
        char switchExpr0 = lexer_c_13;
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
        lexer_idx_10 = (lexer_idx_10 + 1);
    }
    if ((lexer_level != 0)) {
        lexer_Result adt24 = {0};
        return adt24;
    }
    table3 lexer_result = {0};
    int64_t idx1 = 0;
    bool first0 = true;
    
    for (; ; idx1++) {
        lexer_PosString call46 = lexer_Take(lexer_str_15, lexer_idx_10);
        if ((idx1 >= strlen(call46.m0))) {
            break;
        }
        char lexer_c_14 = call46.m0[idx1];
        char switchExpr1 = lexer_c_14;
        switch(0) {
            case 0: {
                if ((switchExpr1 == '\n')) {
                    array3 array2 = {(char)(31)};
                    table3 table4 = {1, 1, array2.arr};
                    lexer_table_append1(&(lexer_result), &(table4));
                    break;
                }
                if (true) {
                    array3 array4 = {lexer_c_14};
                    table3 table5 = {1, 1, array4.arr};
                    lexer_table_append1(&(lexer_result), &(table5));
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString lexer_str2 = lexer_str_15;
    char* call47 = doodad_string_copy(lexer_result.r0);
    lexer_str2.m0 = call47;
    lexer_Token adt25 = {8};
    adt25.u8.m0 = lexer_str2.m0;
    adt25.u8.m1 = lexer_str2.m1;
    lexer_PosString call48 = lexer_Drop(lexer_str_15, lexer_idx_10);
    lexer_Result adt26 = {1};
    adt26.u1.m0 = adt25;
    adt26.u1.m1.m0 = call48.m0;
    adt26.u1.m1.m1 = call48.m1;
    return adt26;
}

lexer_Result lexer_lexCharLiteral(lexer_PosString lexer_str_16) {
    lexer_PosString call49 = lexer_Take(lexer_str_16, 4);
    bool match16 = false;
    if ((4 != strlen(call49.m0))) {
        goto end0;
    }
    if ((call49.m0[0] != '\'')) {
        goto end0;
    }
    if ((call49.m0[1] != '\\')) {
        goto end0;
    }
    char lexer_c_15 = call49.m0[2];
    if ((call49.m0[3] != '\'')) {
        goto end0;
    }
    match16 = true;
    end0:;
    if (match16) {
        char switchExpr2 = lexer_c_15;
        switch(0) {
            case 0: {
                if ((switchExpr2 == 'n')) {
                    lexer_PosString call50 = lexer_Take(lexer_str_16, 4);
                    lexer_Token adt27 = {6};
                    adt27.u6.m0 = call50.m0;
                    adt27.u6.m1 = call50.m1;
                    lexer_PosString call51 = lexer_Drop(lexer_str_16, 4);
                    lexer_Result adt28 = {1};
                    adt28.u1.m0 = adt27;
                    adt28.u1.m1.m0 = call51.m0;
                    adt28.u1.m1.m1 = call51.m1;
                    return adt28;
                }
                if ((switchExpr2 == 't')) {
                    lexer_PosString call52 = lexer_Take(lexer_str_16, 4);
                    lexer_Token adt29 = {6};
                    adt29.u6.m0 = call52.m0;
                    adt29.u6.m1 = call52.m1;
                    lexer_PosString call53 = lexer_Drop(lexer_str_16, 4);
                    lexer_Result adt30 = {1};
                    adt30.u1.m0 = adt29;
                    adt30.u1.m1.m0 = call53.m0;
                    adt30.u1.m1.m1 = call53.m1;
                    return adt30;
                }
                if ((switchExpr2 == '0')) {
                    lexer_PosString call54 = lexer_Take(lexer_str_16, 4);
                    lexer_Token adt31 = {6};
                    adt31.u6.m0 = call54.m0;
                    adt31.u6.m1 = call54.m1;
                    lexer_PosString call55 = lexer_Drop(lexer_str_16, 4);
                    lexer_Result adt32 = {1};
                    adt32.u1.m0 = adt31;
                    adt32.u1.m1.m0 = call55.m0;
                    adt32.u1.m1.m1 = call55.m1;
                    return adt32;
                }
                if ((switchExpr2 == '\\')) {
                    lexer_PosString call56 = lexer_Take(lexer_str_16, 4);
                    lexer_Token adt33 = {6};
                    adt33.u6.m0 = call56.m0;
                    adt33.u6.m1 = call56.m1;
                    lexer_PosString call57 = lexer_Drop(lexer_str_16, 4);
                    lexer_Result adt34 = {1};
                    adt34.u1.m0 = adt33;
                    adt34.u1.m1.m0 = call57.m0;
                    adt34.u1.m1.m1 = call57.m1;
                    return adt34;
                }
                if ((switchExpr2 == '\'')) {
                    lexer_PosString call58 = lexer_Take(lexer_str_16, 4);
                    lexer_Token adt35 = {6};
                    adt35.u6.m0 = call58.m0;
                    adt35.u6.m1 = call58.m1;
                    lexer_PosString call59 = lexer_Drop(lexer_str_16, 4);
                    lexer_Result adt36 = {1};
                    adt36.u1.m0 = adt35;
                    adt36.u1.m1.m0 = call59.m0;
                    adt36.u1.m1.m1 = call59.m1;
                    return adt36;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
    }
    lexer_PosString call60 = lexer_Take(lexer_str_16, 3);
    char* switchExpr3 = call60.m0;
    switch(0) {
        case 0: {
            bool match17 = false;
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
            match17 = true;
            end1:;
            if (match17) {
                break;
            }
            bool match18 = false;
            if ((3 != strlen(switchExpr3))) {
                goto end2;
            }
            if ((switchExpr3[0] != '\'')) {
                goto end2;
            }
            char lexer_c_16 = switchExpr3[1];
            if ((switchExpr3[2] != '\'')) {
                goto end2;
            }
            match18 = true;
            end2:;
            if (match18) {
                lexer_PosString call61 = lexer_Take(lexer_str_16, 3);
                lexer_Token adt37 = {6};
                adt37.u6.m0 = call61.m0;
                adt37.u6.m1 = call61.m1;
                lexer_PosString call62 = lexer_Drop(lexer_str_16, 3);
                lexer_Result adt38 = {1};
                adt38.u1.m0 = adt37;
                adt38.u1.m1.m0 = call62.m0;
                adt38.u1.m1.m1 = call62.m1;
                return adt38;
            }
            if (true) {
                break;
            }
            assert(false);
        }
    }
    lexer_Result adt39 = {0};
    return adt39;
}

lexer_Result lexer_lexComment(lexer_PosString lexer_str_6) {
    adt3 call63 = lexer_At(&(lexer_str_6), 0);
    bool matchNull7 = (1 == call63.en);
    if (!matchNull7) {
        goto matchSkip7;
    }
    matchNull7 = (call63.u1 == '/');
    matchSkip7:;
    bool match19 = matchNull7;
    if (match19) {
        adt3 call64 = lexer_At(&(lexer_str_6), 1);
        bool matchNull8 = (1 == call64.en);
        if (!matchNull8) {
            goto matchSkip8;
        }
        matchNull8 = (call64.u1 == '/');
        matchSkip8:;
        match19 = matchNull8;
    }
    if (match19) {
        
        for (; ; ) {
            adt3 call65 = lexer_At(&(lexer_str_6), 0);
            bool matchNull9 = (1 == call65.en);
            if (!matchNull9) {
                goto matchSkip9;
            }
            char lexer_c_5 = call65.u1;
            matchNull9 = true;
            matchSkip9:;
            bool match20 = matchNull9;
            if (match20) {
                match20 = (lexer_c_5 != '\n');
            }
            if (!match20) {
                break;
            }
            lexer_PosString call66 = lexer_Drop(lexer_str_6, 1);
            lexer_str_6.m0 = call66.m0;
            lexer_str_6.m1 = call66.m1;
        }
        lexer_Token adt40 = {11};
        lexer_Result adt41 = {1};
        adt41.u1.m0 = adt40;
        adt41.u1.m1.m0 = lexer_str_6.m0;
        adt41.u1.m1.m1 = lexer_str_6.m1;
        return adt41;
    }
    lexer_Result adt42 = {0};
    return adt42;
}

adt4 lexer_lexComment_1(lexer_PosString lexer_str_12, int64_t lexer_idx_6) {
    adt3 call67 = lexer_At(&(lexer_str_12), lexer_idx_6);
    bool matchNull10 = (1 == call67.en);
    if (!matchNull10) {
        goto matchSkip10;
    }
    matchNull10 = (call67.u1 == '/');
    matchSkip10:;
    bool match21 = matchNull10;
    if (match21) {
        adt3 call68 = lexer_At(&(lexer_str_12), (lexer_idx_6 + 1));
        bool matchNull11 = (1 == call68.en);
        if (!matchNull11) {
            goto matchSkip11;
        }
        matchNull11 = (call68.u1 == '/');
        matchSkip11:;
        match21 = matchNull11;
    }
    if (match21) {
        lexer_idx_6 = (lexer_idx_6 + 2);
    }
    else {
        adt4 adt43 = {0};
        return adt43;
    }
    
    for (; ; ) {
        adt3 call69 = lexer_At(&(lexer_str_12), lexer_idx_6);
        bool matchNull12 = (1 == call69.en);
        if (!matchNull12) {
            goto matchSkip12;
        }
        char lexer_c_11 = call69.u1;
        matchNull12 = true;
        matchSkip12:;
        bool match22 = matchNull12;
        if (match22) {
            match22 = (lexer_c_11 != '\n');
        }
        if (!match22) {
            break;
        }
        lexer_idx_6 = (lexer_idx_6 + 1);
    }
    adt4 adt44 = {1};
    adt44.u1 = lexer_idx_6;
    return adt44;
}

void lexer_lexFile(char* lexer_fileNameIn, char* lexer_fileNameOut) {
    io_Io lexer_io_1 = {0};
    io_FileKey call70 = io_openFile(&(lexer_io_1), lexer_fileNameOut);
    io_FileKey lexer_key_1 = call70;
    char* call71 = io_readFile(&(lexer_io_1), lexer_fileNameIn);
    lexer_Pos tuple6 = {1, 1};
    lexer_PosString tuple7 = {call71, tuple6};
    lexer_PosString lexer_str_20 = tuple7;
    table1 lexer_indentStack = {0};
    array1 array5 = {""};
    table1 table6 = {1, 1, array5.arr};
    lexer_table_append0(&(lexer_indentStack), &(table6));
    
    for (; ; ) {
        lexer_Result call72 = lexer_lex_1(lexer_str_20);
        bool match23 = (1 == call72.en);
        if (!match23) {
            goto skipMatch12;
        }
        match23 = false;
        lexer_Token lexer_token_12 = call72.u1.m0;
        lexer_PosString lexer_rest_15 = call72.u1.m1;
        match23 = true;
        skipMatch12:;
        if (!match23) {
            break;
        }
        lexer_Token switchExpr4 = lexer_token_12;
        switch(0) {
            case 0: {
                bool match24 = (0 == switchExpr4.en);
                if (!match24) {
                    goto skipMatch13;
                }
                match24 = false;
                lexer_PosString lexer_s_1 = switchExpr4.u0;
                match24 = true;
                skipMatch13:;
                if (match24) {
                    char* call73 = doodad_string_i64(lexer_s_1.m1.m0);
                    char* call74 = doodad_string_plus(call73, ":");
                    char* call75 = doodad_string_i64(lexer_s_1.m1.m1);
                    char* call76 = doodad_string_plus(call74, call75);
                    char* call77 = doodad_string_plus(call76, ":ident: ");
                    char* call78 = doodad_string_plus(call77, lexer_s_1.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call78);
                    break;
                }
                bool match25 = (1 == switchExpr4.en);
                if (!match25) {
                    goto skipMatch14;
                }
                match25 = false;
                lexer_PosString lexer_s_2 = switchExpr4.u1;
                match25 = true;
                skipMatch14:;
                if (match25) {
                    char* call79 = doodad_string_i64(lexer_s_2.m1.m0);
                    char* call80 = doodad_string_plus(call79, ":");
                    char* call81 = doodad_string_i64(lexer_s_2.m1.m1);
                    char* call82 = doodad_string_plus(call80, call81);
                    char* call83 = doodad_string_plus(call82, ":keyword: ");
                    char* call84 = doodad_string_plus(call83, lexer_s_2.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call84);
                    break;
                }
                bool match26 = (2 == switchExpr4.en);
                if (!match26) {
                    goto skipMatch15;
                }
                match26 = false;
                lexer_PosString lexer_s_3 = switchExpr4.u2;
                match26 = true;
                skipMatch15:;
                if (match26) {
                    char* call85 = doodad_string_i64(lexer_s_3.m1.m0);
                    char* call86 = doodad_string_plus(call85, ":");
                    char* call87 = doodad_string_i64(lexer_s_3.m1.m1);
                    char* call88 = doodad_string_plus(call86, call87);
                    char* call89 = doodad_string_plus(call88, ":integer: ");
                    char* call90 = doodad_string_plus(call89, lexer_s_3.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call90);
                    break;
                }
                bool match27 = (3 == switchExpr4.en);
                if (!match27) {
                    goto skipMatch16;
                }
                match27 = false;
                lexer_PosString lexer_s_4 = switchExpr4.u3;
                match27 = true;
                skipMatch16:;
                if (match27) {
                    char* call91 = doodad_string_i64(lexer_s_4.m1.m0);
                    char* call92 = doodad_string_plus(call91, ":");
                    char* call93 = doodad_string_i64(lexer_s_4.m1.m1);
                    char* call94 = doodad_string_plus(call92, call93);
                    char* call95 = doodad_string_plus(call94, ":floating: ");
                    char* call96 = doodad_string_plus(call95, lexer_s_4.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call96);
                    break;
                }
                bool match28 = (5 == switchExpr4.en);
                if (!match28) {
                    goto skipMatch17;
                }
                match28 = false;
                lexer_PosString lexer_s_5 = switchExpr4.u5;
                match28 = true;
                skipMatch17:;
                if (match28) {
                    char* call97 = doodad_string_i64(lexer_s_5.m1.m0);
                    char* call98 = doodad_string_plus(call97, ":");
                    char* call99 = doodad_string_i64(lexer_s_5.m1.m1);
                    char* call100 = doodad_string_plus(call98, call99);
                    char* call101 = doodad_string_plus(call100, ":symbol: ");
                    char* call102 = doodad_string_plus(call101, lexer_s_5.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call102);
                    break;
                }
                bool match29 = (6 == switchExpr4.en);
                if (!match29) {
                    goto skipMatch18;
                }
                match29 = false;
                lexer_PosString lexer_s_6 = switchExpr4.u6;
                match29 = true;
                skipMatch18:;
                if (match29) {
                    char* call103 = doodad_string_i64(lexer_s_6.m1.m0);
                    char* call104 = doodad_string_plus(call103, ":");
                    char* call105 = doodad_string_i64(lexer_s_6.m1.m1);
                    char* call106 = doodad_string_plus(call104, call105);
                    char* call107 = doodad_string_plus(call106, ":char: ");
                    char* call108 = doodad_string_plus(call107, lexer_s_6.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call108);
                    break;
                }
                bool match30 = (7 == switchExpr4.en);
                if (!match30) {
                    goto skipMatch19;
                }
                match30 = false;
                lexer_PosString lexer_s_7 = switchExpr4.u7;
                match30 = true;
                skipMatch19:;
                if (match30) {
                    char* call109 = doodad_string_i64(lexer_s_7.m1.m0);
                    char* call110 = doodad_string_plus(call109, ":");
                    char* call111 = doodad_string_i64(lexer_s_7.m1.m1);
                    char* call112 = doodad_string_plus(call110, call111);
                    char* call113 = doodad_string_plus(call112, ":string: ");
                    char* call114 = doodad_string_plus(call113, lexer_s_7.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call114);
                    break;
                }
                bool match31 = (4 == switchExpr4.en);
                if (!match31) {
                    goto skipMatch20;
                }
                match31 = false;
                lexer_PosString lexer_s_8 = switchExpr4.u4;
                match31 = true;
                skipMatch20:;
                if (match31) {
                    lexer_indent(&(lexer_io_1), &(lexer_indentStack), lexer_key_1, lexer_s_8);
                    break;
                }
                bool match32 = (8 == switchExpr4.en);
                if (!match32) {
                    goto skipMatch21;
                }
                match32 = false;
                lexer_PosString lexer_s_9 = switchExpr4.u8;
                match32 = true;
                skipMatch21:;
                if (match32) {
                    char* call115 = doodad_string_i64(lexer_s_9.m1.m0);
                    char* call116 = doodad_string_plus(call115, ":");
                    char* call117 = doodad_string_i64(lexer_s_9.m1.m1);
                    char* call118 = doodad_string_plus(call116, call117);
                    char* call119 = doodad_string_plus(call118, ":cembed: ");
                    char* call120 = doodad_string_plus(call119, lexer_s_9.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call120);
                    break;
                }
                bool match33 = (9 == switchExpr4.en);
                if (!match33) {
                    goto skipMatch22;
                }
                match33 = false;
                lexer_PosString lexer_s_10 = switchExpr4.u9;
                match33 = true;
                skipMatch22:;
                if (match33) {
                    char* call121 = doodad_string_i64(lexer_s_10.m1.m0);
                    char* call122 = doodad_string_plus(call121, ":");
                    char* call123 = doodad_string_i64(lexer_s_10.m1.m1);
                    char* call124 = doodad_string_plus(call122, call123);
                    char* call125 = doodad_string_plus(call124, ":import: ");
                    char* call126 = doodad_string_plus(call125, lexer_s_10.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call126);
                    break;
                }
                bool match34 = (10 == switchExpr4.en);
                if (!match34) {
                    goto skipMatch23;
                }
                match34 = false;
                lexer_PosString lexer_s_11 = switchExpr4.u10;
                match34 = true;
                skipMatch23:;
                if (match34) {
                    char* call127 = doodad_string_i64(lexer_s_11.m1.m0);
                    char* call128 = doodad_string_plus(call127, ":");
                    char* call129 = doodad_string_i64(lexer_s_11.m1.m1);
                    char* call130 = doodad_string_plus(call128, call129);
                    char* call131 = doodad_string_plus(call130, ":include: ");
                    char* call132 = doodad_string_plus(call131, lexer_s_11.m0);
                    io_fPutStrLn(&(lexer_io_1), lexer_key_1, call132);
                    break;
                }
                bool match35 = (11 == switchExpr4.en);
                if (!match35) {
                    goto skipMatch24;
                }
                match35 = false;
                match35 = true;
                skipMatch24:;
                if (match35) {
                    break;
                }
                assert(false);
            }
        }
        lexer_str_20.m0 = lexer_rest_15.m0;
        lexer_str_20.m1 = lexer_rest_15.m1;
    }
    io_closeFile(&(lexer_io_1), lexer_key_1);
}

lexer_Result lexer_lexFloating(lexer_PosString lexer_str_10) {
    adt3 call133 = lexer_At(&(lexer_str_10), 0);
    bool matchNull13 = (1 == call133.en);
    if (!matchNull13) {
        goto matchSkip13;
    }
    char lexer_c_9 = call133.u1;
    matchNull13 = true;
    matchSkip13:;
    bool match36 = matchNull13;
    if (match36) {
        bool call134 = strings_isDigit(lexer_c_9);
        match36 = call134;
    }
    if (match36) {
    }
    else {
        lexer_Result adt45 = {0};
        return adt45;
    }
    int64_t lexer_idx_5 = 0;
    bool lexer_dot = false;
    
    for (; ; ) {
        adt3 call135 = lexer_At(&(lexer_str_10), lexer_idx_5);
        bool matchNull14 = (1 == call135.en);
        if (!matchNull14) {
            goto matchSkip14;
        }
        char lexer_c_10 = call135.u1;
        matchNull14 = true;
        matchSkip14:;
        bool match37 = matchNull14;
        if (match37) {
            bool call136 = strings_isDigit(lexer_c_10);
            match37 = (call136 || ((lexer_c_10 == '.') && !lexer_dot));
        }
        if (!match37) {
            break;
        }
        lexer_idx_5 = (lexer_idx_5 + 1);
        if ((lexer_c_10 == '.')) {
            lexer_dot = true;
        }
    }
    adt3 call137 = lexer_At(&(lexer_str_10), lexer_idx_5);
    bool matchNull15 = (1 == call137.en);
    if (!matchNull15) {
        goto matchSkip15;
    }
    matchNull15 = (call137.u1 == '.');
    matchSkip15:;
    if (matchNull15) {
        lexer_Result adt46 = {0};
        return adt46;
    }
    if (lexer_dot) {
        lexer_PosString call138 = lexer_Take(lexer_str_10, lexer_idx_5);
        lexer_Token adt47 = {3};
        adt47.u3.m0 = call138.m0;
        adt47.u3.m1 = call138.m1;
        lexer_PosString call139 = lexer_Drop(lexer_str_10, lexer_idx_5);
        lexer_Result adt48 = {1};
        adt48.u1.m0 = adt47;
        adt48.u1.m1.m0 = call139.m0;
        adt48.u1.m1.m1 = call139.m1;
        return adt48;
    }
    lexer_Result adt49 = {0};
    return adt49;
}

lexer_Result lexer_lexIdent(lexer_PosString lexer_str_3) {
    adt3 call140 = lexer_At(&(lexer_str_3), 0);
    bool matchNull16 = (1 == call140.en);
    if (!matchNull16) {
        goto matchSkip16;
    }
    char lexer_c_1 = call140.u1;
    matchNull16 = true;
    matchSkip16:;
    bool match38 = matchNull16;
    if (match38) {
        bool call141 = strings_isAlpha(lexer_c_1);
        match38 = call141;
    }
    if (match38) {
        int64_t lexer_idx_1 = 0;
        
        for (; ; ) {
            adt3 call142 = lexer_At(&(lexer_str_3), lexer_idx_1);
            bool matchNull17 = (1 == call142.en);
            if (!matchNull17) {
                goto matchSkip17;
            }
            char lexer_c_2 = call142.u1;
            matchNull17 = true;
            matchSkip17:;
            bool match39 = matchNull17;
            if (match39) {
                bool call143 = strings_isAlpha(lexer_c_2);
                bool call144 = strings_isDigit(lexer_c_2);
                match39 = ((call143 || call144) || (lexer_c_2 == '_'));
            }
            if (!match39) {
                break;
            }
            lexer_idx_1 = (lexer_idx_1 + 1);
        }
        lexer_PosString call145 = lexer_Take(lexer_str_3, lexer_idx_1);
        lexer_Token adt50 = {0};
        adt50.u0.m0 = call145.m0;
        adt50.u0.m1 = call145.m1;
        lexer_PosString call146 = lexer_Drop(lexer_str_3, lexer_idx_1);
        lexer_Result adt51 = {1};
        adt51.u1.m0 = adt50;
        adt51.u1.m1.m0 = call146.m0;
        adt51.u1.m1.m1 = call146.m1;
        return adt51;
    }
    lexer_Result adt52 = {0};
    return adt52;
}

lexer_Result lexer_lexImport(lexer_PosString lexer_str_4) {
    lexer_Result call147 = lexer_lexIdent(lexer_str_4);
    bool match40 = (1 == call147.en);
    if (!match40) {
        goto skipMatch25;
    }
    match40 = false;
    bool match41 = (0 == call147.u1.m0.en);
    if (!match41) {
        goto skipMatch26;
    }
    match41 = false;
    lexer_PosString lexer_ps = call147.u1.m0.u0;
    match41 = true;
    skipMatch26:;
    if (!match41) {
        goto skipMatch25;
    }
    lexer_PosString lexer_rest = call147.u1.m1;
    match40 = true;
    skipMatch25:;
    bool match42 = match40;
    if (match42) {
        bool call148 = doodad_string_eqeq(lexer_ps.m0, "import");
        match42 = call148;
    }
    if (match42) {
        lexer_str_4.m0 = lexer_rest.m0;
        lexer_str_4.m1 = lexer_rest.m1;
    }
    else {
        lexer_Result adt53 = {0};
        return adt53;
    }
    int64_t lexer_idx_2 = 0;
    
    for (; ; ) {
        adt3 call149 = lexer_At(&(lexer_str_4), lexer_idx_2);
        bool matchNull18 = (1 == call149.en);
        if (!matchNull18) {
            goto matchSkip18;
        }
        char lexer_c_3 = call149.u1;
        matchNull18 = true;
        matchSkip18:;
        bool match43 = matchNull18;
        if (match43) {
            match43 = (lexer_c_3 != '\n');
        }
        if (!match43) {
            break;
        }
        lexer_idx_2 = (lexer_idx_2 + 1);
    }
    lexer_PosString call150 = lexer_Take(lexer_str_4, lexer_idx_2);
    lexer_Token adt54 = {9};
    adt54.u9.m0 = call150.m0;
    adt54.u9.m1 = call150.m1;
    lexer_PosString call151 = lexer_Drop(lexer_str_4, lexer_idx_2);
    lexer_Result adt55 = {1};
    adt55.u1.m0 = adt54;
    adt55.u1.m1.m0 = call151.m0;
    adt55.u1.m1.m1 = call151.m1;
    return adt55;
}

lexer_Result lexer_lexInclude(lexer_PosString lexer_str_5) {
    lexer_Result call152 = lexer_lexIdent(lexer_str_5);
    bool match44 = (1 == call152.en);
    if (!match44) {
        goto skipMatch27;
    }
    match44 = false;
    bool match45 = (0 == call152.u1.m0.en);
    if (!match45) {
        goto skipMatch28;
    }
    match45 = false;
    lexer_PosString lexer_ps_1 = call152.u1.m0.u0;
    match45 = true;
    skipMatch28:;
    if (!match45) {
        goto skipMatch27;
    }
    lexer_PosString lexer_rest_1 = call152.u1.m1;
    match44 = true;
    skipMatch27:;
    bool match46 = match44;
    if (match46) {
        bool call153 = doodad_string_eqeq(lexer_ps_1.m0, "include");
        match46 = call153;
    }
    if (match46) {
        lexer_str_5.m0 = lexer_rest_1.m0;
        lexer_str_5.m1 = lexer_rest_1.m1;
    }
    else {
        lexer_Result adt56 = {0};
        return adt56;
    }
    int64_t lexer_idx_3 = 0;
    
    for (; ; ) {
        adt3 call154 = lexer_At(&(lexer_str_5), lexer_idx_3);
        bool matchNull19 = (1 == call154.en);
        if (!matchNull19) {
            goto matchSkip19;
        }
        char lexer_c_4 = call154.u1;
        matchNull19 = true;
        matchSkip19:;
        bool match47 = matchNull19;
        if (match47) {
            match47 = (lexer_c_4 != '\n');
        }
        if (!match47) {
            break;
        }
        lexer_idx_3 = (lexer_idx_3 + 1);
    }
    lexer_PosString call155 = lexer_Take(lexer_str_5, lexer_idx_3);
    lexer_Token adt57 = {10};
    adt57.u10.m0 = call155.m0;
    adt57.u10.m1 = call155.m1;
    lexer_PosString call156 = lexer_Drop(lexer_str_5, lexer_idx_3);
    lexer_Result adt58 = {1};
    adt58.u1.m0 = adt57;
    adt58.u1.m1.m0 = call156.m0;
    adt58.u1.m1.m1 = call156.m1;
    return adt58;
}

lexer_Result lexer_lexInteger(lexer_PosString lexer_str_9) {
    adt3 call157 = lexer_At(&(lexer_str_9), 0);
    bool matchNull20 = (1 == call157.en);
    if (!matchNull20) {
        goto matchSkip20;
    }
    char lexer_c_7 = call157.u1;
    matchNull20 = true;
    matchSkip20:;
    bool match48 = matchNull20;
    if (match48) {
        bool call158 = strings_isDigit(lexer_c_7);
        match48 = call158;
    }
    if (match48) {
        int64_t lexer_idx_4 = 0;
        
        for (; ; ) {
            adt3 call159 = lexer_At(&(lexer_str_9), lexer_idx_4);
            bool matchNull21 = (1 == call159.en);
            if (!matchNull21) {
                goto matchSkip21;
            }
            char lexer_c_8 = call159.u1;
            matchNull21 = true;
            matchSkip21:;
            bool match49 = matchNull21;
            if (match49) {
                bool call160 = strings_isDigit(lexer_c_8);
                match49 = call160;
            }
            if (!match49) {
                break;
            }
            lexer_idx_4 = (lexer_idx_4 + 1);
        }
        lexer_PosString call161 = lexer_Take(lexer_str_9, lexer_idx_4);
        lexer_Token adt59 = {2};
        adt59.u2.m0 = call161.m0;
        adt59.u2.m1 = call161.m1;
        lexer_PosString call162 = lexer_Drop(lexer_str_9, lexer_idx_4);
        lexer_Result adt60 = {1};
        adt60.u1.m0 = adt59;
        adt60.u1.m1.m0 = call162.m0;
        adt60.u1.m1.m1 = call162.m1;
        return adt60;
    }
    lexer_Result adt61 = {0};
    return adt61;
}

typedef struct { char* arr[27]; } array7;

lexer_Result lexer_lexKeyword(lexer_PosString lexer_str_7) {
    array7 array6 = {"module", "type", "fn", "for", "if", "else", "switch", "while", "return", "data", "const", "let", "null", "true", "false", "string", "bool", "char", "i8", "i16", "i32", "i64", "u8", "f32", "f64", "table", "integer"};
    array7 lexer_keywords = array6;
    lexer_Result call163 = lexer_lexIdent(lexer_str_7);
    bool match50 = (1 == call163.en);
    if (!match50) {
        goto skipMatch29;
    }
    match50 = false;
    bool match51 = (0 == call163.u1.m0.en);
    if (!match51) {
        goto skipMatch30;
    }
    match51 = false;
    lexer_PosString lexer_ps_2 = call163.u1.m0.u0;
    match51 = true;
    skipMatch30:;
    if (!match51) {
        goto skipMatch29;
    }
    lexer_PosString lexer_rest_2 = call163.u1.m1;
    match50 = true;
    skipMatch29:;
    if (match50) {
        int64_t idx3 = 0;
        bool first1 = true;
        
        for (; ; idx3++) {
            if ((idx3 >= 27)) {
                break;
            }
            char* lexer_keyword = lexer_keywords.arr[idx3];
            bool call164 = doodad_string_eqeq(lexer_keyword, lexer_ps_2.m0);
            if (call164) {
                lexer_Token adt62 = {1};
                adt62.u1.m0 = lexer_ps_2.m0;
                adt62.u1.m1 = lexer_ps_2.m1;
                lexer_Result adt63 = {1};
                adt63.u1.m0 = adt62;
                adt63.u1.m1.m0 = lexer_rest_2.m0;
                adt63.u1.m1.m1 = lexer_rest_2.m1;
                return adt63;
            }
        }
    }
    lexer_Result adt64 = {0};
    return adt64;
}

typedef struct { int64_t min; int64_t max; } range1;

lexer_Result lexer_lexNewline(lexer_PosString lexer_str_11) {
    adt3 call165 = lexer_At(&(lexer_str_11), 0);
    bool matchNull22 = (1 == call165.en);
    if (!matchNull22) {
        goto matchSkip22;
    }
    matchNull22 = (call165.u1 == '\n');
    matchSkip22:;
    if (matchNull22) {
    }
    else {
        lexer_Result adt65 = {0};
        return adt65;
    }
    int64_t lexer_idx_9 = 0;
    
    for (; ; ) {
        adt4 call166 = lexer_lex(lexer_str_11, lexer_idx_9);
        bool matchNull23 = (1 == call166.en);
        if (!matchNull23) {
            goto matchSkip23;
        }
        int64_t lexer_i_2 = call166.u1;
        matchNull23 = true;
        matchSkip23:;
        if (!matchNull23) {
            break;
        }
        lexer_idx_9 = lexer_i_2;
    }
    lexer_PosString call167 = lexer_Take(lexer_str_11, lexer_idx_9);
    lexer_PosString lexer_spaces = call167;
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
    lexer_PosString call168 = lexer_Drop(lexer_spaces, (lexer_lastNewline + 1));
    lexer_Token adt66 = {4};
    adt66.u4.m0 = call168.m0;
    adt66.u4.m1 = call168.m1;
    lexer_PosString call169 = lexer_Drop(lexer_str_11, lexer_idx_9);
    lexer_Result adt67 = {1};
    adt67.u1.m0 = adt66;
    adt67.u1.m1.m0 = call169.m0;
    adt67.u1.m1.m1 = call169.m1;
    return adt67;
}

adt4 lexer_lexSpaceChar(lexer_PosString lexer_str_13, int64_t lexer_idx_7) {
    adt3 call170 = lexer_At(&(lexer_str_13), lexer_idx_7);
    bool matchNull24 = (1 == call170.en);
    if (!matchNull24) {
        goto matchSkip24;
    }
    char lexer_c_12 = call170.u1;
    matchNull24 = true;
    matchSkip24:;
    bool match52 = matchNull24;
    if (match52) {
        match52 = (((lexer_c_12 == '\n') || (lexer_c_12 == '\t')) || (lexer_c_12 == ' '));
    }
    if (match52) {
        adt4 adt68 = {1};
        adt68.u1 = (lexer_idx_7 + 1);
        return adt68;
    }
    adt4 adt69 = {0};
    return adt69;
}

lexer_Result lexer_lexStringLiteral(lexer_PosString lexer_str_17) {
    adt3 call171 = lexer_At(&(lexer_str_17), 0);
    bool matchNull25 = (1 == call171.en);
    if (!matchNull25) {
        goto matchSkip25;
    }
    matchNull25 = (call171.u1 == '"');
    matchSkip25:;
    if (matchNull25) {
        int64_t lexer_idx_11 = 1;
        
        for (; ; ) {
            adt3 call172 = lexer_At(&(lexer_str_17), lexer_idx_11);
            bool matchNull26 = (1 == call172.en);
            if (!matchNull26) {
                goto matchSkip26;
            }
            char lexer_c_17 = call172.u1;
            matchNull26 = true;
            matchSkip26:;
            bool match53 = matchNull26;
            if (match53) {
                match53 = ((lexer_c_17 != '"') && (lexer_c_17 != '\n'));
            }
            if (!match53) {
                break;
            }
            lexer_idx_11 = (lexer_idx_11 + 1);
        }
        adt3 call173 = lexer_At(&(lexer_str_17), lexer_idx_11);
        adt3 switchExpr5 = call173;
        switch(0) {
            case 0: {
                bool matchNull27 = (0 == switchExpr5.en);
                if (matchNull27) {
                    lexer_Result adt70 = {0};
                    return adt70;
                }
                bool matchNull28 = (1 == switchExpr5.en);
                if (!matchNull28) {
                    goto matchSkip27;
                }
                char lexer_c_18 = switchExpr5.u1;
                matchNull28 = true;
                matchSkip27:;
                bool match54 = matchNull28;
                if (match54) {
                    match54 = (lexer_c_18 != '"');
                }
                if (match54) {
                    lexer_Result adt71 = {0};
                    return adt71;
                }
                if (true) {
                    break;
                }
                assert(false);
            }
        }
        lexer_PosString call174 = lexer_Take(lexer_str_17, lexer_idx_11);
        lexer_PosString call175 = lexer_Drop(call174, 1);
        lexer_Token adt72 = {7};
        adt72.u7.m0 = call175.m0;
        adt72.u7.m1 = call175.m1;
        lexer_PosString call176 = lexer_Drop(lexer_str_17, (lexer_idx_11 + 1));
        lexer_Result adt73 = {1};
        adt73.u1.m0 = adt72;
        adt73.u1.m1.m0 = call176.m0;
        adt73.u1.m1.m1 = call176.m1;
        return adt73;
    }
    lexer_Result adt74 = {0};
    return adt74;
}

typedef struct { char* arr[10]; } array9;

lexer_Result lexer_lexSymbol(lexer_PosString lexer_str_8) {
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
        int64_t call177 = strlen(lexer_symbol);
        lexer_PosString call178 = lexer_Take(lexer_str_8, call177);
        lexer_PosString lexer_x = call178;
        bool call179 = doodad_string_eqeq(lexer_x.m0, lexer_symbol);
        if (call179) {
            lexer_Token adt75 = {5};
            adt75.u5.m0 = lexer_x.m0;
            adt75.u5.m1 = lexer_x.m1;
            int64_t call180 = strlen(lexer_symbol);
            lexer_PosString call181 = lexer_Drop(lexer_str_8, call180);
            lexer_Result adt76 = {1};
            adt76.u1.m0 = adt75;
            adt76.u1.m1.m0 = call181.m0;
            adt76.u1.m1.m1 = call181.m1;
            return adt76;
        }
    }
    int64_t idx6 = 0;
    bool first4 = true;
    
    for (; ; idx6++) {
        if ((idx6 >= strlen(lexer_singles))) {
            break;
        }
        char lexer_symbol_1 = lexer_singles[idx6];
        adt3 call182 = lexer_At(&(lexer_str_8), 0);
        bool matchNull29 = (1 == call182.en);
        if (!matchNull29) {
            goto matchSkip28;
        }
        char lexer_c_6 = call182.u1;
        matchNull29 = true;
        matchSkip28:;
        bool match55 = matchNull29;
        if (match55) {
            match55 = (lexer_c_6 == lexer_symbol_1);
        }
        if (match55) {
            lexer_PosString call183 = lexer_Take(lexer_str_8, 1);
            lexer_Token adt77 = {5};
            adt77.u5.m0 = call183.m0;
            adt77.u5.m1 = call183.m1;
            lexer_PosString call184 = lexer_Drop(lexer_str_8, 1);
            lexer_Result adt78 = {1};
            adt78.u1.m0 = adt77;
            adt78.u1.m1.m0 = call184.m0;
            adt78.u1.m1.m1 = call184.m1;
            return adt78;
        }
    }
    lexer_Result adt79 = {0};
    return adt79;
}


void lexer_pop(table1* lexer_t) {
    {
            lexer_t->len--;
        }
}

