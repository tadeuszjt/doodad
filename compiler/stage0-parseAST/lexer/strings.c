/* Doodad Module: strings */
#include "doodad.h"

typedef struct { int64_t en; union { int8_t u0; char u1; } ; } adt0;
extern adt0 strings_at(char**, int64_t);
extern char* strings_drop(char*, int64_t);
extern bool strings_isAlpha(char);
extern bool strings_isDigit(char);
extern bool strings_isPrefix(char*, char*);
extern bool strings_isSpace(char);

typedef struct { int64_t en; union { int8_t u0; int64_t u1; } ; } adt1;

typedef struct { adt1 m0; char* m1; } tuple0;
extern tuple0 strings_readI64(char*);
extern char* strings_take(char*, int64_t);

adt0 strings_at(char** strings_s, int64_t strings_idx) {
    int64_t call0 = strlen((*strings_s));
    if (((strings_idx >= 0) && (strings_idx < call0))) {
        adt0 adt2 = {1};
        adt2.u1 = (*strings_s)[strings_idx];
        return adt2;
    }
    adt0 adt3 = {0};
    return adt3;
}

char* strings_drop(char* strings_s_1, int64_t strings_n) {
    char* strings_r_3 = "";
    {
            if (strings_n <= strlen(strings_s_1)) {
                strings_r_3 = strings_s_1 + strings_n;
            }
        }
    return strings_r_3;
}

bool strings_isAlpha(char strings_c) {
    bool strings_r = false;
    { strings_r = isalpha(strings_c); }
    return strings_r;
}

bool strings_isDigit(char strings_c_1) {
    bool strings_r_1 = false;
    { strings_r_1 = isdigit(strings_c_1); }
    return strings_r_1;
}

typedef struct { int64_t min; int64_t max; } range1;

bool strings_isPrefix(char* strings_pre, char* strings_str) {
    int64_t call1 = strlen(strings_pre);
    int64_t call2 = strlen(strings_str);
    if ((call1 > call2)) {
        return false;
    }
    int64_t idx0 = 0;
    bool first0 = true;
    
    for (; ; idx0++) {
        range1 range0 = {0, strlen(strings_pre)};
        if (first0) {
            idx0 = range0.min;
            first0 = false;
        }
        if ((idx0 >= range0.max)) {
            break;
        }
        int64_t strings_i = idx0;
        if ((strings_str[strings_i] != strings_pre[strings_i])) {
            return false;
        }
    }
    return true;
}

bool strings_isSpace(char strings_c_2) {
    bool strings_r_2 = false;
    { strings_r_2 = isspace(strings_c_2); }
    return strings_r_2;
}

typedef struct { int64_t len; int64_t cap; char* r0; } table0;

void strings_table_append0(table0* a, table0* b) {
    if (((a->len + b->len) >= a->cap)) {
        a->cap = ((a->len + b->len) * 2);
        void* mem0 = GC_malloc((a->cap * sizeof((*a->r0))));
        memcpy(mem0, a->r0, (a->len * sizeof((*a->r0))));
        a->r0 = mem0;
    }
    int64_t idx1 = 0;
    
    for (; (idx1 < b->len); idx1++) {
        a->r0[a->len] = b->r0[idx1];
        a->len++;
    }
}

typedef struct { char arr[1]; } array1;

tuple0 strings_readI64(char* strings_s_3) {
    adt1 adt4 = {0};
    tuple0 tuple1 = {adt4, strings_s_3};
    tuple0 strings_fail = tuple1;
    
    for (; ; ) {
        adt0 call3 = strings_at(&(strings_s_3), 0);
        bool matchNull0 = (1 == call3.en);
        if (!matchNull0) {
            goto matchSkip0;
        }
        char strings_c_3 = call3.u1;
        matchNull0 = true;
        matchSkip0:;
        bool match0 = matchNull0;
        if (match0) {
            bool call4 = strings_isSpace(strings_c_3);
            match0 = call4;
        }
        if (!match0) {
            break;
        }
        char* call5 = strings_drop(strings_s_3, 1);
        strings_s_3 = call5;
    }
    bool strings_isMinus = false;
    adt0 call6 = strings_at(&(strings_s_3), 0);
    bool matchNull1 = (1 == call6.en);
    if (!matchNull1) {
        goto matchSkip1;
    }
    matchNull1 = (call6.u1 == '-');
    matchSkip1:;
    if (matchNull1) {
        strings_isMinus = true;
        char* call7 = strings_drop(strings_s_3, 1);
        strings_s_3 = call7;
    }
    bool strings_hasDigits = false;
    int64_t strings_x = 0;
    table0 strings_digits = {0};
    
    for (; ; ) {
        adt0 call8 = strings_at(&(strings_s_3), 0);
        bool matchNull2 = (1 == call8.en);
        if (!matchNull2) {
            goto matchSkip2;
        }
        char strings_c_4 = call8.u1;
        matchNull2 = true;
        matchSkip2:;
        bool match1 = matchNull2;
        if (match1) {
            bool call9 = strings_isDigit(strings_c_4);
            match1 = call9;
        }
        if (!match1) {
            break;
        }
        strings_hasDigits = true;
        array1 array0 = {strings_c_4};
        table0 table1 = {1, 1, array0.arr};
        strings_table_append0(&(strings_digits), &(table1));
        char* call10 = strings_drop(strings_s_3, 1);
        strings_s_3 = call10;
    }
    int64_t idx2 = 0;
    bool first1 = true;
    
    for (; ; idx2++) {
        range1 range2 = {0, strings_digits.len};
        if (first1) {
            idx2 = range2.min;
            first1 = false;
        }
        if ((idx2 >= range2.max)) {
            break;
        }
        int64_t strings_i_1 = idx2;
        strings_x = ((strings_x * 10) + (strings_digits.r0[((strings_digits.len - strings_i_1) - 1)] - '0'));
    }
    if (!strings_hasDigits) {
        return strings_fail;
    }
    if (strings_isMinus) {
        strings_x = (0 - strings_x);
    }
    adt1 adt5 = {1};
    adt5.u1 = strings_x;
    tuple0 tuple2 = {adt5, strings_s_3};
    return tuple2;
}

char* strings_take(char* strings_s_2, int64_t strings_n_1) {
    char* strings_r_4 = "";
    {
            size_t len = strlen(strings_s_2);
            if (strings_n_1 >= len) {
                strings_r_4 = strings_s_2;
            } else {
                strings_r_4 = doodad_string_alloc(strings_n_1);
                memcpy(strings_r_4, strings_s_2, strings_n_1);
                strings_r_4[strings_n_1] = '\0';
            }
        }
    return strings_r_4;
}

