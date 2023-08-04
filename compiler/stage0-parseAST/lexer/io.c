/* Doodad Module: io */
#include "doodad.h"

typedef int64_t io_FileKey;

typedef int64_t io_FilePtr;

typedef struct { int64_t len; int64_t cap; io_FileKey* r0; io_FilePtr* r1; } table0;

typedef table0 io_Files;

typedef struct { io_FileKey m0; io_Files m1; } tuple0;

typedef tuple0 io_Io;
extern void assert_assert(bool);
extern void io_closeFile(io_Io*, io_FileKey);
extern void io_delete(io_Files*, int64_t);
extern void io_fPutStr(io_Io*, io_FileKey, char*);
extern void io_fPutStrLn(io_Io*, io_FileKey, char*);
extern char io_getChar(io_Io*);

typedef struct { int64_t en; union { int8_t u0; char* u1; } ; } adt0;
extern adt0 io_getStrLn(io_Io*);
extern io_FileKey io_newKey(io_Io*);
extern io_FileKey io_openFile(io_Io*, char*);
extern void io_putChar(io_Io*, char);
extern void io_putStr(io_Io*, char*);
extern void io_putStrLn(io_Io*, char*);
extern char* io_readFile(io_Io*, char*);

typedef struct { int64_t min; int64_t max; } range1;

typedef struct { int64_t len; int64_t cap; io_FileKey* r0; } table1;

typedef struct { int64_t len; int64_t cap; io_FilePtr* r0; } table2;

void io_closeFile(io_Io* io_io_2, io_FileKey io_key_2) {
    int64_t idx0 = 0;
    bool first0 = true;
    
    for (; ; idx0++) {
        range1 range0 = {0, (*io_io_2).m1.len};
        if (first0) {
            idx0 = range0.min;
            first0 = false;
        }
        if ((idx0 >= range0.max)) {
            break;
        }
        int64_t io_i = idx0;
        table1 row0 = {(*io_io_2).m1.len, (*io_io_2).m1.cap, (*io_io_2).m1.r0};
        if ((row0.r0[io_i] == io_key_2)) {
            table2 row1 = {(*io_io_2).m1.len, (*io_io_2).m1.cap, (*io_io_2).m1.r1};
            io_FilePtr io_ptr_1 = row1.r0[io_i];
            { fclose((FILE*)io_ptr_1); }
            io_delete(&((*io_io_2).m1), io_i);
            return;
        }
    }
    assert_assert(false);
}

typedef struct { io_FileKey m0; io_FilePtr m1; } tuple1;

void io_delete(io_Files* io_t, int64_t io_idx) {
    assert_assert(((io_idx >= 0) && (io_idx < (*io_t).len)));
    table1 row2 = {(*io_t).len, (*io_t).cap, (*io_t).r0};
    table2 row3 = {(*io_t).len, (*io_t).cap, (*io_t).r1};
    tuple1 subscr0 = {(*io_t).r0[((*io_t).len - 1)], (*io_t).r1[((*io_t).len - 1)]};
    row2.r0[io_idx] = subscr0.m0;
    row3.r0[io_idx] = subscr0.m1;
    { io_t->len--; }
}

void io_fPutStr(io_Io* io_io_3, io_FileKey io_key_3, char* io_s) {
    int64_t idx1 = 0;
    bool first1 = true;
    
    for (; ; idx1++) {
        if ((idx1 >= (*io_io_3).m1.len)) {
            break;
        }
        tuple1 subscr1 = {(*io_io_3).m1.r0[idx1], (*io_io_3).m1.r1[idx1]};
        bool match0 = false;
        io_FileKey io_k = subscr1.m0;
        io_FilePtr io_p = subscr1.m1;
        match0 = true;
        end0:;
        if (!match0) {
            break;
        }
        if ((io_k == io_key_3)) {
            { fprintf((FILE*)io_p, "%s", io_s); }
            return;
        }
    }
    assert_assert(false);
}

void io_fPutStrLn(io_Io* io_io_4, io_FileKey io_key_4, char* io_s_1) {
    int64_t idx2 = 0;
    bool first2 = true;
    
    for (; ; idx2++) {
        if ((idx2 >= (*io_io_4).m1.len)) {
            break;
        }
        tuple1 subscr2 = {(*io_io_4).m1.r0[idx2], (*io_io_4).m1.r1[idx2]};
        bool match1 = false;
        io_FileKey io_k_1 = subscr2.m0;
        io_FilePtr io_p_1 = subscr2.m1;
        match1 = true;
        end1:;
        if (!match1) {
            break;
        }
        if ((io_k_1 == io_key_4)) {
            { fprintf((FILE*)io_p_1, "%s\n", io_s_1); }
            return;
        }
    }
    assert_assert(false);
}

char io_getChar(io_Io* io_io_8) {
    char zero0 = {0};
    char io_c_1 = zero0;
    { io_c_1 = getchar(); }
    return io_c_1;
}

adt0 io_getStrLn(io_Io* io_io_9) {
    bool io_eof = false;
    char* io_s_4 = "";
    {
            char buffer[1024];
            char *ret = fgets(buffer, sizeof(buffer), stdin);
            int len = strlen(buffer);
            if (ret == NULL) {
                io_eof = true;
            } else {
                if (buffer[len-1] == '\n') {
                    buffer[len-1] = '\0';
                }
                io_s_4 = doodad_string_copy(buffer);
            }
        }
    if (io_eof) {
        adt0 adt1 = {0};
        return adt1;
    }
    adt0 adt2 = {1};
    adt2.u1 = io_s_4;
    return adt2;
}

io_FileKey io_newKey(io_Io* io_io) {
    io_FileKey io_key = io_io->m0;
    (*io_io).m0 = ((*io_io).m0 + 1);
    return io_key;
}

void io_table_append0(io_Files* a, io_Files* b) {
    if (((a->len + b->len) >= a->cap)) {
        a->cap = ((a->len + b->len) * 2);
        void* mem0 = GC_malloc((a->cap * sizeof((*a->r0))));
        void* mem1 = GC_malloc((a->cap * sizeof((*a->r1))));
        memcpy(mem0, a->r0, (a->len * sizeof((*a->r0))));
        memcpy(mem1, a->r1, (a->len * sizeof((*a->r1))));
        a->r0 = mem0;
        a->r1 = mem1;
    }
    int64_t idx3 = 0;
    
    for (; (idx3 < b->len); idx3++) {
        a->r0[a->len] = b->r0[idx3];
        a->r1[a->len] = b->r1[idx3];
        a->len++;
    }
}

typedef struct { io_FileKey arr[1]; } array1;

typedef struct { io_FilePtr arr[1]; } array3;

io_FileKey io_openFile(io_Io* io_io_1, char* io_filename) {
    io_FileKey call0 = io_newKey(io_io_1);
    io_FileKey io_key_1 = call0;
    io_FilePtr io_ptr = 0;
    {
            FILE *fp = fopen(io_filename, "w+");
            assert(fp != NULL);
            io_ptr = (int64_t)(fp);
        }
    array1 array0 = {io_key_1};
    table1 table3 = {1, 1, array0.arr};
    array3 array2 = {io_ptr};
    table2 table4 = {1, 1, array2.arr};
    array1 array4 = {io_key_1};
    table1 table5 = {1, 1, array4.arr};
    array3 array5 = {io_ptr};
    table2 table6 = {1, 1, array5.arr};
    io_Files table7 = {table3.len, table3.cap, table5.r0, table6.r0};
    io_table_append0(&((*io_io_1).m1), &(table7));
    return io_key_1;
}

void io_putChar(io_Io* io_io_7, char io_c) {
    { putchar(io_c); }
}

void io_putStr(io_Io* io_io_6, char* io_s_3) {
    { printf("%s", io_s_3); }
}

void io_putStrLn(io_Io* io_io_5, char* io_s_2) {
    { printf("%s\n", io_s_2); }
}

char* io_readFile(io_Io* io_io_10, char* io_filename_1) {
    char* io_contents = "";
    {
            FILE *fp = fopen(io_filename_1, "r");
            assert(fp != NULL);
    
            fseek(fp, 0, SEEK_END);
            int64_t fileSize = ftell(fp);
            fseek(fp, 0, SEEK_SET);
    
            io_contents = doodad_string_alloc(fileSize);
            fread(io_contents, 1, fileSize, fp);
            fclose(fp);
        }
    return io_contents;
}

