; ModuleID = '<string>'
source_filename = "<string>"

@lexer.str = unnamed_addr constant [3 x i8] c"==\00"
@lexer.str_1 = unnamed_addr constant [3 x i8] c"!=\00"
@lexer.str_2 = unnamed_addr constant [3 x i8] c">=\00"
@lexer.str_3 = unnamed_addr constant [3 x i8] c"<=\00"
@lexer.str_4 = unnamed_addr constant [3 x i8] c"||\00"
@lexer.str_5 = unnamed_addr constant [3 x i8] c"&&\00"
@lexer.str_6 = unnamed_addr constant [3 x i8] c"..\00"
@lexer.str_7 = unnamed_addr constant [3 x i8] c"->\00"
@lexer.str_8 = unnamed_addr constant [3 x i8] c"::\00"
@lexer.str_9 = unnamed_addr constant [1 x i8] zeroinitializer
@lexer.str_10 = unnamed_addr constant [31 x i8] c"-/<>[]{}().,;:|=!?@#$%^&*()_+ \00"
@lexer.str_11 = unnamed_addr constant [7 x i8] c"import\00"
@lexer.str_12 = unnamed_addr constant [9 x i8] c"import_c\00"
@lexer.str_13 = unnamed_addr constant [3 x i8] c"fn\00"
@lexer.str_14 = unnamed_addr constant [5 x i8] c"type\00"
@lexer.str_15 = unnamed_addr constant [3 x i8] c"if\00"
@lexer.str_16 = unnamed_addr constant [5 x i8] c"else\00"
@lexer.str_17 = unnamed_addr constant [4 x i8] c"let\00"
@lexer.str_18 = unnamed_addr constant [6 x i8] c"while\00"
@lexer.str_19 = unnamed_addr constant [7 x i8] c"return\00"
@lexer.str_20 = unnamed_addr constant [7 x i8] c"switch\00"
@lexer.str_21 = unnamed_addr constant [5 x i8] c"true\00"
@lexer.str_22 = unnamed_addr constant [6 x i8] c"false\00"
@lexer.str_23 = unnamed_addr constant [7 x i8] c"module\00"
@lexer.str_24 = unnamed_addr constant [4 x i8] c"for\00"
@lexer.str_25 = unnamed_addr constant [5 x i8] c"null\00"
@lexer.str_26 = unnamed_addr constant [5 x i8] c"data\00"
@lexer.str_27 = unnamed_addr constant [3 x i8] c"\5Cn\00"
@lexer.str_28 = unnamed_addr constant [3 x i8] c"\5Ct\00"
@lexer.str_29 = unnamed_addr constant [3 x i8] c"\5C0\00"
@lexer.str_30 = unnamed_addr constant [3 x i8] c"\5C\22\00"
@lexer.str_31 = unnamed_addr constant [3 x i8] c"\5C\5C\00"
@lexer.str_32 = unnamed_addr constant [22 x i8] c"+-*/%<>[]{}().,;:|=_!\00"
@lexer.str_33 = unnamed_addr constant [3 x i8] c"i8\00"
@lexer.str_34 = unnamed_addr constant [4 x i8] c"i16\00"
@lexer.str_35 = unnamed_addr constant [4 x i8] c"i32\00"
@lexer.str_36 = unnamed_addr constant [4 x i8] c"i64\00"
@lexer.str_37 = unnamed_addr constant [4 x i8] c"f32\00"
@lexer.str_38 = unnamed_addr constant [4 x i8] c"f64\00"
@lexer.str_39 = unnamed_addr constant [5 x i8] c"bool\00"
@lexer.str_40 = unnamed_addr constant [5 x i8] c"char\00"
@lexer.str_41 = unnamed_addr constant [7 x i8] c"string\00"
@lexer.str_42 = unnamed_addr constant [7 x i8] c"sparse\00"
@lexer.str_43 = unnamed_addr constant [4 x i8] c"map\00"
@lexer.str_44 = unnamed_addr constant [6 x i8] c"chr: \00"
@lexer.str_45 = unnamed_addr constant [6 x i8] c"sym: \00"
@lexer.str_46 = unnamed_addr constant [6 x i8] c"int: \00"
@lexer.str_47 = unnamed_addr constant [6 x i8] c"flt: \00"
@lexer.str_48 = unnamed_addr constant [6 x i8] c"imp: \00"
@lexer.str_49 = unnamed_addr constant [6 x i8] c"imc: \00"
@lexer.str_50 = unnamed_addr constant [6 x i8] c"idt: \00"
@lexer.str_51 = unnamed_addr constant [6 x i8] c"kwd: \00"
@lexer.str_52 = unnamed_addr constant [6 x i8] c"typ: \00"
@lexer.str_53 = unnamed_addr constant [6 x i8] c"str: \00"
@lexer.str_54 = unnamed_addr constant [3 x i8] c"\5C'\00"
@lexer.str_55 = unnamed_addr constant [6 x i8] c"nln: \00"
@lexer.str_56 = unnamed_addr constant [17 x i8] c"<unknown lexeme>\00"
@lexer.str_57 = unnamed_addr constant [7 x i8] c"%-.*s\0A\00"

declare i1 @chars.isAlpha(i8) local_unnamed_addr

declare i1 @chars.isDigit(i8) local_unnamed_addr

declare void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }*, i8) local_unnamed_addr

declare void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }*, i64) local_unnamed_addr

declare void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }*, { i64, i64, i8* }*) local_unnamed_addr

declare void @llvm.trap()

; Function Attrs: norecurse nounwind readonly
define { i64, i8 } @lexer.at({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %idx, -1
  %4 = icmp sgt i64 %2, %idx
  %5 = and i1 %3, %4
  br i1 %5, label %when_true_0, label %if_exit_0

when_true_0:                                      ; preds = %0
  %6 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %7 = load i8*, i8** %6, align 8
  %8 = getelementptr i8, i8* %7, i64 %idx
  %9 = load i8, i8* %8, align 1
  %10 = icmp eq i8 %9, 0
  br i1 %10, label %if_exit_0, label %if_true_0

if_true_0:                                        ; preds = %when_true_0
  %11 = alloca { i64, i8 }, align 8
  %12 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %11, i64 0, i32 0
  store i64 1, i64* %12, align 8
  %13 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %11, i64 0, i32 1
  store i8 %9, i8* %13, align 8
  %14 = load { i64, i8 }, { i64, i8 }* %11, align 8
  ret { i64, i8 } %14

if_exit_0:                                        ; preds = %when_true_0, %0
  %15 = alloca { i64, i8 }, align 8
  %16 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %15, i64 0, i32 0
  store i64 0, i64* %16, align 8
  %17 = load { i64, i8 }, { i64, i8 }* %15, align 8
  ret { i64, i8 } %17
}

define { i64, { { i64, i64 }, i64 } } @lexer.lex({ i64, i64, i8* }* %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = extractvalue { i64, i8 } %1, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_true_0, label %while_exit_0

when_true_0:                                      ; preds = %0, %when_exit_0
  %5 = phi { i64, i8 }* [ %10, %when_exit_0 ], [ %2, %0 ]
  %.0.ph495 = phi i64 [ %8, %when_exit_0 ], [ %idx, %0 ]
  %6 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %5, i64 0, i32 1
  %7 = load i8, i8* %6, align 8
  switch i8 %7, label %while_exit_0 [
    i8 32, label %when_exit_0
    i8 9, label %when_exit_0
  ]

when_exit_0:                                      ; preds = %when_true_0, %when_true_0
  %8 = add i64 %.0.ph495, 1
  %9 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %8)
  %10 = alloca { i64, i8 }, align 8
  store { i64, i8 } %9, { i64, i8 }* %10, align 8
  %11 = extractvalue { i64, i8 } %9, 0
  %12 = icmp eq i64 %11, 1
  br i1 %12, label %when_true_0, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_0, %when_true_0, %0
  %.0.ph.lcssa = phi i64 [ %idx, %0 ], [ %8, %when_exit_0 ], [ %.0.ph495, %when_true_0 ]
  %13 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexNewline({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %13, 0
  %14 = icmp eq i64 %.elt, 1
  br i1 %14, label %when_true_2, label %when_exit_2

when_true_2:                                      ; preds = %while_exit_0
  %.elt2 = extractvalue { i64, { { i64, i64 }, i64 } } %13, 1
  %15 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt2, 1
  ret { i64, { { i64, i64 }, i64 } } %15

when_exit_2:                                      ; preds = %while_exit_0
  %16 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexStringLit({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt17 = extractvalue { i64, { { i64, i64 }, i64 } } %16, 0
  %17 = icmp eq i64 %.elt17, 1
  br i1 %17, label %when_true_3, label %when_exit_3

when_true_3:                                      ; preds = %when_exit_2
  %.elt19 = extractvalue { i64, { { i64, i64 }, i64 } } %16, 1
  %18 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt19, 1
  ret { i64, { { i64, i64 }, i64 } } %18

when_exit_3:                                      ; preds = %when_exit_2
  %19 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexType({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt35 = extractvalue { i64, { { i64, i64 }, i64 } } %19, 0
  %20 = icmp eq i64 %.elt35, 1
  br i1 %20, label %when_true_4, label %when_exit_4

when_true_4:                                      ; preds = %when_exit_3
  %.elt37 = extractvalue { i64, { { i64, i64 }, i64 } } %19, 1
  %21 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt37, 1
  ret { i64, { { i64, i64 }, i64 } } %21

when_exit_4:                                      ; preds = %when_exit_3
  %22 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexImport({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt53 = extractvalue { i64, { { i64, i64 }, i64 } } %22, 0
  %23 = icmp eq i64 %.elt53, 1
  br i1 %23, label %when_true_5, label %when_exit_5

when_true_5:                                      ; preds = %when_exit_4
  %.elt55 = extractvalue { i64, { { i64, i64 }, i64 } } %22, 1
  %24 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt55, 1
  ret { i64, { { i64, i64 }, i64 } } %24

when_exit_5:                                      ; preds = %when_exit_4
  %25 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexKeyword({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt71 = extractvalue { i64, { { i64, i64 }, i64 } } %25, 0
  %26 = icmp eq i64 %.elt71, 1
  br i1 %26, label %when_true_6, label %when_exit_6

when_true_6:                                      ; preds = %when_exit_5
  %.elt73 = extractvalue { i64, { { i64, i64 }, i64 } } %25, 1
  %27 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt73, 1
  ret { i64, { { i64, i64 }, i64 } } %27

when_exit_6:                                      ; preds = %when_exit_5
  %28 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt89 = extractvalue { i64, { { i64, i64 }, i64 } } %28, 0
  %29 = icmp eq i64 %.elt89, 1
  br i1 %29, label %when_true_7, label %when_exit_7

when_true_7:                                      ; preds = %when_exit_6
  %.elt91 = extractvalue { i64, { { i64, i64 }, i64 } } %28, 1
  %30 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt91, 1
  ret { i64, { { i64, i64 }, i64 } } %30

when_exit_7:                                      ; preds = %when_exit_6
  %31 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexFloat({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt107 = extractvalue { i64, { { i64, i64 }, i64 } } %31, 0
  %32 = icmp eq i64 %.elt107, 1
  br i1 %32, label %when_true_8, label %when_exit_8

when_true_8:                                      ; preds = %when_exit_7
  %.elt109 = extractvalue { i64, { { i64, i64 }, i64 } } %31, 1
  %33 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt109, 1
  ret { i64, { { i64, i64 }, i64 } } %33

when_exit_8:                                      ; preds = %when_exit_7
  %34 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexInt({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt125 = extractvalue { i64, { { i64, i64 }, i64 } } %34, 0
  %35 = icmp eq i64 %.elt125, 1
  br i1 %35, label %when_true_9, label %when_exit_9

when_true_9:                                      ; preds = %when_exit_8
  %.elt127 = extractvalue { i64, { { i64, i64 }, i64 } } %34, 1
  %36 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt127, 1
  ret { i64, { { i64, i64 }, i64 } } %36

when_exit_9:                                      ; preds = %when_exit_8
  %37 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexDoubleSym({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt143 = extractvalue { i64, { { i64, i64 }, i64 } } %37, 0
  %38 = icmp eq i64 %.elt143, 1
  br i1 %38, label %when_true_10, label %when_exit_10

when_true_10:                                     ; preds = %when_exit_9
  %.elt145 = extractvalue { i64, { { i64, i64 }, i64 } } %37, 1
  %39 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt145, 1
  ret { i64, { { i64, i64 }, i64 } } %39

when_exit_10:                                     ; preds = %when_exit_9
  %40 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexSym({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt161 = extractvalue { i64, { { i64, i64 }, i64 } } %40, 0
  %41 = icmp eq i64 %.elt161, 1
  br i1 %41, label %when_true_11, label %when_exit_11

when_true_11:                                     ; preds = %when_exit_10
  %.elt163 = extractvalue { i64, { { i64, i64 }, i64 } } %40, 1
  %42 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt163, 1
  ret { i64, { { i64, i64 }, i64 } } %42

when_exit_11:                                     ; preds = %when_exit_10
  %43 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexCharLit({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %.elt179 = extractvalue { i64, { { i64, i64 }, i64 } } %43, 0
  %44 = icmp eq i64 %.elt179, 1
  br i1 %44, label %when_true_12, label %when_exit_12

when_true_12:                                     ; preds = %when_exit_11
  %.elt181 = extractvalue { i64, { { i64, i64 }, i64 } } %43, 1
  %45 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt181, 1
  ret { i64, { { i64, i64 }, i64 } } %45

when_exit_12:                                     ; preds = %when_exit_11
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexCharLit({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %.fca.1.extract = extractvalue { i64, i8 } %1, 1
  %2 = extractvalue { i64, i8 } %1, 0
  %3 = icmp eq i64 %2, 1
  %4 = icmp eq i8 %.fca.1.extract, 39
  %or.cond = and i1 %3, %4
  br i1 %or.cond, label %when_true_1, label %when_exit_0

when_true_1:                                      ; preds = %0
  %5 = add i64 %idx, 1
  %6 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexEscaped({ i64, i64, i8* }* %s, i64 %5)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %6, 0
  %.elt25 = extractvalue { i64, { { i64, i64 }, i64 } } %6, 1
  %.elt25.elt = extractvalue { { i64, i64 }, i64 } %.elt25, 0
  %7 = icmp eq i64 %.elt, 1
  br i1 %7, label %when_true_13, label %when_true_3

when_true_3:                                      ; preds = %when_true_1
  %8 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %5)
  %9 = add i64 %idx, 2
  %10 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %9)
  %11 = alloca { { i64, i8 }, { i64, i8 } }, align 8
  %12 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 0
  %13 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 1
  store { i64, i8 } %8, { i64, i8 }* %12, align 8
  store { i64, i8 } %10, { i64, i8 }* %13, align 4
  %14 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 0, i32 0
  %15 = load i64, i64* %14, align 8
  %16 = icmp eq i64 %15, 1
  br i1 %16, label %when_true_4, label %when_true_8

when_true_4:                                      ; preds = %when_true_3
  %17 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 0, i32 1
  %18 = load i8, i8* %17, align 8
  %19 = icmp eq i8 %18, 92
  br i1 %19, label %when_true_5, label %when_true_8

when_true_5:                                      ; preds = %when_true_4
  %20 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 1, i32 0
  %21 = load i64, i64* %20, align 4
  %22 = icmp eq i64 %21, 1
  br i1 %22, label %when_true_6, label %when_true_8

when_true_6:                                      ; preds = %when_true_5
  %23 = getelementptr inbounds { { i64, i8 }, { i64, i8 } }, { { i64, i8 }, { i64, i8 } }* %11, i64 0, i32 1, i32 1
  %24 = load i8, i8* %23, align 4
  %25 = icmp eq i8 %24, 39
  br i1 %25, label %when_exit_4, label %when_true_8

when_exit_4:                                      ; preds = %when_true_6
  %26 = alloca { i64, i64 }, align 8
  %27 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %26, i64 0, i32 0
  store i64 0, i64* %27, align 8
  %28 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %26, i64 0, i32 1
  %29 = bitcast i64* %28 to i8*
  store i8 39, i8* %29, align 8
  %30 = add i64 %idx, 3
  %.unpack152 = load i64, i64* %28, align 8
  %31 = trunc i64 %.unpack152 to i8
  br label %when_true_14

when_true_8:                                      ; preds = %when_true_5, %when_true_6, %when_true_4, %when_true_3
  %32 = alloca { i64, i8 }, align 8
  store { i64, i8 } %8, { i64, i8 }* %32, align 8
  %33 = extractvalue { i64, i8 } %8, 0
  %34 = icmp eq i64 %33, 1
  br i1 %34, label %when_true_9, label %when_true_11

when_true_9:                                      ; preds = %when_true_8
  %35 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %32, i64 0, i32 1
  %36 = load i8, i8* %35, align 8
  %37 = tail call i1 @chars.isAlpha(i8 %36)
  %38 = tail call i1 @chars.isDigit(i8 %36)
  %39 = or i1 %37, %38
  %40 = icmp eq i8 %36, 34
  %41 = or i1 %40, %39
  br i1 %41, label %when_exit_9, label %when_true_11

when_exit_9:                                      ; preds = %when_true_9
  %42 = alloca { i64, i64 }, align 8
  %43 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %42, i64 0, i32 0
  store i64 0, i64* %43, align 8
  %44 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %42, i64 0, i32 1
  %45 = bitcast i64* %44 to i8*
  store i8 %36, i8* %45, align 8
  %.unpack126 = load i64, i64* %44, align 8
  %46 = trunc i64 %.unpack126 to i8
  br label %when_true_14

when_true_11:                                     ; preds = %when_true_9, %when_true_8
  %47 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexGraphic({ i64, i64, i8* }* %s, i64 %5)
  %.elt72 = extractvalue { i64, { { i64, i64 }, i64 } } %47, 0
  %.elt74 = extractvalue { i64, { { i64, i64 }, i64 } } %47, 1
  %.elt74.elt = extractvalue { { i64, i64 }, i64 } %.elt74, 0
  %48 = icmp eq i64 %.elt72, 1
  br i1 %48, label %when_true_13, label %when_exit_0

when_true_13:                                     ; preds = %when_true_11, %when_true_1
  %.elt25.elt.sink232 = phi { i64, i64 } [ %.elt25.elt, %when_true_1 ], [ %.elt74.elt, %when_true_11 ]
  %.elt25.sink = phi { { i64, i64 }, i64 } [ %.elt25, %when_true_1 ], [ %.elt74, %when_true_11 ]
  %.elt25.elt.elt = extractvalue { i64, i64 } %.elt25.elt.sink232, 0
  %.elt25.elt.elt29 = extractvalue { i64, i64 } %.elt25.elt.sink232, 1
  %.elt25.elt27 = extractvalue { { i64, i64 }, i64 } %.elt25.sink, 1
  %49 = trunc i64 %.elt25.elt.elt29 to i8
  %50 = icmp eq i64 %.elt25.elt.elt, 0
  br i1 %50, label %when_true_14, label %when_exit_0

when_true_14:                                     ; preds = %when_exit_9, %when_exit_4, %when_true_13
  %51 = phi i8 [ %49, %when_true_13 ], [ %31, %when_exit_4 ], [ %46, %when_exit_9 ]
  %52 = phi i64 [ %.elt25.elt27, %when_true_13 ], [ %30, %when_exit_4 ], [ %9, %when_exit_9 ]
  %53 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %52)
  %54 = alloca { i64, i8 }, align 8
  store { i64, i8 } %53, { i64, i8 }* %54, align 8
  %55 = extractvalue { i64, i8 } %53, 0
  %56 = icmp eq i64 %55, 1
  br i1 %56, label %when_true_15, label %when_exit_0

when_true_15:                                     ; preds = %when_true_14
  %57 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %54, i64 0, i32 1
  %58 = load i8, i8* %57, align 8
  %59 = icmp eq i8 %58, 39
  br i1 %59, label %when_true_16, label %when_exit_0

when_true_16:                                     ; preds = %when_true_15
  %60 = alloca { i64, i64 }, align 8
  %61 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %60, i64 0, i32 0
  store i64 9, i64* %61, align 8
  %62 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %60, i64 0, i32 1
  %63 = bitcast i64* %62 to i8*
  store i8 %51, i8* %63, align 8
  %64 = add i64 %52, 1
  %.unpack56 = load i64, i64* %62, align 8
  %.unpack64.unpack70 = insertvalue { i64, i64 } { i64 9, i64 undef }, i64 %.unpack56, 1
  %65 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack64.unpack70, 0
  %.unpack6467 = insertvalue { { i64, i64 }, i64 } %65, i64 %64, 1
  %66 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack6467, 1
  ret { i64, { { i64, i64 }, i64 } } %66

when_exit_0:                                      ; preds = %when_true_11, %when_true_13, %when_true_15, %when_true_14, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

; Function Attrs: norecurse nounwind readonly
define { i64, { { i64, i64 }, i64 } } @lexer.lexComment({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr #0 {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %.fca.1.extract = extractvalue { i64, i8 } %1, 1
  %2 = extractvalue { i64, i8 } %1, 0
  %3 = icmp eq i64 %2, 1
  %4 = icmp eq i8 %.fca.1.extract, 47
  %or.cond = and i1 %3, %4
  br i1 %or.cond, label %when_true_1, label %when_exit_0

when_true_1:                                      ; preds = %0
  %5 = add i64 %idx, 1
  %6 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %5)
  %7 = alloca { i64, i8 }, align 8
  store { i64, i8 } %6, { i64, i8 }* %7, align 8
  %8 = extractvalue { i64, i8 } %6, 0
  %9 = icmp eq i64 %8, 1
  br i1 %9, label %when_true_2, label %when_exit_0

when_true_2:                                      ; preds = %when_true_1
  %10 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %7, i64 0, i32 1
  %11 = load i8, i8* %10, align 8
  %12 = icmp eq i8 %11, 47
  br i1 %12, label %when_true_3, label %when_exit_0

when_true_3:                                      ; preds = %when_true_2
  %13 = add i64 %idx, 2
  %14 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %13)
  %15 = alloca { i64, i8 }, align 8
  store { i64, i8 } %14, { i64, i8 }* %15, align 8
  %16 = extractvalue { i64, i8 } %14, 0
  %17 = icmp eq i64 %16, 1
  br i1 %17, label %when_true_4, label %while_exit_0

when_true_4:                                      ; preds = %when_true_3, %when_exit_4
  %18 = phi { i64, i8 }* [ %24, %when_exit_4 ], [ %15, %when_true_3 ]
  %.0.ph27 = phi i64 [ %22, %when_exit_4 ], [ %13, %when_true_3 ]
  %19 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %18, i64 0, i32 1
  %20 = load i8, i8* %19, align 8
  %21 = icmp eq i8 %20, 10
  br i1 %21, label %while_exit_0, label %when_exit_4

when_exit_4:                                      ; preds = %when_true_4
  %22 = add i64 %.0.ph27, 1
  %23 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %22)
  %24 = alloca { i64, i8 }, align 8
  store { i64, i8 } %23, { i64, i8 }* %24, align 8
  %25 = extractvalue { i64, i8 } %23, 0
  %26 = icmp eq i64 %25, 1
  br i1 %26, label %when_true_4, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_4, %when_true_4, %when_true_3
  %.0.ph.lcssa = phi i64 [ %13, %when_true_3 ], [ %22, %when_exit_4 ], [ %.0.ph27, %when_true_4 ]
  %.unpack1720 = insertvalue { { i64, i64 }, i64 } { { i64, i64 } { i64 13, i64 undef }, i64 undef }, i64 %.0.ph.lcssa, 1
  %27 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack1720, 1
  ret { i64, { { i64, i64 }, i64 } } %27

when_exit_0:                                      ; preds = %when_true_2, %when_true_1, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

declare i64 @memcmp(i8*, i8*, i64) local_unnamed_addr

define { i64, { { i64, i64 }, i64 } } @lexer.lexDoubleSym({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
  %1 = alloca [2 x i8], align 1
  %.repack = getelementptr inbounds [2 x i8], [2 x i8]* %1, i64 0, i64 0
  store i8 0, i8* %.repack, align 1
  %.repack1 = getelementptr inbounds [2 x i8], [2 x i8]* %1, i64 0, i64 1
  store i8 0, i8* %.repack1, align 1
  %2 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %3 = extractvalue { i64, i8 } %2, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_true_0, label %when_exit_0

when_true_0:                                      ; preds = %0
  %.fca.1.extract = extractvalue { i64, i8 } %2, 1
  store i8 %.fca.1.extract, i8* %.repack, align 1
  br label %when_exit_0

when_exit_0:                                      ; preds = %when_true_0, %0
  %5 = add i64 %idx, 1
  %6 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %5)
  %7 = alloca { i64, i8 }, align 8
  store { i64, i8 } %6, { i64, i8 }* %7, align 8
  %8 = extractvalue { i64, i8 } %6, 0
  %9 = icmp eq i64 %8, 1
  br i1 %9, label %when_true_1, label %case_0

when_true_1:                                      ; preds = %when_exit_0
  %10 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %7, i64 0, i32 1
  %11 = load i8, i8* %10, align 8
  store i8 %11, i8* %.repack1, align 1
  br label %case_0

case_0:                                           ; preds = %when_exit_0, %when_true_1
  %12 = alloca i16, align 2
  %.repack2 = bitcast i16* %12 to i8*
  store i16 15677, i16* %12, align 2
  %13 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack2, i64 2)
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %switch_exit_0, label %case_1

case_1:                                           ; preds = %case_0
  %15 = alloca i16, align 2
  %.repack4 = bitcast i16* %15 to i8*
  store i16 15649, i16* %15, align 2
  %16 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack4, i64 2)
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %switch_exit_0, label %case_2

case_2:                                           ; preds = %case_1
  %18 = alloca i16, align 2
  %.repack6 = bitcast i16* %18 to i8*
  store i16 15678, i16* %18, align 2
  %19 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack6, i64 2)
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %switch_exit_0, label %case_3

case_3:                                           ; preds = %case_2
  %21 = alloca i16, align 2
  %.repack8 = bitcast i16* %21 to i8*
  store i16 15676, i16* %21, align 2
  %22 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack8, i64 2)
  %23 = icmp eq i64 %22, 0
  br i1 %23, label %switch_exit_0, label %case_4

case_4:                                           ; preds = %case_3
  %24 = alloca i16, align 2
  %.repack10 = bitcast i16* %24 to i8*
  store i16 31868, i16* %24, align 2
  %25 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack10, i64 2)
  %26 = icmp eq i64 %25, 0
  br i1 %26, label %switch_exit_0, label %case_5

case_5:                                           ; preds = %case_4
  %27 = alloca i16, align 2
  %.repack12 = bitcast i16* %27 to i8*
  store i16 9766, i16* %27, align 2
  %28 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack12, i64 2)
  %29 = icmp eq i64 %28, 0
  br i1 %29, label %switch_exit_0, label %case_6

case_6:                                           ; preds = %case_5
  %30 = alloca i16, align 2
  %.repack14 = bitcast i16* %30 to i8*
  store i16 11822, i16* %30, align 2
  %31 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack14, i64 2)
  %32 = icmp eq i64 %31, 0
  br i1 %32, label %switch_exit_0, label %case_7

case_7:                                           ; preds = %case_6
  %33 = alloca i16, align 2
  %.repack16 = bitcast i16* %33 to i8*
  store i16 15917, i16* %33, align 2
  %34 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack16, i64 2)
  %35 = icmp eq i64 %34, 0
  br i1 %35, label %switch_exit_0, label %case_8

case_8:                                           ; preds = %case_7
  %36 = alloca i16, align 2
  %.repack18 = bitcast i16* %36 to i8*
  store i16 14906, i16* %36, align 2
  %37 = call i64 @memcmp(i8* nonnull %.repack, i8* nonnull %.repack18, i64 2)
  %38 = icmp eq i64 %37, 0
  br i1 %38, label %switch_exit_0, label %case_9

case_9:                                           ; preds = %case_8
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }

switch_exit_0:                                    ; preds = %case_8, %case_7, %case_6, %case_5, %case_4, %case_3, %case_2, %case_1, %case_0
  %39 = alloca { i64, i64 }, align 8
  %40 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %39, i64 0, i32 0
  store i64 7, i64* %40, align 8
  %41 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %39, i64 0, i32 1
  %42 = bitcast i64* %41 to { i8, i8 }*
  %43 = bitcast i64* %41 to i8*
  %44 = load i8, i8* %.repack, align 1
  store i8 %44, i8* %43, align 8
  %45 = getelementptr inbounds { i8, i8 }, { i8, i8 }* %42, i64 0, i32 1
  %46 = load i8, i8* %.repack1, align 1
  store i8 %46, i8* %45, align 1
  %47 = add i64 %idx, 2
  %.unpack29 = load i64, i64* %41, align 8
  %.unpack37.unpack43 = insertvalue { i64, i64 } { i64 7, i64 undef }, i64 %.unpack29, 1
  %48 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack37.unpack43, 0
  %.unpack3740 = insertvalue { { i64, i64 }, i64 } %48, i64 %47, 1
  %49 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack3740, 1
  ret { i64, { { i64, i64 }, i64 } } %49
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexEscaped({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
case_0:
  %0 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %1 = add i64 %idx, 1
  %2 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %1)
  %.fca.0.extract = extractvalue { i64, i8 } %0, 0
  %.fca.0.extract79 = extractvalue { i64, i8 } %2, 0
  %3 = icmp eq i64 %.fca.0.extract, 1
  %.fca.1.extract = extractvalue { i64, i8 } %0, 1
  %4 = icmp eq i8 %.fca.1.extract, 92
  %or.cond = and i1 %3, %4
  %5 = icmp eq i64 %.fca.0.extract79, 1
  %or.cond96 = and i1 %or.cond, %5
  br i1 %or.cond96, label %when_true_2, label %case_4

when_true_2:                                      ; preds = %case_0
  %.fca.1.extract81 = extractvalue { i64, i8 } %2, 1
  switch i8 %.fca.1.extract81, label %case_4 [
    i8 110, label %when_true_3
    i8 116, label %when_true_7
    i8 48, label %when_true_11
    i8 92, label %when_true_15
  ]

when_true_3:                                      ; preds = %when_true_2
  %6 = alloca { i64, i64 }, align 8
  %7 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %6, i64 0, i32 0
  store i64 0, i64* %7, align 8
  %8 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %6, i64 0, i32 1
  %9 = bitcast i64* %8 to i8*
  store i8 10, i8* %9, align 8
  %10 = add i64 %idx, 2
  %.unpack64 = load i64, i64* %8, align 8
  %.unpack72.unpack78 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack64, 1
  %11 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack72.unpack78, 0
  %.unpack7275 = insertvalue { { i64, i64 }, i64 } %11, i64 %10, 1
  %12 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack7275, 1
  ret { i64, { { i64, i64 }, i64 } } %12

when_true_7:                                      ; preds = %when_true_2
  %13 = alloca { i64, i64 }, align 8
  %14 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %13, i64 0, i32 0
  store i64 0, i64* %14, align 8
  %15 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %13, i64 0, i32 1
  %16 = bitcast i64* %15 to i8*
  store i8 9, i8* %16, align 8
  %17 = add i64 %idx, 2
  %.unpack46 = load i64, i64* %15, align 8
  %.unpack54.unpack60 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack46, 1
  %18 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack54.unpack60, 0
  %.unpack5457 = insertvalue { { i64, i64 }, i64 } %18, i64 %17, 1
  %19 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack5457, 1
  ret { i64, { { i64, i64 }, i64 } } %19

when_true_11:                                     ; preds = %when_true_2
  %20 = alloca { i64, i64 }, align 8
  %21 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %20, i64 0, i32 0
  store i64 0, i64* %21, align 8
  %22 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %20, i64 0, i32 1
  %23 = bitcast i64* %22 to i8*
  store i8 0, i8* %23, align 8
  %24 = add i64 %idx, 2
  %.unpack28 = load i64, i64* %22, align 8
  %.unpack36.unpack42 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack28, 1
  %25 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack36.unpack42, 0
  %.unpack3639 = insertvalue { { i64, i64 }, i64 } %25, i64 %24, 1
  %26 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack3639, 1
  ret { i64, { { i64, i64 }, i64 } } %26

when_true_15:                                     ; preds = %when_true_2
  %27 = alloca { i64, i64 }, align 8
  %28 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %27, i64 0, i32 0
  store i64 0, i64* %28, align 8
  %29 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %27, i64 0, i32 1
  %30 = bitcast i64* %29 to i8*
  store i8 92, i8* %30, align 8
  %31 = add i64 %idx, 2
  %.unpack10 = load i64, i64* %29, align 8
  %.unpack17.unpack23 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack10, 1
  %32 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack17.unpack23, 0
  %.unpack1720 = insertvalue { { i64, i64 }, i64 } %32, i64 %31, 1
  %33 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack1720, 1
  ret { i64, { { i64, i64 }, i64 } } %33

case_4:                                           ; preds = %when_true_2, %case_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define { i64, { { i64, i64 }, i64 } } @lexer.lexFloat({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = extractvalue { i64, i8 } %1, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_true_0, label %when_exit_3

when_true_0:                                      ; preds = %0, %when_exit_0
  %5 = phi { i64, i8 }* [ %16, %when_exit_0 ], [ %2, %0 ]
  %.0.ph218 = phi i64 [ %9, %when_exit_0 ], [ %idx, %0 ]
  %.sroa.0.0.ph217 = phi i64 [ %10, %when_exit_0 ], [ 0, %0 ]
  %.sroa.15.0.ph216 = phi i64 [ %.sroa.15.1, %when_exit_0 ], [ 0, %0 ]
  %.sroa.22.0.ph215 = phi i8* [ %.sroa.22.1, %when_exit_0 ], [ null, %0 ]
  %6 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %5, i64 0, i32 1
  %7 = load i8, i8* %6, align 8
  %8 = tail call i1 @chars.isDigit(i8 %7)
  br i1 %8, label %when_true_1, label %when_exit_3

when_true_1:                                      ; preds = %when_true_0
  %9 = add i64 %.0.ph218, 1
  %10 = add i64 %.sroa.0.0.ph217, 1
  %11 = icmp sgt i64 %10, %.sroa.15.0.ph216
  br i1 %11, label %when_true_2, label %when_exit_0

when_true_2:                                      ; preds = %when_true_1
  %12 = shl i64 %10, 1
  %13 = tail call i8* @GC_malloc(i64 %12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %13, i8* align 1 %.sroa.22.0.ph215, i64 %.sroa.0.0.ph217, i1 false)
  br label %when_exit_0

when_exit_0:                                      ; preds = %when_true_1, %when_true_2
  %.sroa.22.1 = phi i8* [ %13, %when_true_2 ], [ %.sroa.22.0.ph215, %when_true_1 ]
  %.sroa.15.1 = phi i64 [ %12, %when_true_2 ], [ %.sroa.15.0.ph216, %when_true_1 ]
  %14 = getelementptr i8, i8* %.sroa.22.1, i64 %.sroa.0.0.ph217
  store i8 %7, i8* %14, align 1
  %15 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %9)
  %16 = alloca { i64, i8 }, align 8
  store { i64, i8 } %15, { i64, i8 }* %16, align 8
  %17 = extractvalue { i64, i8 } %15, 0
  %18 = icmp eq i64 %17, 1
  br i1 %18, label %when_true_0, label %when_exit_3

when_exit_3:                                      ; preds = %when_exit_0, %when_true_0, %0
  %.sroa.22.0.ph.lcssa = phi i8* [ null, %0 ], [ %.sroa.22.1, %when_exit_0 ], [ %.sroa.22.0.ph215, %when_true_0 ]
  %.sroa.15.0.ph.lcssa = phi i64 [ 0, %0 ], [ %.sroa.15.1, %when_exit_0 ], [ %.sroa.15.0.ph216, %when_true_0 ]
  %.sroa.0.0.ph.lcssa = phi i64 [ 0, %0 ], [ %10, %when_exit_0 ], [ %.sroa.0.0.ph217, %when_true_0 ]
  %.0.ph.lcssa = phi i64 [ %idx, %0 ], [ %9, %when_exit_0 ], [ %.0.ph218, %when_true_0 ]
  %19 = icmp eq i64 %.sroa.0.0.ph.lcssa, 0
  br i1 %19, label %eqeq_table_exit_0, label %if_exit_0

eqeq_table_exit_0:                                ; preds = %when_exit_3
  %20 = tail call i64 @memcmp(i8* %.sroa.22.0.ph.lcssa, i8* null, i64 0)
  %21 = icmp eq i64 %20, 0
  br i1 %21, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %while_exit_1, %if_exit_0, %when_true_4, %eqeq_table_exit_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }

if_exit_0:                                        ; preds = %when_exit_3, %eqeq_table_exit_0
  %22 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %23 = alloca { i64, i8 }, align 8
  store { i64, i8 } %22, { i64, i8 }* %23, align 8
  %24 = extractvalue { i64, i8 } %22, 0
  %25 = icmp eq i64 %24, 1
  br i1 %25, label %when_true_4, label %if_true_0

when_true_4:                                      ; preds = %if_exit_0
  %26 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %23, i64 0, i32 1
  %27 = load i8, i8* %26, align 8
  %28 = icmp eq i8 %27, 46
  br i1 %28, label %when_true_5, label %if_true_0

when_true_5:                                      ; preds = %when_true_4
  %29 = add i64 %.sroa.0.0.ph.lcssa, 1
  %30 = icmp sgt i64 %29, %.sroa.15.0.ph.lcssa
  br i1 %30, label %when_true_6, label %when_exit_7

when_true_6:                                      ; preds = %when_true_5
  %31 = shl i64 %29, 1
  %32 = tail call i8* @GC_malloc(i64 %31)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %32, i8* align 1 %.sroa.22.0.ph.lcssa, i64 %.sroa.0.0.ph.lcssa, i1 false)
  br label %when_exit_7

when_exit_7:                                      ; preds = %when_true_6, %when_true_5
  %.sroa.22.3 = phi i8* [ %32, %when_true_6 ], [ %.sroa.22.0.ph.lcssa, %when_true_5 ]
  %.sroa.15.3 = phi i64 [ %31, %when_true_6 ], [ %.sroa.15.0.ph.lcssa, %when_true_5 ]
  %33 = getelementptr i8, i8* %.sroa.22.3, i64 %.sroa.0.0.ph.lcssa
  store i8 46, i8* %33, align 1
  %.5.ph203 = add i64 %.0.ph.lcssa, 1
  %34 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.5.ph203)
  %35 = alloca { i64, i8 }, align 8
  store { i64, i8 } %34, { i64, i8 }* %35, align 8
  %36 = extractvalue { i64, i8 } %34, 0
  %37 = icmp eq i64 %36, 1
  br i1 %37, label %when_true_8, label %while_exit_1

when_true_8:                                      ; preds = %when_exit_7, %when_exit_8
  %38 = phi { i64, i8 }* [ %49, %when_exit_8 ], [ %35, %when_exit_7 ]
  %.5.ph207 = phi i64 [ %.5.ph, %when_exit_8 ], [ %.5.ph203, %when_exit_7 ]
  %.sroa.0.3.ph206 = phi i64 [ %43, %when_exit_8 ], [ %29, %when_exit_7 ]
  %.sroa.15.5.ph205 = phi i64 [ %.sroa.15.6, %when_exit_8 ], [ %.sroa.15.3, %when_exit_7 ]
  %.sroa.22.5.ph204 = phi i8* [ %.sroa.22.6, %when_exit_8 ], [ %.sroa.22.3, %when_exit_7 ]
  %39 = phi i1 [ true, %when_exit_8 ], [ false, %when_exit_7 ]
  %40 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %38, i64 0, i32 1
  %41 = load i8, i8* %40, align 8
  %42 = tail call i1 @chars.isDigit(i8 %41)
  br i1 %42, label %when_true_9, label %while_exit_1

when_true_9:                                      ; preds = %when_true_8
  %43 = add i64 %.sroa.0.3.ph206, 1
  %44 = icmp sgt i64 %43, %.sroa.15.5.ph205
  br i1 %44, label %when_true_10, label %when_exit_8

when_true_10:                                     ; preds = %when_true_9
  %45 = shl i64 %43, 1
  %46 = tail call i8* @GC_malloc(i64 %45)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %46, i8* align 1 %.sroa.22.5.ph204, i64 %.sroa.0.3.ph206, i1 false)
  br label %when_exit_8

when_exit_8:                                      ; preds = %when_true_9, %when_true_10
  %.sroa.22.6 = phi i8* [ %46, %when_true_10 ], [ %.sroa.22.5.ph204, %when_true_9 ]
  %.sroa.15.6 = phi i64 [ %45, %when_true_10 ], [ %.sroa.15.5.ph205, %when_true_9 ]
  %47 = getelementptr i8, i8* %.sroa.22.6, i64 %.sroa.0.3.ph206
  store i8 %41, i8* %47, align 1
  %.5.ph = add i64 %.5.ph207, 1
  %48 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.5.ph)
  %49 = alloca { i64, i8 }, align 8
  store { i64, i8 } %48, { i64, i8 }* %49, align 8
  %50 = extractvalue { i64, i8 } %48, 0
  %51 = icmp eq i64 %50, 1
  br i1 %51, label %when_true_8, label %while_exit_1

while_exit_1:                                     ; preds = %when_exit_8, %when_true_8, %when_exit_7
  %.lcssa = phi i1 [ false, %when_exit_7 ], [ true, %when_exit_8 ], [ %39, %when_true_8 ]
  %.sroa.22.5.ph.lcssa = phi i8* [ %.sroa.22.3, %when_exit_7 ], [ %.sroa.22.6, %when_exit_8 ], [ %.sroa.22.5.ph204, %when_true_8 ]
  %.sroa.0.3.ph.lcssa = phi i64 [ %29, %when_exit_7 ], [ %43, %when_exit_8 ], [ %.sroa.0.3.ph206, %when_true_8 ]
  %.5.ph.lcssa = phi i64 [ %.5.ph203, %when_exit_7 ], [ %.5.ph, %when_exit_8 ], [ %.5.ph207, %when_true_8 ]
  br i1 %.lcssa, label %if_exit_1, label %if_true_0

if_exit_1:                                        ; preds = %while_exit_1
  %52 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %53 = load i64, i64* %52, align 4
  %54 = add i64 %53, 1
  %55 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %56 = load i64, i64* %55, align 4
  %57 = icmp sgt i64 %54, %56
  br i1 %57, label %when_true_11, label %if_exit_1.when_exit_11_crit_edge

if_exit_1.when_exit_11_crit_edge:                 ; preds = %if_exit_1
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_11

when_true_11:                                     ; preds = %if_exit_1
  %58 = shl i64 %54, 1
  %59 = tail call i8* @GC_malloc(i64 %58)
  %60 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %61 = load i8*, i8** %60, align 8
  %62 = load i64, i64* %52, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %59, i8* align 1 %61, i64 %62, i1 false)
  store i64 %58, i64* %52, align 8
  store i64 %58, i64* %55, align 8
  store i8* %59, i8** %60, align 8
  br label %when_exit_11

when_exit_11:                                     ; preds = %if_exit_1.when_exit_11_crit_edge, %when_true_11
  %63 = phi i8* [ %.pre, %if_exit_1.when_exit_11_crit_edge ], [ %59, %when_true_11 ]
  store i64 %54, i64* %52, align 4
  %64 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %65 = getelementptr i8, i8* %63, i64 %53
  store i8 0, i8* %65, align 1
  %66 = icmp sgt i64 %.sroa.0.3.ph.lcssa, 0
  br i1 %66, label %for_body_0, label %for_exit_0

for_body_0:                                       ; preds = %when_exit_11, %when_exit_12
  %storemerge190 = phi i64 [ %79, %when_exit_12 ], [ 0, %when_exit_11 ]
  %67 = getelementptr i8, i8* %.sroa.22.5.ph.lcssa, i64 %storemerge190
  %68 = load i8, i8* %67, align 1
  %69 = load i64, i64* %52, align 4
  %70 = add i64 %69, 1
  %71 = load i64, i64* %55, align 4
  %72 = icmp sgt i64 %70, %71
  br i1 %72, label %when_true_12, label %for_body_0.when_exit_12_crit_edge

for_body_0.when_exit_12_crit_edge:                ; preds = %for_body_0
  %.pre197 = load i8*, i8** %64, align 8
  br label %when_exit_12

when_true_12:                                     ; preds = %for_body_0
  %73 = shl i64 %70, 1
  %74 = tail call i8* @GC_malloc(i64 %73)
  %75 = load i8*, i8** %64, align 8
  %76 = load i64, i64* %52, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %74, i8* align 1 %75, i64 %76, i1 false)
  store i64 %73, i64* %52, align 8
  store i64 %73, i64* %55, align 8
  store i8* %74, i8** %64, align 8
  br label %when_exit_12

when_exit_12:                                     ; preds = %for_body_0.when_exit_12_crit_edge, %when_true_12
  %77 = phi i8* [ %.pre197, %for_body_0.when_exit_12_crit_edge ], [ %74, %when_true_12 ]
  store i64 %70, i64* %52, align 4
  %78 = getelementptr i8, i8* %77, i64 %69
  store i8 %68, i8* %78, align 1
  %79 = add nuw nsw i64 %storemerge190, 1
  %80 = icmp slt i64 %79, %.sroa.0.3.ph.lcssa
  br i1 %80, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_exit_12, %when_exit_11
  %.unpack24.unpack30 = insertvalue { i64, i64 } { i64 3, i64 undef }, i64 %54, 1
  %81 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack24.unpack30, 0
  %.unpack2427 = insertvalue { { i64, i64 }, i64 } %81, i64 %.5.ph.lcssa, 1
  %82 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack2427, 1
  ret { i64, { { i64, i64 }, i64 } } %82
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexGraphic({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
when_exit_0:
  %0 = tail call i8* @GC_malloc(i64 60)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %0, i8* align 16 getelementptr inbounds ([31 x i8], [31 x i8]* @lexer.str_10, i64 0, i64 0), i64 30, i1 false)
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = extractvalue { i64, i8 } %1, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_true_1, label %when_exit_1

when_true_1:                                      ; preds = %when_exit_0
  %5 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 1
  %6 = load i8, i8* %5, align 8
  switch i8 %6, label %when_exit_1 [
    i8 45, label %if_true_0
    i8 47, label %if_true_0
    i8 60, label %if_true_0
    i8 62, label %if_true_0
    i8 91, label %if_true_0
    i8 93, label %if_true_0
    i8 123, label %if_true_0
    i8 125, label %if_true_0
    i8 40, label %if_true_0
    i8 41, label %if_true_0
    i8 46, label %if_true_0
    i8 44, label %if_true_0
    i8 59, label %if_true_0
    i8 58, label %if_true_0
    i8 124, label %if_true_0
    i8 61, label %if_true_0
    i8 33, label %if_true_0
    i8 63, label %if_true_0
    i8 64, label %if_true_0
    i8 35, label %if_true_0
    i8 36, label %if_true_0
    i8 37, label %if_true_0
    i8 94, label %if_true_0
    i8 38, label %if_true_0
    i8 42, label %if_true_0
    i8 95, label %if_true_0
    i8 43, label %if_true_0
    i8 32, label %if_true_0
  ]

if_true_0:                                        ; preds = %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1
  %7 = alloca { i64, i64 }, align 8
  %8 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %7, i64 0, i32 0
  store i64 0, i64* %8, align 8
  %9 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %7, i64 0, i32 1
  %10 = bitcast i64* %9 to i8*
  store i8 %6, i8* %10, align 8
  %11 = add i64 %idx, 1
  %.unpack32 = load i64, i64* %9, align 8
  %.unpack40.unpack46 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack32, 1
  %12 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack40.unpack46, 0
  %.unpack4043 = insertvalue { { i64, i64 }, i64 } %12, i64 %11, 1
  %13 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack4043, 1
  ret { i64, { { i64, i64 }, i64 } } %13

when_exit_1:                                      ; preds = %when_true_1, %when_exit_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = extractvalue { i64, i8 } %1, 0
  %3 = icmp eq i64 %2, 1
  br i1 %3, label %when_true_0, label %when_exit_0

when_true_0:                                      ; preds = %0
  %.fca.1.extract = extractvalue { i64, i8 } %1, 1
  %4 = tail call i1 @chars.isAlpha(i8 %.fca.1.extract)
  br i1 %4, label %when_true_1, label %when_exit_0

when_true_1:                                      ; preds = %when_true_0
  %5 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %6 = load i64, i64* %5, align 4
  %7 = add i64 %6, 1
  %8 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %9 = load i64, i64* %8, align 4
  %10 = icmp sgt i64 %7, %9
  br i1 %10, label %when_true_2, label %when_true_1.when_exit_2_crit_edge

when_true_1.when_exit_2_crit_edge:                ; preds = %when_true_1
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_2

when_true_2:                                      ; preds = %when_true_1
  %11 = shl i64 %7, 1
  %12 = tail call i8* @GC_malloc(i64 %11)
  %13 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %14 = load i8*, i8** %13, align 8
  %15 = load i64, i64* %5, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %12, i8* align 1 %14, i64 %15, i1 false)
  store i64 %11, i64* %5, align 8
  store i64 %11, i64* %8, align 8
  store i8* %12, i8** %13, align 8
  br label %when_exit_2

when_exit_2:                                      ; preds = %when_true_1.when_exit_2_crit_edge, %when_true_2
  %16 = phi i8* [ %.pre, %when_true_1.when_exit_2_crit_edge ], [ %12, %when_true_2 ]
  store i64 %7, i64* %5, align 4
  %17 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %18 = getelementptr i8, i8* %16, i64 %6
  store i8 0, i8* %18, align 1
  %19 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %s, i64 %idx)
  %20 = alloca { i64, i8 }, align 8
  store { i64, i8 } %19, { i64, i8 }* %20, align 8
  %21 = extractvalue { i64, i8 } %19, 0
  %22 = icmp eq i64 %21, 1
  br i1 %22, label %when_true_3, label %while_exit_0

when_true_3:                                      ; preds = %when_exit_2, %when_exit_3
  %23 = phi { i64, i8 }* [ %43, %when_exit_3 ], [ %20, %when_exit_2 ]
  %.0.ph60 = phi i64 [ %31, %when_exit_3 ], [ %idx, %when_exit_2 ]
  %24 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %23, i64 0, i32 1
  %25 = load i8, i8* %24, align 8
  %26 = tail call i1 @chars.isAlpha(i8 %25)
  %27 = tail call i1 @chars.isDigit(i8 %25)
  %28 = or i1 %26, %27
  %29 = icmp eq i8 %25, 95
  %30 = or i1 %29, %28
  br i1 %30, label %when_true_4, label %while_exit_0

when_true_4:                                      ; preds = %when_true_3
  %31 = add i64 %.0.ph60, 1
  %32 = load i64, i64* %5, align 4
  %33 = add i64 %32, 1
  %34 = load i64, i64* %8, align 4
  %35 = icmp sgt i64 %33, %34
  br i1 %35, label %when_true_5, label %when_true_4.when_exit_3_crit_edge

when_true_4.when_exit_3_crit_edge:                ; preds = %when_true_4
  %.pre59 = load i8*, i8** %17, align 8
  br label %when_exit_3

when_true_5:                                      ; preds = %when_true_4
  %36 = shl i64 %33, 1
  %37 = tail call i8* @GC_malloc(i64 %36)
  %38 = load i8*, i8** %17, align 8
  %39 = load i64, i64* %5, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %37, i8* align 1 %38, i64 %39, i1 false)
  store i64 %36, i64* %5, align 8
  store i64 %36, i64* %8, align 8
  store i8* %37, i8** %17, align 8
  br label %when_exit_3

when_exit_3:                                      ; preds = %when_true_4.when_exit_3_crit_edge, %when_true_5
  %40 = phi i8* [ %.pre59, %when_true_4.when_exit_3_crit_edge ], [ %37, %when_true_5 ]
  store i64 %33, i64* %5, align 4
  %41 = getelementptr i8, i8* %40, i64 %32
  store i8 %25, i8* %41, align 1
  %42 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %s, i64 %31)
  %43 = alloca { i64, i8 }, align 8
  store { i64, i8 } %42, { i64, i8 }* %43, align 8
  %44 = extractvalue { i64, i8 } %42, 0
  %45 = icmp eq i64 %44, 1
  br i1 %45, label %when_true_3, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_3, %when_true_3, %when_exit_2
  %.0.ph.lcssa = phi i64 [ %idx, %when_exit_2 ], [ %31, %when_exit_3 ], [ %.0.ph60, %when_true_3 ]
  %.unpack18.unpack24 = insertvalue { i64, i64 } { i64 1, i64 undef }, i64 %7, 1
  %46 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack18.unpack24, 0
  %.unpack1821 = insertvalue { { i64, i64 }, i64 } %46, i64 %.0.ph.lcssa, 1
  %47 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack1821, 1
  ret { i64, { { i64, i64 }, i64 } } %47

when_exit_0:                                      ; preds = %when_true_0, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexImport({ i64, i64, i8* }* %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* %s, i64 %idx)
  %2 = extractvalue { i64, { { i64, i64 }, i64 } } %1, 0
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %when_true_0, label %when_exit_0

when_true_0:                                      ; preds = %when_exit_5.thread, %when_exit_5, %eqeq_table_exit_1, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }

when_exit_0:                                      ; preds = %0
  %4 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* %s, i64 %idx)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %4, 0
  %.elt11 = extractvalue { i64, { { i64, i64 }, i64 } } %4, 1
  %.elt11.elt = extractvalue { { i64, i64 }, i64 } %.elt11, 0
  %.elt11.elt.elt15 = extractvalue { i64, i64 } %.elt11.elt, 1
  %.elt11.elt13 = extractvalue { { i64, i64 }, i64 } %.elt11, 1
  %5 = icmp eq i64 %.elt, 1
  br i1 %5, label %when_exit_1, label %when_true_1

when_true_1:                                      ; preds = %when_exit_0
  tail call void @llvm.trap()
  unreachable

when_exit_1:                                      ; preds = %when_exit_0
  %.elt11.elt.elt = extractvalue { i64, i64 } %.elt11.elt, 0
  %6 = icmp eq i64 %.elt11.elt.elt, 1
  br i1 %6, label %when_exit_3, label %when_true_2

when_true_2:                                      ; preds = %when_exit_1
  tail call void @llvm.trap()
  unreachable

when_exit_3:                                      ; preds = %when_exit_1
  %7 = alloca { i64, i64, i8* }, align 8
  %.repack29 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %7, i64 0, i32 0
  %.repack31 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %7, i64 0, i32 2
  %8 = bitcast { i64, i64, i8* }* %7 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %8, i8 0, i64 24, i1 false)
  call void @lexer.read({ i64, i64, i8* }* %s, { i64, i64, i8* }* nonnull %7, i64 %.elt11.elt.elt15)
  tail call void @lexer.popString({ i64, i64, i8* }* %s)
  %9 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %9, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_11, i64 0, i64 0), i64 6, i1 false)
  %10 = load i64, i64* %.repack29, align 8
  %11 = icmp eq i64 %10, 6
  br i1 %11, label %eqeq_table_exit_0, label %when_exit_5

eqeq_table_exit_0:                                ; preds = %when_exit_3
  %12 = load i8*, i8** %.repack31, align 8
  %13 = tail call i64 @memcmp(i8* %12, i8* %9, i64 6)
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %switch_exit_0, label %when_exit_5.thread

when_exit_5.thread:                               ; preds = %eqeq_table_exit_0
  %15 = tail call i8* @GC_malloc(i64 16)
  %16 = bitcast i8* %15 to i64*
  store i64 7160569967387110761, i64* %16, align 1
  br label %when_true_0

when_exit_5:                                      ; preds = %when_exit_3
  %17 = tail call i8* @GC_malloc(i64 16)
  %18 = bitcast i8* %17 to i64*
  store i64 7160569967387110761, i64* %18, align 1
  %19 = icmp eq i64 %10, 8
  br i1 %19, label %eqeq_table_exit_1, label %when_true_0

eqeq_table_exit_1:                                ; preds = %when_exit_5
  %20 = load i8*, i8** %.repack31, align 8
  %21 = tail call i64 @memcmp(i8* %20, i8* %17, i64 8)
  %22 = icmp eq i64 %21, 0
  br i1 %22, label %switch_exit_0, label %when_true_0

switch_exit_0:                                    ; preds = %eqeq_table_exit_1, %eqeq_table_exit_0
  %23 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %24 = load i64, i64* %23, align 4
  %25 = add i64 %24, 1
  %26 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %27 = load i64, i64* %26, align 4
  %28 = icmp sgt i64 %25, %27
  br i1 %28, label %when_true_7, label %switch_exit_0.when_exit_7_crit_edge

switch_exit_0.when_exit_7_crit_edge:              ; preds = %switch_exit_0
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_7

when_true_7:                                      ; preds = %switch_exit_0
  %29 = shl i64 %25, 1
  %30 = tail call i8* @GC_malloc(i64 %29)
  %31 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %32 = load i8*, i8** %31, align 8
  %33 = load i64, i64* %23, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %30, i8* align 1 %32, i64 %33, i1 false)
  store i64 %29, i64* %23, align 8
  store i64 %29, i64* %26, align 8
  store i8* %30, i8** %31, align 8
  br label %when_exit_7

when_exit_7:                                      ; preds = %switch_exit_0.when_exit_7_crit_edge, %when_true_7
  %34 = phi i8* [ %.pre, %switch_exit_0.when_exit_7_crit_edge ], [ %30, %when_true_7 ]
  store i64 %25, i64* %23, align 4
  %35 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %36 = getelementptr i8, i8* %34, i64 %24
  store i8 0, i8* %36, align 1
  %37 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.elt11.elt13)
  %38 = alloca { i64, i8 }, align 8
  store { i64, i8 } %37, { i64, i8 }* %38, align 8
  %39 = extractvalue { i64, i8 } %37, 0
  %40 = icmp eq i64 %39, 1
  br i1 %40, label %when_true_8, label %while_cond_1.outer.preheader

when_true_8:                                      ; preds = %when_exit_7, %when_exit_8
  %41 = phi { i64, i8 }* [ %50, %when_exit_8 ], [ %38, %when_exit_7 ]
  %.0.ph217 = phi i64 [ %48, %when_exit_8 ], [ %.elt11.elt13, %when_exit_7 ]
  %42 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %41, i64 0, i32 1
  %43 = load i8, i8* %42, align 8
  switch i8 %43, label %while_cond_1.outer.preheader [
    i8 32, label %when_exit_8
    i8 9, label %when_exit_8
  ]

while_cond_1.outer.preheader:                     ; preds = %when_exit_8, %when_true_8, %when_exit_7
  %.0.ph.lcssa = phi i64 [ %.elt11.elt13, %when_exit_7 ], [ %48, %when_exit_8 ], [ %.0.ph217, %when_true_8 ]
  %44 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %45 = alloca { i64, i8 }, align 8
  store { i64, i8 } %44, { i64, i8 }* %45, align 8
  %46 = extractvalue { i64, i8 } %44, 0
  %47 = icmp eq i64 %46, 1
  br i1 %47, label %when_true_10, label %when_exit_14

when_exit_8:                                      ; preds = %when_true_8, %when_true_8
  %48 = add i64 %.0.ph217, 1
  %49 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %48)
  %50 = alloca { i64, i8 }, align 8
  store { i64, i8 } %49, { i64, i8 }* %50, align 8
  %51 = extractvalue { i64, i8 } %49, 0
  %52 = icmp eq i64 %51, 1
  br i1 %52, label %when_true_8, label %while_cond_1.outer.preheader

when_true_10:                                     ; preds = %while_cond_1.outer.preheader, %when_exit_10
  %53 = phi { i64, i8 }* [ %68, %when_exit_10 ], [ %45, %while_cond_1.outer.preheader ]
  %.3.ph213 = phi i64 [ %66, %when_exit_10 ], [ %.0.ph.lcssa, %while_cond_1.outer.preheader ]
  %54 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %53, i64 0, i32 1
  %55 = load i8, i8* %54, align 8
  switch i8 %55, label %when_true_12 [
    i8 10, label %when_exit_14
    i8 59, label %when_exit_14
  ]

when_true_12:                                     ; preds = %when_true_10
  %56 = load i64, i64* %23, align 4
  %57 = add i64 %56, 1
  %58 = load i64, i64* %26, align 4
  %59 = icmp sgt i64 %57, %58
  br i1 %59, label %when_true_13, label %when_exit_10

when_true_13:                                     ; preds = %when_true_12
  %60 = shl i64 %57, 1
  %61 = tail call i8* @GC_malloc(i64 %60)
  %62 = load i8*, i8** %35, align 8
  %63 = load i64, i64* %23, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %61, i8* align 1 %62, i64 %63, i1 false)
  store i64 %60, i64* %23, align 8
  store i64 %60, i64* %26, align 8
  store i8* %61, i8** %35, align 8
  br label %when_exit_10

when_exit_10:                                     ; preds = %when_true_12, %when_true_13
  store i64 %57, i64* %23, align 4
  %64 = load i8*, i8** %35, align 8
  %65 = getelementptr i8, i8* %64, i64 %56
  store i8 %55, i8* %65, align 1
  %66 = add i64 %.3.ph213, 1
  %67 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %66)
  %68 = alloca { i64, i8 }, align 8
  store { i64, i8 } %67, { i64, i8 }* %68, align 8
  %69 = extractvalue { i64, i8 } %67, 0
  %70 = icmp eq i64 %69, 1
  br i1 %70, label %when_true_10, label %when_exit_14

when_exit_14:                                     ; preds = %when_true_10, %when_true_10, %when_exit_10, %while_cond_1.outer.preheader
  %.3.ph.lcssa = phi i64 [ %.0.ph.lcssa, %while_cond_1.outer.preheader ], [ %.3.ph213, %when_true_10 ], [ %.3.ph213, %when_true_10 ], [ %66, %when_exit_10 ]
  %71 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %71, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_11, i64 0, i64 0), i64 6, i1 false)
  br i1 %11, label %eqeq_table_exit_2, label %when_exit_16

eqeq_table_exit_2:                                ; preds = %when_exit_14
  %72 = load i8*, i8** %.repack31, align 8
  %73 = tail call i64 @memcmp(i8* %72, i8* %71, i64 6)
  %74 = icmp eq i64 %73, 0
  br i1 %74, label %when_true_15, label %when_exit_16.thread

when_exit_16.thread:                              ; preds = %eqeq_table_exit_2
  %75 = tail call i8* @GC_malloc(i64 16)
  %76 = bitcast i8* %75 to i64*
  store i64 7160569967387110761, i64* %76, align 1
  br label %case_stmt_6

when_true_15:                                     ; preds = %eqeq_table_exit_2
  %.unpack110.unpack116 = insertvalue { i64, i64 } { i64 11, i64 undef }, i64 %25, 1
  %77 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack110.unpack116, 0
  %.unpack110113 = insertvalue { { i64, i64 }, i64 } %77, i64 %.3.ph.lcssa, 1
  %78 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack110113, 1
  ret { i64, { { i64, i64 }, i64 } } %78

when_exit_16:                                     ; preds = %when_exit_14
  %79 = tail call i8* @GC_malloc(i64 16)
  %80 = bitcast i8* %79 to i64*
  store i64 7160569967387110761, i64* %80, align 1
  %81 = icmp eq i64 %10, 8
  br i1 %81, label %eqeq_table_exit_3, label %case_stmt_6

eqeq_table_exit_3:                                ; preds = %when_exit_16
  %82 = load i8*, i8** %.repack31, align 8
  %83 = tail call i64 @memcmp(i8* %82, i8* %79, i64 8)
  %84 = icmp eq i64 %83, 0
  br i1 %84, label %when_true_17, label %case_stmt_6

when_true_17:                                     ; preds = %eqeq_table_exit_3
  %.unpack77.unpack83 = insertvalue { i64, i64 } { i64 12, i64 undef }, i64 %25, 1
  %85 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack77.unpack83, 0
  %.unpack7780 = insertvalue { { i64, i64 }, i64 } %85, i64 %.3.ph.lcssa, 1
  %86 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack7780, 1
  ret { i64, { { i64, i64 }, i64 } } %86

case_stmt_6:                                      ; preds = %when_exit_16.thread, %when_exit_16, %eqeq_table_exit_3
  tail call void @llvm.trap()
  unreachable
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexInt({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
exit_0:
  %0 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %1 = alloca { i64, i8 }, align 8
  store { i64, i8 } %0, { i64, i8 }* %1, align 8
  %2 = extractvalue { i64, i8 } %0, 0
  %3 = icmp eq i64 %2, 1
  br i1 %3, label %when_true_0, label %while_exit_0

when_true_0:                                      ; preds = %exit_0, %when_exit_0
  %4 = phi { i64, i8 }* [ %14, %when_exit_0 ], [ %1, %exit_0 ]
  %.0.ph33 = phi i64 [ %12, %when_exit_0 ], [ %idx, %exit_0 ]
  %.025.ph32 = phi i64 [ %11, %when_exit_0 ], [ 0, %exit_0 ]
  %5 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %4, i64 0, i32 1
  %6 = load i8, i8* %5, align 8
  %7 = tail call i1 @chars.isDigit(i8 %6)
  br i1 %7, label %when_exit_0, label %while_exit_0

when_exit_0:                                      ; preds = %when_true_0
  %8 = mul i64 %.025.ph32, 10
  %9 = add i8 %6, -48
  %10 = sext i8 %9 to i64
  %11 = add i64 %8, %10
  %12 = add i64 %.0.ph33, 1
  %13 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %12)
  %14 = alloca { i64, i8 }, align 8
  store { i64, i8 } %13, { i64, i8 }* %14, align 8
  %15 = extractvalue { i64, i8 } %13, 0
  %16 = icmp eq i64 %15, 1
  br i1 %16, label %when_true_0, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_0, %when_true_0, %exit_0
  %.025.ph.lcssa = phi i64 [ 0, %exit_0 ], [ %11, %when_exit_0 ], [ %.025.ph32, %when_true_0 ]
  %.0.ph.lcssa = phi i64 [ %idx, %exit_0 ], [ %12, %when_exit_0 ], [ %.0.ph33, %when_true_0 ]
  %17 = icmp sgt i64 %.0.ph.lcssa, %idx
  br i1 %17, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %while_exit_0
  %.unpack17.unpack23 = insertvalue { i64, i64 } { i64 2, i64 undef }, i64 %.025.ph.lcssa, 1
  %18 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack17.unpack23, 0
  %.unpack1720 = insertvalue { { i64, i64 }, i64 } %18, i64 %.0.ph.lcssa, 1
  %19 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack1720, 1
  ret { i64, { { i64, i64 }, i64 } } %19

if_exit_0:                                        ; preds = %while_exit_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexKeyword({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* %s, i64 %idx)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %1, 0
  %.elt30 = extractvalue { i64, { { i64, i64 }, i64 } } %1, 1
  %.elt30.elt = extractvalue { { i64, i64 }, i64 } %.elt30, 0
  %.elt30.elt.elt = extractvalue { i64, i64 } %.elt30.elt, 0
  %.elt30.elt.elt34 = extractvalue { i64, i64 } %.elt30.elt, 1
  %2 = icmp eq i64 %.elt, 1
  %3 = icmp eq i64 %.elt30.elt.elt, 1
  %or.cond = and i1 %2, %3
  br i1 %or.cond, label %when_exit_15, label %when_exit_0

when_exit_15:                                     ; preds = %0
  %.elt30.elt32 = extractvalue { { i64, i64 }, i64 } %.elt30, 1
  %4 = alloca i64, align 8
  store i64 %.elt30.elt.elt34, i64* %4, align 8
  %5 = alloca i64, align 8
  store i64 %.elt30.elt32, i64* %5, align 8
  %6 = alloca { i64, i64, i8* }, align 8
  %.repack58 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %6, i64 0, i32 0
  %.repack60 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %6, i64 0, i32 2
  %7 = bitcast { i64, i64, i8* }* %6 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %7, i8 0, i64 24, i1 false)
  call void @lexer.read({ i64, i64, i8* }* %s, { i64, i64, i8* }* nonnull %6, i64 %.elt30.elt.elt34)
  %8 = alloca { i64, i64, i8* }, align 8
  %.repack67 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 0
  %.repack68 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 1
  %.repack69 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 2
  %9 = tail call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack68, align 8
  store i8* %9, i8** %.repack69, align 8
  store i64 2, i64* %.repack67, align 8
  %10 = bitcast i8* %9 to i16*
  store i16 28262, i16* %10, align 1
  %11 = alloca { i64, i64, i8* }, align 8
  %.repack73 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %11, i64 0, i32 0
  %.repack74 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %11, i64 0, i32 1
  %.repack75 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %11, i64 0, i32 2
  %12 = tail call i8* @GC_malloc(i64 8)
  store i64 8, i64* %.repack74, align 8
  store i8* %12, i8** %.repack75, align 8
  store i64 4, i64* %.repack73, align 8
  %13 = bitcast i8* %12 to i32*
  store i32 1701869940, i32* %13, align 1
  %14 = alloca { i64, i64, i8* }, align 8
  %.repack79 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 0
  %.repack80 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 1
  %.repack81 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 2
  %15 = tail call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack80, align 8
  store i8* %15, i8** %.repack81, align 8
  store i64 2, i64* %.repack79, align 8
  %16 = bitcast i8* %15 to i16*
  store i16 26217, i16* %16, align 1
  %17 = alloca { i64, i64, i8* }, align 8
  %.repack85 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %17, i64 0, i32 0
  %.repack86 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %17, i64 0, i32 1
  %.repack87 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %17, i64 0, i32 2
  %18 = tail call i8* @GC_malloc(i64 8)
  store i64 8, i64* %.repack86, align 8
  store i8* %18, i8** %.repack87, align 8
  store i64 4, i64* %.repack85, align 8
  %19 = bitcast i8* %18 to i32*
  store i32 1702063205, i32* %19, align 1
  %20 = alloca { i64, i64, i8* }, align 8
  %.repack91 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 0
  %.repack92 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 1
  %.repack93 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 2
  %21 = tail call i8* @GC_malloc(i64 6)
  store i64 6, i64* %.repack92, align 8
  store i8* %21, i8** %.repack93, align 8
  store i64 3, i64* %.repack91, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %21, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_17, i64 0, i64 0), i64 3, i1 false)
  %22 = alloca { i64, i64, i8* }, align 8
  %.repack97 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 0
  %.repack98 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 1
  %.repack99 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 2
  %23 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack98, align 8
  store i8* %23, i8** %.repack99, align 8
  store i64 5, i64* %.repack97, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %23, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_18, i64 0, i64 0), i64 5, i1 false)
  %24 = alloca { i64, i64, i8* }, align 8
  %.repack103 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 0
  %.repack104 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 1
  %.repack105 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 2
  %25 = tail call i8* @GC_malloc(i64 12)
  store i64 12, i64* %.repack104, align 8
  store i8* %25, i8** %.repack105, align 8
  store i64 6, i64* %.repack103, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %25, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_19, i64 0, i64 0), i64 6, i1 false)
  %26 = alloca { i64, i64, i8* }, align 8
  %.repack109 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %26, i64 0, i32 0
  %.repack110 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %26, i64 0, i32 1
  %.repack111 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %26, i64 0, i32 2
  %27 = tail call i8* @GC_malloc(i64 12)
  store i64 12, i64* %.repack110, align 8
  store i8* %27, i8** %.repack111, align 8
  store i64 6, i64* %.repack109, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %27, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_20, i64 0, i64 0), i64 6, i1 false)
  %28 = tail call i8* @GC_malloc(i64 8)
  %29 = bitcast i8* %28 to i32*
  store i32 1702195828, i32* %29, align 1
  %30 = tail call i8* @GC_malloc(i64 10)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %30, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_22, i64 0, i64 0), i64 5, i1 false)
  %31 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %31, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_23, i64 0, i64 0), i64 6, i1 false)
  %32 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %32, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_24, i64 0, i64 0), i64 3, i1 false)
  %33 = tail call i8* @GC_malloc(i64 8)
  %34 = bitcast i8* %33 to i32*
  store i32 1819047278, i32* %34, align 1
  %35 = tail call i8* @GC_malloc(i64 8)
  %36 = bitcast i8* %35 to i32*
  store i32 1635017060, i32* %36, align 1
  %37 = tail call i8* @GC_malloc(i64 336)
  %38 = load i64, i64* %.repack67, align 8
  %39 = getelementptr i8, i8* %37, i64 8
  %40 = bitcast i8* %39 to i64*
  %41 = bitcast i8* %37 to i64*
  %42 = load i64, i64* %40, align 4
  %43 = icmp sgt i64 %38, %42
  %44 = bitcast i8* %37 to { i64, i64, i8* }*
  br i1 %43, label %when_true_16, label %when_exit_15.when_exit_16_crit_edge

when_exit_15.when_exit_16_crit_edge:              ; preds = %when_exit_15
  %.phi.trans.insert = getelementptr i8, i8* %37, i64 16
  %.phi.trans.insert604 = bitcast i8* %.phi.trans.insert to i8**
  %.pre = load i8*, i8** %.phi.trans.insert604, align 8
  br label %when_exit_16

when_true_16:                                     ; preds = %when_exit_15
  %45 = shl i64 %38, 1
  %46 = tail call i8* @GC_malloc(i64 %45)
  %47 = getelementptr i8, i8* %37, i64 16
  %48 = bitcast i8* %47 to i8**
  %49 = load i8*, i8** %48, align 8
  %50 = load i64, i64* %41, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %46, i8* align 1 %49, i64 %50, i1 false)
  store i64 %45, i64* %41, align 8
  store i64 %45, i64* %40, align 8
  store i8* %46, i8** %48, align 8
  br label %when_exit_16

when_exit_16:                                     ; preds = %when_exit_15.when_exit_16_crit_edge, %when_true_16
  %51 = phi i8* [ %.pre, %when_exit_15.when_exit_16_crit_edge ], [ %46, %when_true_16 ]
  store i64 %38, i64* %41, align 4
  %52 = load i8*, i8** %.repack69, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %51, i8* align 1 %52, i64 %38, i1 false)
  %53 = load i64, i64* %.repack73, align 8
  %54 = getelementptr i8, i8* %37, i64 32
  %55 = bitcast i8* %54 to i64*
  %56 = getelementptr i8, i8* %37, i64 24
  %57 = bitcast i8* %56 to i64*
  %58 = load i64, i64* %55, align 4
  %59 = icmp sgt i64 %53, %58
  br i1 %59, label %when_true_17, label %when_exit_16.when_exit_17_crit_edge

when_exit_16.when_exit_17_crit_edge:              ; preds = %when_exit_16
  %.phi.trans.insert605 = getelementptr i8, i8* %37, i64 40
  %60 = bitcast i8* %.phi.trans.insert605 to i8**
  %.pre606 = load i8*, i8** %60, align 8
  br label %when_exit_17

when_true_17:                                     ; preds = %when_exit_16
  %61 = shl i64 %53, 1
  %62 = tail call i8* @GC_malloc(i64 %61)
  %63 = getelementptr i8, i8* %37, i64 40
  %64 = bitcast i8* %63 to i8**
  %65 = load i8*, i8** %64, align 8
  %66 = load i64, i64* %57, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %62, i8* align 1 %65, i64 %66, i1 false)
  store i64 %61, i64* %57, align 8
  store i64 %61, i64* %55, align 8
  store i8* %62, i8** %64, align 8
  br label %when_exit_17

when_exit_17:                                     ; preds = %when_exit_16.when_exit_17_crit_edge, %when_true_17
  %67 = phi i8* [ %.pre606, %when_exit_16.when_exit_17_crit_edge ], [ %62, %when_true_17 ]
  store i64 %53, i64* %57, align 4
  %68 = load i8*, i8** %.repack75, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %67, i8* align 1 %68, i64 %53, i1 false)
  %69 = load i64, i64* %.repack79, align 8
  %70 = getelementptr i8, i8* %37, i64 56
  %71 = bitcast i8* %70 to i64*
  %72 = getelementptr i8, i8* %37, i64 48
  %73 = bitcast i8* %72 to i64*
  %74 = load i64, i64* %71, align 4
  %75 = icmp sgt i64 %69, %74
  br i1 %75, label %when_true_18, label %when_exit_17.when_exit_18_crit_edge

when_exit_17.when_exit_18_crit_edge:              ; preds = %when_exit_17
  %.phi.trans.insert607 = getelementptr i8, i8* %37, i64 64
  %76 = bitcast i8* %.phi.trans.insert607 to i8**
  %.pre608 = load i8*, i8** %76, align 8
  br label %when_exit_18

when_true_18:                                     ; preds = %when_exit_17
  %77 = shl i64 %69, 1
  %78 = tail call i8* @GC_malloc(i64 %77)
  %79 = getelementptr i8, i8* %37, i64 64
  %80 = bitcast i8* %79 to i8**
  %81 = load i8*, i8** %80, align 8
  %82 = load i64, i64* %73, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %78, i8* align 1 %81, i64 %82, i1 false)
  store i64 %77, i64* %73, align 8
  store i64 %77, i64* %71, align 8
  store i8* %78, i8** %80, align 8
  br label %when_exit_18

when_exit_18:                                     ; preds = %when_exit_17.when_exit_18_crit_edge, %when_true_18
  %83 = phi i8* [ %.pre608, %when_exit_17.when_exit_18_crit_edge ], [ %78, %when_true_18 ]
  store i64 %69, i64* %73, align 4
  %84 = load i8*, i8** %.repack81, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %83, i8* align 1 %84, i64 %69, i1 false)
  %85 = load i64, i64* %.repack85, align 8
  %86 = getelementptr i8, i8* %37, i64 80
  %87 = bitcast i8* %86 to i64*
  %88 = getelementptr i8, i8* %37, i64 72
  %89 = bitcast i8* %88 to i64*
  %90 = load i64, i64* %87, align 4
  %91 = icmp sgt i64 %85, %90
  br i1 %91, label %when_true_19, label %when_exit_18.when_exit_19_crit_edge

when_exit_18.when_exit_19_crit_edge:              ; preds = %when_exit_18
  %.phi.trans.insert609 = getelementptr i8, i8* %37, i64 88
  %92 = bitcast i8* %.phi.trans.insert609 to i8**
  %.pre610 = load i8*, i8** %92, align 8
  br label %when_exit_19

when_true_19:                                     ; preds = %when_exit_18
  %93 = shl i64 %85, 1
  %94 = tail call i8* @GC_malloc(i64 %93)
  %95 = getelementptr i8, i8* %37, i64 88
  %96 = bitcast i8* %95 to i8**
  %97 = load i8*, i8** %96, align 8
  %98 = load i64, i64* %89, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %94, i8* align 1 %97, i64 %98, i1 false)
  store i64 %93, i64* %89, align 8
  store i64 %93, i64* %87, align 8
  store i8* %94, i8** %96, align 8
  br label %when_exit_19

when_exit_19:                                     ; preds = %when_exit_18.when_exit_19_crit_edge, %when_true_19
  %99 = phi i8* [ %.pre610, %when_exit_18.when_exit_19_crit_edge ], [ %94, %when_true_19 ]
  store i64 %85, i64* %89, align 4
  %100 = load i8*, i8** %.repack87, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %99, i8* align 1 %100, i64 %85, i1 false)
  %101 = load i64, i64* %.repack91, align 8
  %102 = getelementptr i8, i8* %37, i64 104
  %103 = bitcast i8* %102 to i64*
  %104 = getelementptr i8, i8* %37, i64 96
  %105 = bitcast i8* %104 to i64*
  %106 = load i64, i64* %103, align 4
  %107 = icmp sgt i64 %101, %106
  br i1 %107, label %when_true_20, label %when_exit_19.when_exit_20_crit_edge

when_exit_19.when_exit_20_crit_edge:              ; preds = %when_exit_19
  %.phi.trans.insert611 = getelementptr i8, i8* %37, i64 112
  %108 = bitcast i8* %.phi.trans.insert611 to i8**
  %.pre612 = load i8*, i8** %108, align 8
  br label %when_exit_20

when_true_20:                                     ; preds = %when_exit_19
  %109 = shl i64 %101, 1
  %110 = tail call i8* @GC_malloc(i64 %109)
  %111 = getelementptr i8, i8* %37, i64 112
  %112 = bitcast i8* %111 to i8**
  %113 = load i8*, i8** %112, align 8
  %114 = load i64, i64* %105, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %110, i8* align 1 %113, i64 %114, i1 false)
  store i64 %109, i64* %105, align 8
  store i64 %109, i64* %103, align 8
  store i8* %110, i8** %112, align 8
  br label %when_exit_20

when_exit_20:                                     ; preds = %when_exit_19.when_exit_20_crit_edge, %when_true_20
  %115 = phi i8* [ %.pre612, %when_exit_19.when_exit_20_crit_edge ], [ %110, %when_true_20 ]
  store i64 %101, i64* %105, align 4
  %116 = load i8*, i8** %.repack93, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %115, i8* align 1 %116, i64 %101, i1 false)
  %117 = load i64, i64* %.repack97, align 8
  %118 = getelementptr i8, i8* %37, i64 128
  %119 = bitcast i8* %118 to i64*
  %120 = getelementptr i8, i8* %37, i64 120
  %121 = bitcast i8* %120 to i64*
  %122 = load i64, i64* %119, align 4
  %123 = icmp sgt i64 %117, %122
  br i1 %123, label %when_true_21, label %when_exit_20.when_exit_21_crit_edge

when_exit_20.when_exit_21_crit_edge:              ; preds = %when_exit_20
  %.phi.trans.insert613 = getelementptr i8, i8* %37, i64 136
  %124 = bitcast i8* %.phi.trans.insert613 to i8**
  %.pre614 = load i8*, i8** %124, align 8
  br label %when_exit_21

when_true_21:                                     ; preds = %when_exit_20
  %125 = shl i64 %117, 1
  %126 = tail call i8* @GC_malloc(i64 %125)
  %127 = getelementptr i8, i8* %37, i64 136
  %128 = bitcast i8* %127 to i8**
  %129 = load i8*, i8** %128, align 8
  %130 = load i64, i64* %121, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %126, i8* align 1 %129, i64 %130, i1 false)
  store i64 %125, i64* %121, align 8
  store i64 %125, i64* %119, align 8
  store i8* %126, i8** %128, align 8
  br label %when_exit_21

when_exit_21:                                     ; preds = %when_exit_20.when_exit_21_crit_edge, %when_true_21
  %131 = phi i8* [ %.pre614, %when_exit_20.when_exit_21_crit_edge ], [ %126, %when_true_21 ]
  store i64 %117, i64* %121, align 4
  %132 = load i8*, i8** %.repack99, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %131, i8* align 1 %132, i64 %117, i1 false)
  %133 = load i64, i64* %.repack103, align 8
  %134 = getelementptr i8, i8* %37, i64 152
  %135 = bitcast i8* %134 to i64*
  %136 = getelementptr i8, i8* %37, i64 144
  %137 = bitcast i8* %136 to i64*
  %138 = load i64, i64* %135, align 4
  %139 = icmp sgt i64 %133, %138
  br i1 %139, label %when_true_22, label %when_exit_21.when_exit_22_crit_edge

when_exit_21.when_exit_22_crit_edge:              ; preds = %when_exit_21
  %.phi.trans.insert615 = getelementptr i8, i8* %37, i64 160
  %140 = bitcast i8* %.phi.trans.insert615 to i8**
  %.pre616 = load i8*, i8** %140, align 8
  br label %when_exit_22

when_true_22:                                     ; preds = %when_exit_21
  %141 = shl i64 %133, 1
  %142 = tail call i8* @GC_malloc(i64 %141)
  %143 = getelementptr i8, i8* %37, i64 160
  %144 = bitcast i8* %143 to i8**
  %145 = load i8*, i8** %144, align 8
  %146 = load i64, i64* %137, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %142, i8* align 1 %145, i64 %146, i1 false)
  store i64 %141, i64* %137, align 8
  store i64 %141, i64* %135, align 8
  store i8* %142, i8** %144, align 8
  br label %when_exit_22

when_exit_22:                                     ; preds = %when_exit_21.when_exit_22_crit_edge, %when_true_22
  %147 = phi i8* [ %.pre616, %when_exit_21.when_exit_22_crit_edge ], [ %142, %when_true_22 ]
  store i64 %133, i64* %137, align 4
  %148 = load i8*, i8** %.repack105, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %147, i8* align 1 %148, i64 %133, i1 false)
  %149 = load i64, i64* %.repack109, align 8
  %150 = getelementptr i8, i8* %37, i64 176
  %151 = bitcast i8* %150 to i64*
  %152 = getelementptr i8, i8* %37, i64 168
  %153 = bitcast i8* %152 to i64*
  %154 = load i64, i64* %151, align 4
  %155 = icmp sgt i64 %149, %154
  br i1 %155, label %when_true_23, label %when_exit_22.when_exit_23_crit_edge

when_exit_22.when_exit_23_crit_edge:              ; preds = %when_exit_22
  %.phi.trans.insert617 = getelementptr i8, i8* %37, i64 184
  %156 = bitcast i8* %.phi.trans.insert617 to i8**
  %.pre618 = load i8*, i8** %156, align 8
  br label %when_exit_23

when_true_23:                                     ; preds = %when_exit_22
  %157 = shl i64 %149, 1
  %158 = tail call i8* @GC_malloc(i64 %157)
  %159 = getelementptr i8, i8* %37, i64 184
  %160 = bitcast i8* %159 to i8**
  %161 = load i8*, i8** %160, align 8
  %162 = load i64, i64* %153, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %158, i8* align 1 %161, i64 %162, i1 false)
  store i64 %157, i64* %153, align 8
  store i64 %157, i64* %151, align 8
  store i8* %158, i8** %160, align 8
  br label %when_exit_23

when_exit_23:                                     ; preds = %when_exit_22.when_exit_23_crit_edge, %when_true_23
  %163 = phi i8* [ %.pre618, %when_exit_22.when_exit_23_crit_edge ], [ %158, %when_true_23 ]
  store i64 %149, i64* %153, align 4
  %164 = load i8*, i8** %.repack111, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %163, i8* align 1 %164, i64 %149, i1 false)
  %165 = getelementptr i8, i8* %37, i64 200
  %166 = bitcast i8* %165 to i64*
  %167 = getelementptr i8, i8* %37, i64 192
  %168 = bitcast i8* %167 to i64*
  %169 = load i64, i64* %166, align 4
  %170 = icmp slt i64 %169, 4
  br i1 %170, label %when_true_24, label %when_exit_23.when_exit_24_crit_edge

when_exit_23.when_exit_24_crit_edge:              ; preds = %when_exit_23
  %.phi.trans.insert619 = getelementptr i8, i8* %37, i64 208
  %171 = bitcast i8* %.phi.trans.insert619 to i8**
  %.pre620 = load i8*, i8** %171, align 8
  br label %when_exit_24

when_true_24:                                     ; preds = %when_exit_23
  %172 = tail call i8* @GC_malloc(i64 8)
  %173 = getelementptr i8, i8* %37, i64 208
  %174 = bitcast i8* %173 to i8**
  %175 = load i8*, i8** %174, align 8
  %176 = load i64, i64* %168, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %172, i8* align 1 %175, i64 %176, i1 false)
  store i64 8, i64* %168, align 8
  store i64 8, i64* %166, align 8
  store i8* %172, i8** %174, align 8
  br label %when_exit_24

when_exit_24:                                     ; preds = %when_exit_23.when_exit_24_crit_edge, %when_true_24
  %177 = phi i8* [ %.pre620, %when_exit_23.when_exit_24_crit_edge ], [ %172, %when_true_24 ]
  store i64 4, i64* %168, align 4
  %178 = bitcast i8* %28 to i32*
  %179 = bitcast i8* %177 to i32*
  %180 = load i32, i32* %178, align 1
  store i32 %180, i32* %179, align 1
  %181 = getelementptr i8, i8* %37, i64 224
  %182 = bitcast i8* %181 to i64*
  %183 = getelementptr i8, i8* %37, i64 216
  %184 = bitcast i8* %183 to i64*
  %185 = load i64, i64* %182, align 4
  %186 = icmp slt i64 %185, 5
  br i1 %186, label %when_true_25, label %when_exit_24.when_exit_25_crit_edge

when_exit_24.when_exit_25_crit_edge:              ; preds = %when_exit_24
  %.phi.trans.insert621 = getelementptr i8, i8* %37, i64 232
  %187 = bitcast i8* %.phi.trans.insert621 to i8**
  %.pre622 = load i8*, i8** %187, align 8
  br label %when_exit_25

when_true_25:                                     ; preds = %when_exit_24
  %188 = tail call i8* @GC_malloc(i64 10)
  %189 = getelementptr i8, i8* %37, i64 232
  %190 = bitcast i8* %189 to i8**
  %191 = load i8*, i8** %190, align 8
  %192 = load i64, i64* %184, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %188, i8* align 1 %191, i64 %192, i1 false)
  store i64 10, i64* %184, align 8
  store i64 10, i64* %182, align 8
  store i8* %188, i8** %190, align 8
  br label %when_exit_25

when_exit_25:                                     ; preds = %when_exit_24.when_exit_25_crit_edge, %when_true_25
  %193 = phi i8* [ %.pre622, %when_exit_24.when_exit_25_crit_edge ], [ %188, %when_true_25 ]
  store i64 5, i64* %184, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %193, i8* align 1 %30, i64 5, i1 false)
  %194 = getelementptr i8, i8* %37, i64 248
  %195 = bitcast i8* %194 to i64*
  %196 = getelementptr i8, i8* %37, i64 240
  %197 = bitcast i8* %196 to i64*
  %198 = load i64, i64* %195, align 4
  %199 = icmp slt i64 %198, 6
  br i1 %199, label %when_true_26, label %when_exit_25.when_exit_26_crit_edge

when_exit_25.when_exit_26_crit_edge:              ; preds = %when_exit_25
  %.phi.trans.insert623 = getelementptr i8, i8* %37, i64 256
  %200 = bitcast i8* %.phi.trans.insert623 to i8**
  %.pre624 = load i8*, i8** %200, align 8
  br label %when_exit_26

when_true_26:                                     ; preds = %when_exit_25
  %201 = tail call i8* @GC_malloc(i64 12)
  %202 = getelementptr i8, i8* %37, i64 256
  %203 = bitcast i8* %202 to i8**
  %204 = load i8*, i8** %203, align 8
  %205 = load i64, i64* %197, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %201, i8* align 1 %204, i64 %205, i1 false)
  store i64 12, i64* %197, align 8
  store i64 12, i64* %195, align 8
  store i8* %201, i8** %203, align 8
  br label %when_exit_26

when_exit_26:                                     ; preds = %when_exit_25.when_exit_26_crit_edge, %when_true_26
  %206 = phi i8* [ %.pre624, %when_exit_25.when_exit_26_crit_edge ], [ %201, %when_true_26 ]
  store i64 6, i64* %197, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %206, i8* align 1 %31, i64 6, i1 false)
  %207 = getelementptr i8, i8* %37, i64 272
  %208 = bitcast i8* %207 to i64*
  %209 = getelementptr i8, i8* %37, i64 264
  %210 = bitcast i8* %209 to i64*
  %211 = load i64, i64* %208, align 4
  %212 = icmp slt i64 %211, 3
  br i1 %212, label %when_true_27, label %when_exit_26.when_exit_27_crit_edge

when_exit_26.when_exit_27_crit_edge:              ; preds = %when_exit_26
  %.phi.trans.insert625 = getelementptr i8, i8* %37, i64 280
  %213 = bitcast i8* %.phi.trans.insert625 to i8**
  %.pre626 = load i8*, i8** %213, align 8
  br label %when_exit_27

when_true_27:                                     ; preds = %when_exit_26
  %214 = tail call i8* @GC_malloc(i64 6)
  %215 = getelementptr i8, i8* %37, i64 280
  %216 = bitcast i8* %215 to i8**
  %217 = load i8*, i8** %216, align 8
  %218 = load i64, i64* %210, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %214, i8* align 1 %217, i64 %218, i1 false)
  store i64 6, i64* %210, align 8
  store i64 6, i64* %208, align 8
  store i8* %214, i8** %216, align 8
  br label %when_exit_27

when_exit_27:                                     ; preds = %when_exit_26.when_exit_27_crit_edge, %when_true_27
  %219 = phi i8* [ %.pre626, %when_exit_26.when_exit_27_crit_edge ], [ %214, %when_true_27 ]
  store i64 3, i64* %210, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %219, i8* align 1 %32, i64 3, i1 false)
  %220 = getelementptr i8, i8* %37, i64 296
  %221 = bitcast i8* %220 to i64*
  %222 = getelementptr i8, i8* %37, i64 288
  %223 = bitcast i8* %222 to i64*
  %224 = load i64, i64* %221, align 4
  %225 = icmp slt i64 %224, 4
  br i1 %225, label %when_true_28, label %when_exit_27.when_exit_28_crit_edge

when_exit_27.when_exit_28_crit_edge:              ; preds = %when_exit_27
  %.phi.trans.insert627 = getelementptr i8, i8* %37, i64 304
  %226 = bitcast i8* %.phi.trans.insert627 to i8**
  %.pre628 = load i8*, i8** %226, align 8
  br label %when_exit_28

when_true_28:                                     ; preds = %when_exit_27
  %227 = tail call i8* @GC_malloc(i64 8)
  %228 = getelementptr i8, i8* %37, i64 304
  %229 = bitcast i8* %228 to i8**
  %230 = load i8*, i8** %229, align 8
  %231 = load i64, i64* %223, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %227, i8* align 1 %230, i64 %231, i1 false)
  store i64 8, i64* %223, align 8
  store i64 8, i64* %221, align 8
  store i8* %227, i8** %229, align 8
  br label %when_exit_28

when_exit_28:                                     ; preds = %when_exit_27.when_exit_28_crit_edge, %when_true_28
  %232 = phi i8* [ %.pre628, %when_exit_27.when_exit_28_crit_edge ], [ %227, %when_true_28 ]
  store i64 4, i64* %223, align 4
  %233 = bitcast i8* %33 to i32*
  %234 = bitcast i8* %232 to i32*
  %235 = load i32, i32* %233, align 1
  store i32 %235, i32* %234, align 1
  %236 = getelementptr i8, i8* %37, i64 320
  %237 = bitcast i8* %236 to i64*
  %238 = getelementptr i8, i8* %37, i64 312
  %239 = bitcast i8* %238 to i64*
  %240 = load i64, i64* %237, align 4
  %241 = icmp slt i64 %240, 4
  br i1 %241, label %when_true_29, label %when_exit_28.when_exit_29_crit_edge

when_exit_28.when_exit_29_crit_edge:              ; preds = %when_exit_28
  %.phi.trans.insert629 = getelementptr i8, i8* %37, i64 328
  %242 = bitcast i8* %.phi.trans.insert629 to i8**
  %.pre630 = load i8*, i8** %242, align 8
  br label %for_body_0.lr.ph

when_true_29:                                     ; preds = %when_exit_28
  %243 = tail call i8* @GC_malloc(i64 8)
  %244 = getelementptr i8, i8* %37, i64 328
  %245 = bitcast i8* %244 to i8**
  %246 = load i8*, i8** %245, align 8
  %247 = load i64, i64* %239, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %243, i8* align 1 %246, i64 %247, i1 false)
  store i64 8, i64* %239, align 8
  store i64 8, i64* %237, align 8
  store i8* %243, i8** %245, align 8
  br label %for_body_0.lr.ph

for_body_0.lr.ph:                                 ; preds = %when_true_29, %when_exit_28.when_exit_29_crit_edge
  %248 = phi i8* [ %.pre630, %when_exit_28.when_exit_29_crit_edge ], [ %243, %when_true_29 ]
  store i64 4, i64* %239, align 4
  %249 = bitcast i8* %35 to i32*
  %250 = bitcast i8* %248 to i32*
  %251 = load i32, i32* %249, align 1
  store i32 %251, i32* %250, align 1
  %252 = load i64, i64* %.repack58, align 8
  %253 = load i8*, i8** %.repack60, align 8
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %if_exit_0
  %storemerge602 = phi i64 [ 0, %for_body_0.lr.ph ], [ %265, %if_exit_0 ]
  %254 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %44, i64 %storemerge602, i32 0
  %255 = load i64, i64* %254, align 4
  %256 = icmp eq i64 %252, %255
  br i1 %256, label %eqeq_table_exit_0, label %if_exit_0

eqeq_table_exit_0:                                ; preds = %for_body_0
  %257 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %44, i64 %storemerge602, i32 2
  %258 = load i8*, i8** %257, align 8
  %259 = tail call i64 @memcmp(i8* %253, i8* %258, i64 %252)
  %260 = icmp eq i64 %259, 0
  br i1 %260, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %eqeq_table_exit_0
  %261 = load i64, i64* %4, align 8
  %262 = load i64, i64* %5, align 8
  %.unpack176.unpack182 = insertvalue { i64, i64 } { i64 4, i64 undef }, i64 %261, 1
  %263 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack176.unpack182, 0
  %.unpack176179 = insertvalue { { i64, i64 }, i64 } %263, i64 %262, 1
  %264 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack176179, 1
  ret { i64, { { i64, i64 }, i64 } } %264

if_exit_0:                                        ; preds = %for_body_0, %eqeq_table_exit_0
  %265 = add nuw nsw i64 %storemerge602, 1
  %266 = icmp ult i64 %265, 14
  br i1 %266, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %if_exit_0
  tail call void @lexer.popString({ i64, i64, i8* }* %s)
  br label %when_exit_0

when_exit_0:                                      ; preds = %for_exit_0, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexNewline({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
exit_0:
  br label %while_body_0.outer

while_body_0.outer:                               ; preds = %when_exit_3, %exit_0
  %.0128.ph = phi i64 [ %6, %when_exit_3 ], [ %idx, %exit_0 ]
  %.sroa.0.0125.ph = phi i64 [ %7, %when_exit_3 ], [ 0, %exit_0 ]
  %.sroa.8.0124.ph = phi i64 [ %.sroa.8.1, %when_exit_3 ], [ 0, %exit_0 ]
  %.sroa.11.0123.ph = phi i8* [ %.sroa.11.1, %when_exit_3 ], [ null, %exit_0 ]
  br label %while_body_0

while_body_0:                                     ; preds = %while_body_0.outer, %when_true_3
  %.0128 = phi i64 [ %.elt70.elt72, %when_true_3 ], [ %.0128.ph, %while_body_0.outer ]
  %0 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.0128)
  %1 = alloca { i64, i8 }, align 8
  store { i64, i8 } %0, { i64, i8 }* %1, align 8
  %2 = extractvalue { i64, i8 } %0, 0
  %3 = icmp eq i64 %2, 1
  br i1 %3, label %when_true_0, label %when_true_3

when_true_0:                                      ; preds = %while_body_0
  %4 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %1, i64 0, i32 1
  %5 = load i8, i8* %4, align 8
  switch i8 %5, label %when_true_3 [
    i8 32, label %when_true_1
    i8 10, label %when_true_1
    i8 9, label %when_true_1
  ]

when_true_1:                                      ; preds = %when_true_0, %when_true_0, %when_true_0
  %6 = add i64 %.0128, 1
  %7 = add i64 %.sroa.0.0125.ph, 1
  %8 = icmp sgt i64 %7, %.sroa.8.0124.ph
  br i1 %8, label %when_true_2, label %when_exit_3

when_true_2:                                      ; preds = %when_true_1
  %9 = shl i64 %7, 1
  %10 = tail call i8* @GC_malloc(i64 %9)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %10, i8* align 1 %.sroa.11.0123.ph, i64 %.sroa.0.0125.ph, i1 false)
  br label %when_exit_3

when_true_3:                                      ; preds = %when_true_0, %while_body_0
  %11 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexComment({ i64, i64, i8* }* %s, i64 %.0128)
  %.elt68 = extractvalue { i64, { { i64, i64 }, i64 } } %11, 0
  %.elt70 = extractvalue { i64, { { i64, i64 }, i64 } } %11, 1
  %.elt70.elt72 = extractvalue { { i64, i64 }, i64 } %.elt70, 1
  %12 = icmp eq i64 %.elt68, 1
  br i1 %12, label %while_body_0, label %while_exit_0

when_exit_3:                                      ; preds = %when_true_2, %when_true_1
  %.sroa.11.1 = phi i8* [ %10, %when_true_2 ], [ %.sroa.11.0123.ph, %when_true_1 ]
  %.sroa.8.1 = phi i64 [ %9, %when_true_2 ], [ %.sroa.8.0124.ph, %when_true_1 ]
  %13 = getelementptr i8, i8* %.sroa.11.1, i64 %.sroa.0.0125.ph
  store i8 %5, i8* %13, align 1
  br label %while_body_0.outer

while_exit_0:                                     ; preds = %when_true_3
  %14 = icmp sgt i64 %.sroa.0.0125.ph, 0
  br i1 %14, label %for_body_0, label %if_true_1

for_body_0:                                       ; preds = %while_exit_0, %when_true_6
  %15 = phi i64 [ %spec.select, %when_true_6 ], [ -1, %while_exit_0 ]
  %16 = phi i64 [ %spec.select145, %when_true_6 ], [ -1, %while_exit_0 ]
  %17 = phi i64 [ %24, %when_true_6 ], [ 0, %while_exit_0 ]
  %18 = xor i64 %17, -1
  %19 = add i64 %.sroa.0.0125.ph, %18
  %20 = icmp eq i64 %16, -1
  br i1 %20, label %when_true_6, label %for_exit_0

when_true_6:                                      ; preds = %for_body_0
  %21 = getelementptr i8, i8* %.sroa.11.0123.ph, i64 %19
  %22 = load i8, i8* %21, align 1
  %23 = icmp eq i8 %22, 10
  %spec.select = select i1 %23, i64 %19, i64 %15
  %spec.select145 = select i1 %23, i64 %19, i64 -1
  %24 = add nuw nsw i64 %17, 1
  %25 = icmp slt i64 %24, %.sroa.0.0125.ph
  br i1 %25, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_true_6, %for_body_0
  %26 = phi i64 [ %spec.select, %when_true_6 ], [ %15, %for_body_0 ]
  %27 = icmp eq i64 %26, -1
  br i1 %27, label %if_true_1, label %if_exit_1

if_true_1:                                        ; preds = %while_exit_0, %for_exit_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }

if_exit_1:                                        ; preds = %for_exit_0
  %28 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %29 = load i64, i64* %28, align 4
  %30 = add i64 %29, 1
  %31 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %32 = load i64, i64* %31, align 4
  %33 = icmp sgt i64 %30, %32
  br i1 %33, label %when_true_7, label %if_exit_1.when_exit_7_crit_edge

if_exit_1.when_exit_7_crit_edge:                  ; preds = %if_exit_1
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_7

when_true_7:                                      ; preds = %if_exit_1
  %34 = shl i64 %30, 1
  %35 = tail call i8* @GC_malloc(i64 %34)
  %36 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %37 = load i8*, i8** %36, align 8
  %38 = load i64, i64* %28, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %35, i8* align 1 %37, i64 %38, i1 false)
  store i64 %34, i64* %28, align 8
  store i64 %34, i64* %31, align 8
  store i8* %35, i8** %36, align 8
  br label %when_exit_7

when_exit_7:                                      ; preds = %if_exit_1.when_exit_7_crit_edge, %when_true_7
  %39 = phi i8* [ %.pre, %if_exit_1.when_exit_7_crit_edge ], [ %35, %when_true_7 ]
  store i64 %30, i64* %28, align 4
  %40 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %41 = getelementptr i8, i8* %39, i64 %29
  store i8 0, i8* %41, align 1
  %42 = add nuw i64 %26, 1
  %43 = icmp sgt i64 %42, 0
  %storemerge = select i1 %43, i64 %42, i64 0
  %44 = icmp slt i64 %storemerge, %.sroa.0.0125.ph
  br i1 %44, label %for_body_1, label %for_exit_1

for_body_1:                                       ; preds = %when_exit_7, %when_exit_8
  %storemerge121122 = phi i64 [ %57, %when_exit_8 ], [ %storemerge, %when_exit_7 ]
  %45 = getelementptr i8, i8* %.sroa.11.0123.ph, i64 %storemerge121122
  %46 = load i64, i64* %28, align 4
  %47 = add i64 %46, 1
  %48 = load i64, i64* %31, align 4
  %49 = icmp sgt i64 %47, %48
  br i1 %49, label %when_true_8, label %for_body_1.when_exit_8_crit_edge

for_body_1.when_exit_8_crit_edge:                 ; preds = %for_body_1
  %.pre130 = load i8*, i8** %40, align 8
  br label %when_exit_8

when_true_8:                                      ; preds = %for_body_1
  %50 = shl i64 %47, 1
  %51 = tail call i8* @GC_malloc(i64 %50)
  %52 = load i8*, i8** %40, align 8
  %53 = load i64, i64* %28, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %51, i8* align 1 %52, i64 %53, i1 false)
  store i64 %50, i64* %28, align 8
  store i64 %50, i64* %31, align 8
  store i8* %51, i8** %40, align 8
  br label %when_exit_8

when_exit_8:                                      ; preds = %for_body_1.when_exit_8_crit_edge, %when_true_8
  %54 = phi i8* [ %.pre130, %for_body_1.when_exit_8_crit_edge ], [ %51, %when_true_8 ]
  store i64 %47, i64* %28, align 4
  %55 = getelementptr i8, i8* %54, i64 %46
  %56 = load i8, i8* %45, align 1
  store i8 %56, i8* %55, align 1
  %57 = add nuw nsw i64 %storemerge121122, 1
  %58 = icmp slt i64 %57, %.sroa.0.0125.ph
  br i1 %58, label %for_body_1, label %for_exit_1

for_exit_1:                                       ; preds = %when_exit_8, %when_exit_7
  %.unpack21.unpack27 = insertvalue { i64, i64 } { i64 8, i64 undef }, i64 %30, 1
  %59 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack21.unpack27, 0
  %.unpack2124 = insertvalue { { i64, i64 }, i64 } %59, i64 %.0128, 1
  %60 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack2124, 1
  ret { i64, { { i64, i64 }, i64 } } %60
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexStringLit({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %.fca.1.extract = extractvalue { i64, i8 } %1, 1
  %2 = extractvalue { i64, i8 } %1, 0
  %3 = icmp eq i64 %2, 1
  %4 = icmp eq i8 %.fca.1.extract, 34
  %or.cond = and i1 %3, %4
  br i1 %or.cond, label %when_true_1, label %when_exit_0

when_true_1:                                      ; preds = %0
  %5 = add i64 %idx, 1
  %6 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %7 = load i64, i64* %6, align 4
  %8 = add i64 %7, 1
  %9 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %10 = load i64, i64* %9, align 4
  %11 = icmp sgt i64 %8, %10
  br i1 %11, label %when_true_2, label %when_true_1.when_exit_2_crit_edge

when_true_1.when_exit_2_crit_edge:                ; preds = %when_true_1
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_2

when_true_2:                                      ; preds = %when_true_1
  %12 = shl i64 %8, 1
  %13 = tail call i8* @GC_malloc(i64 %12)
  %14 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %15 = load i8*, i8** %14, align 8
  %16 = load i64, i64* %6, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %13, i8* align 1 %15, i64 %16, i1 false)
  store i64 %12, i64* %6, align 8
  store i64 %12, i64* %9, align 8
  store i8* %13, i8** %14, align 8
  br label %when_exit_2

when_exit_2:                                      ; preds = %when_true_1.when_exit_2_crit_edge, %when_true_2
  %17 = phi i8* [ %.pre, %when_true_1.when_exit_2_crit_edge ], [ %13, %when_true_2 ]
  store i64 %8, i64* %6, align 4
  %18 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %19 = getelementptr i8, i8* %17, i64 %7
  store i8 0, i8* %19, align 1
  %20 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexStringLitChar({ i64, i64, i8* }* nonnull %s, i64 %5)
  %.elt314 = extractvalue { i64, { { i64, i64 }, i64 } } %20, 0
  %.elt22315 = extractvalue { i64, { { i64, i64 }, i64 } } %20, 1
  %.elt22.elt316 = extractvalue { { i64, i64 }, i64 } %.elt22315, 0
  %.elt22.elt.elt317 = extractvalue { i64, i64 } %.elt22.elt316, 0
  %21 = icmp eq i64 %.elt314, 1
  %22 = icmp eq i64 %.elt22.elt.elt317, 0
  %or.cond286320 = and i1 %21, %22
  br i1 %or.cond286320, label %when_true_4.lr.ph, label %while_exit_0

when_true_4.lr.ph:                                ; preds = %when_exit_2
  %.elt22.elt.elt26318 = extractvalue { i64, i64 } %.elt22.elt316, 1
  %23 = trunc i64 %.elt22.elt.elt26318 to i8
  %.elt22.elt24319 = extractvalue { { i64, i64 }, i64 } %.elt22315, 1
  br label %when_true_4

while_cond_0.loopexit:                            ; preds = %when_exit_21
  %24 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexStringLitChar({ i64, i64, i8* }* nonnull %s, i64 %.elt22.elt24321)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %24, 0
  %.elt22 = extractvalue { i64, { { i64, i64 }, i64 } } %24, 1
  %.elt22.elt = extractvalue { { i64, i64 }, i64 } %.elt22, 0
  %.elt22.elt.elt = extractvalue { i64, i64 } %.elt22.elt, 0
  %.elt22.elt.elt26 = extractvalue { i64, i64 } %.elt22.elt, 1
  %.elt22.elt24 = extractvalue { { i64, i64 }, i64 } %.elt22, 1
  %25 = icmp eq i64 %.elt, 1
  %26 = icmp eq i64 %.elt22.elt.elt, 0
  %or.cond286 = and i1 %25, %26
  %27 = trunc i64 %.elt22.elt.elt26 to i8
  br i1 %or.cond286, label %when_true_4, label %while_exit_0

when_true_4:                                      ; preds = %when_true_4.lr.ph, %while_cond_0.loopexit
  %28 = phi i8 [ %23, %when_true_4.lr.ph ], [ %27, %while_cond_0.loopexit ]
  %.elt22.elt24321 = phi i64 [ %.elt22.elt24319, %when_true_4.lr.ph ], [ %.elt22.elt24, %while_cond_0.loopexit ]
  %29 = alloca { i64, i64, i8* }, align 8
  %.repack58 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %29, i64 0, i32 0
  %.repack59 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %29, i64 0, i32 1
  %.repack60 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %29, i64 0, i32 2
  %30 = bitcast { i64, i64, i8* }* %29 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %30, i8 0, i64 24, i1 false)
  switch i8 %28, label %when_exit_20 [
    i8 10, label %when_exit_6
    i8 9, label %when_exit_9
    i8 0, label %when_exit_12
    i8 34, label %when_exit_15
    i8 92, label %when_exit_18
  ]

when_exit_6:                                      ; preds = %when_true_4
  %31 = tail call i8* @GC_malloc(i64 4)
  %32 = bitcast i8* %31 to i16*
  store i16 28252, i16* %32, align 1
  %33 = load i64, i64* %.repack59, align 8
  %34 = icmp slt i64 %33, 2
  br i1 %34, label %when_true_7, label %when_exit_6.when_exit_5.thread_crit_edge

when_exit_6.when_exit_5.thread_crit_edge:         ; preds = %when_exit_6
  %.phi.trans.insert296 = bitcast i8** %.repack60 to i16**
  %.pre297 = load i16*, i16** %.phi.trans.insert296, align 8
  br label %when_exit_5.thread

when_true_7:                                      ; preds = %when_exit_6
  %35 = tail call i8* @GC_malloc(i64 4)
  %36 = load i8*, i8** %.repack60, align 8
  %37 = load i64, i64* %.repack58, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %35, i8* align 1 %36, i64 %37, i1 false)
  store i64 4, i64* %.repack58, align 8
  store i64 4, i64* %.repack59, align 8
  store i8* %35, i8** %.repack60, align 8
  %38 = bitcast i8* %35 to i16*
  br label %when_exit_5.thread

when_exit_5.thread:                               ; preds = %when_exit_6.when_exit_5.thread_crit_edge, %when_true_7
  %39 = phi i16* [ %.pre297, %when_exit_6.when_exit_5.thread_crit_edge ], [ %38, %when_true_7 ]
  store i64 2, i64* %.repack58, align 8
  %40 = load i16, i16* %32, align 1
  store i16 %40, i16* %39, align 1
  br label %for_body_0.preheader

when_exit_9:                                      ; preds = %when_true_4
  %41 = tail call i8* @GC_malloc(i64 4)
  %42 = bitcast i8* %41 to i16*
  store i16 29788, i16* %42, align 1
  %43 = load i64, i64* %.repack59, align 8
  %44 = icmp slt i64 %43, 2
  br i1 %44, label %when_true_10, label %when_exit_9.when_exit_8.thread_crit_edge

when_exit_9.when_exit_8.thread_crit_edge:         ; preds = %when_exit_9
  %.phi.trans.insert294 = bitcast i8** %.repack60 to i16**
  %.pre295 = load i16*, i16** %.phi.trans.insert294, align 8
  br label %when_exit_8.thread

when_true_10:                                     ; preds = %when_exit_9
  %45 = tail call i8* @GC_malloc(i64 4)
  %46 = load i8*, i8** %.repack60, align 8
  %47 = load i64, i64* %.repack58, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %45, i8* align 1 %46, i64 %47, i1 false)
  store i64 4, i64* %.repack58, align 8
  store i64 4, i64* %.repack59, align 8
  store i8* %45, i8** %.repack60, align 8
  %48 = bitcast i8* %45 to i16*
  br label %when_exit_8.thread

when_exit_8.thread:                               ; preds = %when_exit_9.when_exit_8.thread_crit_edge, %when_true_10
  %49 = phi i16* [ %.pre295, %when_exit_9.when_exit_8.thread_crit_edge ], [ %48, %when_true_10 ]
  store i64 2, i64* %.repack58, align 8
  %50 = load i16, i16* %42, align 1
  store i16 %50, i16* %49, align 1
  br label %for_body_0.preheader

when_exit_12:                                     ; preds = %when_true_4
  %51 = tail call i8* @GC_malloc(i64 4)
  %52 = bitcast i8* %51 to i16*
  store i16 12380, i16* %52, align 1
  %53 = load i64, i64* %.repack59, align 8
  %54 = icmp slt i64 %53, 2
  br i1 %54, label %when_true_13, label %when_exit_12.when_exit_11.thread_crit_edge

when_exit_12.when_exit_11.thread_crit_edge:       ; preds = %when_exit_12
  %.phi.trans.insert302 = bitcast i8** %.repack60 to i16**
  %.pre303 = load i16*, i16** %.phi.trans.insert302, align 8
  br label %when_exit_11.thread

when_true_13:                                     ; preds = %when_exit_12
  %55 = tail call i8* @GC_malloc(i64 4)
  %56 = load i8*, i8** %.repack60, align 8
  %57 = load i64, i64* %.repack58, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %55, i8* align 1 %56, i64 %57, i1 false)
  store i64 4, i64* %.repack58, align 8
  store i64 4, i64* %.repack59, align 8
  store i8* %55, i8** %.repack60, align 8
  %58 = bitcast i8* %55 to i16*
  br label %when_exit_11.thread

when_exit_11.thread:                              ; preds = %when_exit_12.when_exit_11.thread_crit_edge, %when_true_13
  %59 = phi i16* [ %.pre303, %when_exit_12.when_exit_11.thread_crit_edge ], [ %58, %when_true_13 ]
  store i64 2, i64* %.repack58, align 8
  %60 = load i16, i16* %52, align 1
  store i16 %60, i16* %59, align 1
  br label %for_body_0.preheader

when_exit_15:                                     ; preds = %when_true_4
  %61 = tail call i8* @GC_malloc(i64 4)
  %62 = bitcast i8* %61 to i16*
  store i16 8796, i16* %62, align 1
  %63 = load i64, i64* %.repack59, align 8
  %64 = icmp slt i64 %63, 2
  br i1 %64, label %when_true_16, label %when_exit_15.when_exit_14.thread_crit_edge

when_exit_15.when_exit_14.thread_crit_edge:       ; preds = %when_exit_15
  %.phi.trans.insert300 = bitcast i8** %.repack60 to i16**
  %.pre301 = load i16*, i16** %.phi.trans.insert300, align 8
  br label %when_exit_14.thread

when_true_16:                                     ; preds = %when_exit_15
  %65 = tail call i8* @GC_malloc(i64 4)
  %66 = load i8*, i8** %.repack60, align 8
  %67 = load i64, i64* %.repack58, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %65, i8* align 1 %66, i64 %67, i1 false)
  store i64 4, i64* %.repack58, align 8
  store i64 4, i64* %.repack59, align 8
  store i8* %65, i8** %.repack60, align 8
  %68 = bitcast i8* %65 to i16*
  br label %when_exit_14.thread

when_exit_14.thread:                              ; preds = %when_exit_15.when_exit_14.thread_crit_edge, %when_true_16
  %69 = phi i16* [ %.pre301, %when_exit_15.when_exit_14.thread_crit_edge ], [ %68, %when_true_16 ]
  store i64 2, i64* %.repack58, align 8
  %70 = load i16, i16* %62, align 1
  store i16 %70, i16* %69, align 1
  br label %for_body_0.preheader

when_exit_18:                                     ; preds = %when_true_4
  %71 = tail call i8* @GC_malloc(i64 4)
  %72 = bitcast i8* %71 to i16*
  store i16 23644, i16* %72, align 1
  %73 = load i64, i64* %.repack59, align 8
  %74 = icmp slt i64 %73, 2
  br i1 %74, label %when_true_19, label %when_exit_18.when_exit_17.thread_crit_edge

when_exit_18.when_exit_17.thread_crit_edge:       ; preds = %when_exit_18
  %.phi.trans.insert298 = bitcast i8** %.repack60 to i16**
  %.pre299 = load i16*, i16** %.phi.trans.insert298, align 8
  br label %when_exit_17.thread

when_true_19:                                     ; preds = %when_exit_18
  %75 = tail call i8* @GC_malloc(i64 4)
  %76 = load i8*, i8** %.repack60, align 8
  %77 = load i64, i64* %.repack58, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %75, i8* align 1 %76, i64 %77, i1 false)
  store i64 4, i64* %.repack58, align 8
  store i64 4, i64* %.repack59, align 8
  store i8* %75, i8** %.repack60, align 8
  %78 = bitcast i8* %75 to i16*
  br label %when_exit_17.thread

when_exit_17.thread:                              ; preds = %when_exit_18.when_exit_17.thread_crit_edge, %when_true_19
  %79 = phi i16* [ %.pre299, %when_exit_18.when_exit_17.thread_crit_edge ], [ %78, %when_true_19 ]
  store i64 2, i64* %.repack58, align 8
  %80 = load i16, i16* %72, align 1
  store i16 %80, i16* %79, align 1
  br label %for_body_0.preheader

when_exit_20:                                     ; preds = %when_true_4
  %81 = tail call i8* @GC_malloc(i64 2)
  store i64 2, i64* %.repack59, align 8
  store i8* %81, i8** %.repack60, align 8
  store i64 1, i64* %.repack58, align 8
  store i8 %28, i8* %81, align 1
  br label %for_body_0.preheader

for_body_0.preheader:                             ; preds = %when_exit_20, %when_exit_5.thread, %when_exit_8.thread, %when_exit_11.thread, %when_exit_14.thread, %when_exit_17.thread
  %82 = load i8*, i8** %.repack60, align 8
  %83 = load i64, i64* %.repack58, align 8
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.preheader, %when_exit_21
  %storemerge290 = phi i64 [ %96, %when_exit_21 ], [ 0, %for_body_0.preheader ]
  %84 = getelementptr i8, i8* %82, i64 %storemerge290
  %85 = load i8, i8* %84, align 1
  %86 = load i64, i64* %6, align 4
  %87 = add i64 %86, 1
  %88 = load i64, i64* %9, align 4
  %89 = icmp sgt i64 %87, %88
  br i1 %89, label %when_true_21, label %for_body_0.when_exit_21_crit_edge

for_body_0.when_exit_21_crit_edge:                ; preds = %for_body_0
  %.pre304 = load i8*, i8** %18, align 8
  br label %when_exit_21

when_true_21:                                     ; preds = %for_body_0
  %90 = shl i64 %87, 1
  %91 = tail call i8* @GC_malloc(i64 %90)
  %92 = load i8*, i8** %18, align 8
  %93 = load i64, i64* %6, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %91, i8* align 1 %92, i64 %93, i1 false)
  store i64 %90, i64* %6, align 8
  store i64 %90, i64* %9, align 8
  store i8* %91, i8** %18, align 8
  br label %when_exit_21

when_exit_21:                                     ; preds = %for_body_0.when_exit_21_crit_edge, %when_true_21
  %94 = phi i8* [ %.pre304, %for_body_0.when_exit_21_crit_edge ], [ %91, %when_true_21 ]
  store i64 %87, i64* %6, align 4
  %95 = getelementptr i8, i8* %94, i64 %86
  store i8 %85, i8* %95, align 1
  %96 = add nuw nsw i64 %storemerge290, 1
  %97 = icmp slt i64 %96, %83
  br i1 %97, label %for_body_0, label %while_cond_0.loopexit

while_exit_0:                                     ; preds = %while_cond_0.loopexit, %when_exit_2
  %.0.ph.lcssa = phi i64 [ %5, %when_exit_2 ], [ %.elt22.elt24321, %while_cond_0.loopexit ]
  %98 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %.0.ph.lcssa)
  %99 = alloca { i64, i8 }, align 8
  store { i64, i8 } %98, { i64, i8 }* %99, align 8
  %100 = extractvalue { i64, i8 } %98, 0
  %101 = icmp eq i64 %100, 1
  br i1 %101, label %when_true_22, label %when_exit_22

when_true_22:                                     ; preds = %while_exit_0
  %102 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %99, i64 0, i32 1
  %103 = load i8, i8* %102, align 8
  %104 = icmp eq i8 %103, 34
  br i1 %104, label %when_true_23, label %when_exit_22

when_true_23:                                     ; preds = %when_true_22
  %105 = add i64 %.0.ph.lcssa, 1
  %.unpack47.unpack53 = insertvalue { i64, i64 } { i64 10, i64 undef }, i64 %8, 1
  %106 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack47.unpack53, 0
  %.unpack4750 = insertvalue { { i64, i64 }, i64 } %106, i64 %105, 1
  %107 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack4750, 1
  ret { i64, { { i64, i64 }, i64 } } %107

when_exit_22:                                     ; preds = %when_true_22, %while_exit_0
  tail call void @lexer.popString({ i64, i64, i8* }* %s)
  br label %when_exit_0

when_exit_0:                                      ; preds = %when_exit_22, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexStringLitChar({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = add i64 %idx, 1
  %3 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %2)
  %.fca.1.extract = extractvalue { i64, i8 } %1, 1
  %.fca.0.extract132 = extractvalue { i64, i8 } %3, 0
  %.fca.1.extract134 = extractvalue { i64, i8 } %3, 1
  %4 = extractvalue { i64, i8 } %1, 0
  %5 = icmp eq i64 %4, 1
  %6 = icmp eq i8 %.fca.1.extract, 92
  %or.cond = and i1 %5, %6
  %7 = icmp eq i64 %.fca.0.extract132, 1
  %or.cond136 = and i1 %or.cond, %7
  %8 = icmp eq i8 %.fca.1.extract134, 34
  %or.cond137 = and i1 %8, %or.cond136
  br i1 %or.cond137, label %when_true_3, label %when_exit_0

when_true_3:                                      ; preds = %0
  %9 = alloca { i64, i64 }, align 8
  %10 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %9, i64 0, i32 0
  store i64 0, i64* %10, align 8
  %11 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %9, i64 0, i32 1
  %12 = bitcast i64* %11 to i8*
  store i8 34, i8* %12, align 8
  %13 = add i64 %idx, 2
  %.unpack117 = load i64, i64* %11, align 8
  %.unpack125.unpack131 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack117, 1
  %14 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack125.unpack131, 0
  %.unpack125128 = insertvalue { { i64, i64 }, i64 } %14, i64 %13, 1
  %15 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack125128, 1
  ret { i64, { { i64, i64 }, i64 } } %15

when_exit_0:                                      ; preds = %0
  %16 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexEscaped({ i64, i64, i8* }* %s, i64 %idx)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %16, 0
  %17 = icmp eq i64 %.elt, 1
  br i1 %17, label %when_true_4, label %when_exit_4

when_true_4:                                      ; preds = %when_exit_0
  %.elt2 = extractvalue { i64, { { i64, i64 }, i64 } } %16, 1
  %18 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt2, 1
  ret { i64, { { i64, i64 }, i64 } } %18

when_exit_4:                                      ; preds = %when_exit_0
  %19 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %20 = alloca { i64, i8 }, align 8
  store { i64, i8 } %19, { i64, i8 }* %20, align 8
  %21 = extractvalue { i64, i8 } %19, 0
  %22 = icmp eq i64 %21, 1
  br i1 %22, label %when_true_5, label %when_exit_5

when_true_5:                                      ; preds = %when_exit_4
  %23 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %20, i64 0, i32 1
  %24 = load i8, i8* %23, align 8
  %25 = tail call i1 @chars.isAlpha(i8 %24)
  %26 = tail call i1 @chars.isDigit(i8 %24)
  %27 = or i1 %25, %26
  %28 = icmp eq i8 %24, 39
  %29 = or i1 %28, %27
  %30 = icmp eq i8 %24, 32
  %31 = or i1 %30, %29
  %32 = icmp eq i8 %24, 9
  %33 = or i1 %32, %31
  br i1 %33, label %when_true_6, label %when_exit_5

when_true_6:                                      ; preds = %when_true_5
  %34 = alloca { i64, i64 }, align 8
  %35 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %34, i64 0, i32 0
  store i64 0, i64* %35, align 8
  %36 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %34, i64 0, i32 1
  %37 = bitcast i64* %36 to i8*
  store i8 %24, i8* %37, align 8
  %.unpack73 = load i64, i64* %36, align 8
  %.unpack81.unpack87 = insertvalue { i64, i64 } { i64 0, i64 undef }, i64 %.unpack73, 1
  %38 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack81.unpack87, 0
  %.unpack8184 = insertvalue { { i64, i64 }, i64 } %38, i64 %2, 1
  %39 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack8184, 1
  ret { i64, { { i64, i64 }, i64 } } %39

when_exit_5:                                      ; preds = %when_true_5, %when_exit_4
  %40 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexGraphic({ i64, i64, i8* }* %s, i64 %idx)
  %.elt17 = extractvalue { i64, { { i64, i64 }, i64 } } %40, 0
  %41 = icmp eq i64 %.elt17, 1
  br i1 %41, label %when_true_7, label %when_exit_7

when_true_7:                                      ; preds = %when_exit_5
  %.elt19 = extractvalue { i64, { { i64, i64 }, i64 } } %40, 1
  %42 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.elt19, 1
  ret { i64, { { i64, i64 }, i64 } } %42

when_exit_7:                                      ; preds = %when_exit_5
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexSym({ i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
when_exit_0:
  %0 = tail call i8* @GC_malloc(i64 42)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %0, i8* align 16 getelementptr inbounds ([22 x i8], [22 x i8]* @lexer.str_32, i64 0, i64 0), i64 21, i1 false)
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = extractvalue { i64, i8 } %1, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_true_1, label %when_exit_1

when_true_1:                                      ; preds = %when_exit_0
  %5 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 1
  %6 = load i8, i8* %5, align 8
  switch i8 %6, label %when_exit_1 [
    i8 43, label %if_true_0
    i8 45, label %if_true_0
    i8 42, label %if_true_0
    i8 47, label %if_true_0
    i8 37, label %if_true_0
    i8 60, label %if_true_0
    i8 62, label %if_true_0
    i8 91, label %if_true_0
    i8 93, label %if_true_0
    i8 123, label %if_true_0
    i8 125, label %if_true_0
    i8 40, label %if_true_0
    i8 41, label %if_true_0
    i8 46, label %if_true_0
    i8 44, label %if_true_0
    i8 59, label %if_true_0
    i8 58, label %if_true_0
    i8 124, label %if_true_0
    i8 61, label %if_true_0
    i8 95, label %if_true_0
    i8 33, label %if_true_0
  ]

if_true_0:                                        ; preds = %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1, %when_true_1
  %7 = alloca { i64, i64 }, align 8
  %8 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %7, i64 0, i32 0
  store i64 6, i64* %8, align 8
  %9 = getelementptr inbounds { i64, i64 }, { i64, i64 }* %7, i64 0, i32 1
  %10 = bitcast i64* %9 to i8*
  store i8 %6, i8* %10, align 8
  %11 = add i64 %idx, 1
  %.unpack32 = load i64, i64* %9, align 8
  %.unpack40.unpack46 = insertvalue { i64, i64 } { i64 6, i64 undef }, i64 %.unpack32, 1
  %12 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack40.unpack46, 0
  %.unpack4043 = insertvalue { { i64, i64 }, i64 } %12, i64 %11, 1
  %13 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack4043, 1
  ret { i64, { { i64, i64 }, i64 } } %13

when_exit_1:                                      ; preds = %when_true_1, %when_exit_0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

define { i64, { { i64, i64 }, i64 } } @lexer.lexType({ i64, i64, i8* }* nocapture %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, { { i64, i64 }, i64 } } @lexer.lexIdent({ i64, i64, i8* }* %s, i64 %idx)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %1, 0
  %.elt23 = extractvalue { i64, { { i64, i64 }, i64 } } %1, 1
  %.elt23.elt = extractvalue { { i64, i64 }, i64 } %.elt23, 0
  %.elt23.elt.elt = extractvalue { i64, i64 } %.elt23.elt, 0
  %.elt23.elt.elt27 = extractvalue { i64, i64 } %.elt23.elt, 1
  %2 = icmp eq i64 %.elt, 1
  %3 = icmp eq i64 %.elt23.elt.elt, 1
  %or.cond = and i1 %2, %3
  br i1 %or.cond, label %when_exit_2, label %when_exit_0

when_exit_2:                                      ; preds = %0
  %.elt23.elt25 = extractvalue { { i64, i64 }, i64 } %.elt23, 1
  %4 = alloca { i64, i64, i8* }, align 8
  %.repack51 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %4, i64 0, i32 0
  %.repack53 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %4, i64 0, i32 2
  %5 = bitcast { i64, i64, i8* }* %4 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %5, i8 0, i64 24, i1 false)
  call void @lexer.read({ i64, i64, i8* }* %s, { i64, i64, i8* }* nonnull %4, i64 %.elt23.elt.elt27)
  %6 = tail call i8* @GC_malloc(i64 4)
  %7 = bitcast i8* %6 to i16*
  store i16 14441, i16* %7, align 1
  %8 = load i64, i64* %.repack51, align 8
  %9 = icmp eq i64 %8, 2
  br i1 %9, label %eqeq_table_exit_0, label %when_exit_4

eqeq_table_exit_0:                                ; preds = %when_exit_2
  %10 = load i8*, i8** %.repack53, align 8
  %11 = tail call i64 @memcmp(i8* %10, i8* %6, i64 2)
  %12 = icmp eq i64 %11, 0
  br i1 %12, label %if_true_0, label %when_exit_4

when_exit_4:                                      ; preds = %eqeq_table_exit_0, %when_exit_2
  %13 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %13, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_34, i64 0, i64 0), i64 3, i1 false)
  %14 = icmp eq i64 %8, 3
  br i1 %14, label %eqeq_table_exit_1, label %when_exit_14

eqeq_table_exit_1:                                ; preds = %when_exit_4
  %15 = load i8*, i8** %.repack53, align 8
  %16 = tail call i64 @memcmp(i8* %15, i8* %13, i64 3)
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %if_true_0, label %eqeq_table_exit_2

eqeq_table_exit_2:                                ; preds = %eqeq_table_exit_1
  %18 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %18, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_35, i64 0, i64 0), i64 3, i1 false)
  %19 = load i8*, i8** %.repack53, align 8
  %20 = tail call i64 @memcmp(i8* %19, i8* %18, i64 3)
  %21 = icmp eq i64 %20, 0
  br i1 %21, label %if_true_0, label %eqeq_table_exit_3

eqeq_table_exit_3:                                ; preds = %eqeq_table_exit_2
  %22 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %22, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_36, i64 0, i64 0), i64 3, i1 false)
  %23 = load i8*, i8** %.repack53, align 8
  %24 = tail call i64 @memcmp(i8* %23, i8* %22, i64 3)
  %25 = icmp eq i64 %24, 0
  br i1 %25, label %if_true_0, label %eqeq_table_exit_4

eqeq_table_exit_4:                                ; preds = %eqeq_table_exit_3
  %26 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %26, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_37, i64 0, i64 0), i64 3, i1 false)
  %27 = load i8*, i8** %.repack53, align 8
  %28 = tail call i64 @memcmp(i8* %27, i8* %26, i64 3)
  %29 = icmp eq i64 %28, 0
  br i1 %29, label %if_true_0, label %eqeq_table_exit_5

eqeq_table_exit_5:                                ; preds = %eqeq_table_exit_4
  %30 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %30, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_38, i64 0, i64 0), i64 3, i1 false)
  %31 = load i8*, i8** %.repack53, align 8
  %32 = tail call i64 @memcmp(i8* %31, i8* %30, i64 3)
  %33 = icmp eq i64 %32, 0
  br i1 %33, label %if_true_0, label %when_exit_16

when_exit_14:                                     ; preds = %when_exit_4
  %34 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %34, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_35, i64 0, i64 0), i64 3, i1 false)
  %35 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %35, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_36, i64 0, i64 0), i64 3, i1 false)
  %36 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %36, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_37, i64 0, i64 0), i64 3, i1 false)
  %37 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %37, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_38, i64 0, i64 0), i64 3, i1 false)
  %38 = tail call i8* @GC_malloc(i64 8)
  %39 = bitcast i8* %38 to i32*
  store i32 1819242338, i32* %39, align 1
  %40 = icmp eq i64 %8, 4
  br i1 %40, label %eqeq_table_exit_6, label %when_exit_18.sink.split

eqeq_table_exit_6:                                ; preds = %when_exit_14
  %41 = load i8*, i8** %.repack53, align 8
  %42 = tail call i64 @memcmp(i8* %41, i8* %38, i64 4)
  %43 = icmp eq i64 %42, 0
  br i1 %43, label %if_true_0, label %eqeq_table_exit_7

when_exit_16:                                     ; preds = %eqeq_table_exit_5
  %44 = tail call i8* @GC_malloc(i64 8)
  %45 = bitcast i8* %44 to i32*
  store i32 1819242338, i32* %45, align 1
  br label %when_exit_18.sink.split

eqeq_table_exit_7:                                ; preds = %eqeq_table_exit_6
  %46 = tail call i8* @GC_malloc(i64 8)
  %47 = bitcast i8* %46 to i32*
  store i32 1918986339, i32* %47, align 1
  %48 = load i8*, i8** %.repack53, align 8
  %49 = tail call i64 @memcmp(i8* %48, i8* %46, i64 4)
  %50 = icmp eq i64 %49, 0
  br i1 %50, label %if_true_0, label %when_exit_18

when_exit_18.sink.split:                          ; preds = %when_exit_14, %when_exit_16
  %51 = tail call i8* @GC_malloc(i64 8)
  %52 = bitcast i8* %51 to i32*
  store i32 1918986339, i32* %52, align 1
  br label %when_exit_18

when_exit_18:                                     ; preds = %when_exit_18.sink.split, %eqeq_table_exit_7
  %53 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %53, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_41, i64 0, i64 0), i64 6, i1 false)
  %54 = icmp eq i64 %8, 6
  br i1 %54, label %eqeq_table_exit_8, label %when_exit_20

eqeq_table_exit_8:                                ; preds = %when_exit_18
  %55 = load i8*, i8** %.repack53, align 8
  %56 = tail call i64 @memcmp(i8* %55, i8* %53, i64 6)
  %57 = icmp eq i64 %56, 0
  br i1 %57, label %if_true_0, label %eqeq_table_exit_9

when_exit_20:                                     ; preds = %when_exit_18
  %58 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %58, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_42, i64 0, i64 0), i64 6, i1 false)
  br label %when_exit_22

eqeq_table_exit_9:                                ; preds = %eqeq_table_exit_8
  %59 = tail call i8* @GC_malloc(i64 12)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %59, i8* align 1 getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_42, i64 0, i64 0), i64 6, i1 false)
  %60 = load i8*, i8** %.repack53, align 8
  %61 = tail call i64 @memcmp(i8* %60, i8* %59, i64 6)
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %if_true_0, label %when_exit_22

when_exit_22:                                     ; preds = %eqeq_table_exit_9, %when_exit_20
  %63 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %63, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @lexer.str_43, i64 0, i64 0), i64 3, i1 false)
  br i1 %14, label %eqeq_table_exit_10, label %if_false_0

eqeq_table_exit_10:                               ; preds = %when_exit_22
  %64 = load i8*, i8** %.repack53, align 8
  %65 = tail call i64 @memcmp(i8* %64, i8* %63, i64 3)
  %66 = icmp eq i64 %65, 0
  br i1 %66, label %if_true_0, label %if_false_0

if_true_0:                                        ; preds = %eqeq_table_exit_0, %eqeq_table_exit_1, %eqeq_table_exit_2, %eqeq_table_exit_3, %eqeq_table_exit_4, %eqeq_table_exit_5, %eqeq_table_exit_6, %eqeq_table_exit_7, %eqeq_table_exit_8, %eqeq_table_exit_9, %eqeq_table_exit_10
  %.unpack131.unpack137 = insertvalue { i64, i64 } { i64 5, i64 undef }, i64 %.elt23.elt.elt27, 1
  %67 = insertvalue { { i64, i64 }, i64 } undef, { i64, i64 } %.unpack131.unpack137, 0
  %.unpack131134 = insertvalue { { i64, i64 }, i64 } %67, i64 %.elt23.elt25, 1
  %68 = insertvalue { i64, { { i64, i64 }, i64 } } { i64 1, { { i64, i64 }, i64 } undef }, { { i64, i64 }, i64 } %.unpack131134, 1
  ret { i64, { { i64, i64 }, i64 } } %68

if_false_0:                                       ; preds = %when_exit_22, %eqeq_table_exit_10
  tail call void @lexer.popString({ i64, i64, i8* }* %s)
  br label %when_exit_0

when_exit_0:                                      ; preds = %if_false_0, %0
  ret { i64, { { i64, i64 }, i64 } } { i64 0, { { i64, i64 }, i64 } undef }
}

; Function Attrs: nofree norecurse nounwind
define void @lexer.popString({ i64, i64, i8* }* nocapture %s) local_unnamed_addr #1 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %.pre = load i64, i64* %1, align 4
  %2 = add i64 %.pre, -1
  %3 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %s, i64 %2)
  %4 = extractvalue { i64, i8 } %3, 0
  %5 = icmp eq i64 %4, 1
  br i1 %5, label %while_cond_0.backedge, label %while_exit_0

while_cond_0.backedge:                            ; preds = %0, %while_cond_0.backedge
  %6 = phi i64 [ %7, %while_cond_0.backedge ], [ %2, %0 ]
  store i64 %6, i64* %1, align 4
  %7 = add i64 %6, -1
  %8 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %s, i64 %7)
  %9 = extractvalue { i64, i8 } %8, 0
  %10 = icmp eq i64 %9, 1
  br i1 %10, label %while_cond_0.backedge, label %while_exit_0

while_exit_0:                                     ; preds = %while_cond_0.backedge, %0
  ret void
}

define void @lexer.read({ i64, i64, i8* }* nocapture readonly %s, { i64, i64, i8* }* nocapture %str, i64 %idx) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %str, i64 0, i32 0
  %2 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %str, i64 0, i32 1
  %3 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %str, i64 0, i32 2
  %4 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %5 = alloca { i64, i8 }, align 8
  store { i64, i8 } %4, { i64, i8 }* %5, align 8
  %6 = extractvalue { i64, i8 } %4, 0
  %7 = icmp eq i64 %6, 1
  br i1 %7, label %when_true_0, label %while_exit_0

when_true_0:                                      ; preds = %0, %when_exit_0
  %8 = phi { i64, i8 }* [ %23, %when_exit_0 ], [ %5, %0 ]
  %.0.ph18 = phi i64 [ %21, %when_exit_0 ], [ %idx, %0 ]
  %9 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %8, i64 0, i32 1
  %10 = load i8, i8* %9, align 8
  %11 = load i64, i64* %1, align 4
  %12 = add i64 %11, 1
  %13 = load i64, i64* %2, align 4
  %14 = icmp sgt i64 %12, %13
  br i1 %14, label %when_true_1, label %when_exit_0

when_true_1:                                      ; preds = %when_true_0
  %15 = shl i64 %12, 1
  %16 = tail call i8* @GC_malloc(i64 %15)
  %17 = load i8*, i8** %3, align 8
  %18 = load i64, i64* %1, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %16, i8* align 1 %17, i64 %18, i1 false)
  store i64 %15, i64* %1, align 8
  store i64 %15, i64* %2, align 8
  store i8* %16, i8** %3, align 8
  br label %when_exit_0

when_exit_0:                                      ; preds = %when_true_0, %when_true_1
  store i64 %12, i64* %1, align 4
  %19 = load i8*, i8** %3, align 8
  %20 = getelementptr i8, i8* %19, i64 %11
  store i8 %10, i8* %20, align 1
  %21 = add i64 %.0.ph18, 1
  %22 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %21)
  %23 = alloca { i64, i8 }, align 8
  store { i64, i8 } %22, { i64, i8 }* %23, align 8
  %24 = extractvalue { i64, i8 } %22, 0
  %25 = icmp eq i64 %24, 1
  br i1 %25, label %when_true_0, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_0, %0
  ret void
}

define void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nocapture readonly %s, i64 %idx) local_unnamed_addr {
  %1 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %idx)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = extractvalue { i64, i8 } %1, 0
  %4 = icmp eq i64 %3, 1
  br i1 %4, label %when_exit_0, label %while_exit_0

when_exit_0:                                      ; preds = %0, %when_exit_0
  %5 = phi { i64, i8 }* [ %10, %when_exit_0 ], [ %2, %0 ]
  %.0.ph7 = phi i64 [ %8, %when_exit_0 ], [ %idx, %0 ]
  %6 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %5, i64 0, i32 1
  %7 = load i8, i8* %6, align 8
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %7)
  %8 = add i64 %.0.ph7, 1
  %9 = tail call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %8)
  %10 = alloca { i64, i8 }, align 8
  store { i64, i8 } %9, { i64, i8 }* %10, align 8
  %11 = extractvalue { i64, i8 } %9, 0
  %12 = icmp eq i64 %11, 1
  br i1 %12, label %when_exit_0, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_0, %0
  ret void
}

; Function Attrs: nofree nounwind
declare i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #2

define void @lexer.writeLexeme({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nocapture readonly %s, { i64, i64 } %lexeme) local_unnamed_addr {
case_0:
  %lexeme.elt = extractvalue { i64, i64 } %lexeme, 0
  %lexeme.elt20 = extractvalue { i64, i64 } %lexeme, 1
  %.sroa.15.8.extract.trunc = trunc i64 %lexeme.elt20 to i8
  switch i64 %lexeme.elt, label %when_exit_41 [
    i64 0, label %when_exit_0.thread
    i64 13, label %switch_exit_0
    i64 6, label %when_exit_3.thread
    i64 2, label %when_exit_5.thread
    i64 3, label %when_exit_7.thread
    i64 7, label %when_exit_9.thread
    i64 11, label %when_exit_11.thread
    i64 12, label %when_exit_13.thread
    i64 1, label %when_exit_15.thread
    i64 4, label %when_exit_17.thread
    i64 5, label %when_exit_19.thread
    i64 10, label %when_exit_21.thread
    i64 9, label %when_exit_24
    i64 8, label %when_exit_36
  ]

when_exit_0.thread:                               ; preds = %case_0
  %0 = alloca { i64, i64, i8* }, align 8
  %.repack456 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %0, i64 0, i32 0
  %.repack457 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %0, i64 0, i32 1
  %.repack458 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %0, i64 0, i32 2
  %1 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack457, align 8
  store i8* %1, i8** %.repack458, align 8
  store i64 5, i64* %.repack456, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %1, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_44, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %0)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sroa.15.8.extract.trunc)
  br label %switch_exit_0

when_exit_3.thread:                               ; preds = %case_0
  %2 = alloca { i64, i64, i8* }, align 8
  %.repack435 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 0
  %.repack436 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 1
  %.repack437 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 2
  %3 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack436, align 8
  store i8* %3, i8** %.repack437, align 8
  store i64 5, i64* %.repack435, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %3, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_45, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %2)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sroa.15.8.extract.trunc)
  br label %switch_exit_0

when_exit_5.thread:                               ; preds = %case_0
  %4 = alloca { i64, i64, i8* }, align 8
  %.repack414 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %4, i64 0, i32 0
  %.repack415 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %4, i64 0, i32 1
  %.repack416 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %4, i64 0, i32 2
  %5 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack415, align 8
  store i8* %5, i8** %.repack416, align 8
  store i64 5, i64* %.repack414, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %5, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_46, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %4)
  call void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_7.thread:                               ; preds = %case_0
  %6 = alloca { i64, i64, i8* }, align 8
  %.repack393 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %6, i64 0, i32 0
  %.repack394 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %6, i64 0, i32 1
  %.repack395 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %6, i64 0, i32 2
  %7 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack394, align 8
  store i8* %7, i8** %.repack395, align 8
  store i64 5, i64* %.repack393, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %7, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_47, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %6)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_9.thread:                               ; preds = %case_0
  %.sroa.29.8.extract.shift = lshr i64 %lexeme.elt20, 8
  %.sroa.29.8.extract.trunc = trunc i64 %.sroa.29.8.extract.shift to i8
  %8 = alloca { i64, i64, i8* }, align 8
  %.repack372 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 0
  %.repack373 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 1
  %.repack374 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %8, i64 0, i32 2
  %9 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack373, align 8
  store i8* %9, i8** %.repack374, align 8
  store i64 5, i64* %.repack372, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %9, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_45, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %8)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sroa.15.8.extract.trunc)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sroa.29.8.extract.trunc)
  br label %switch_exit_0

when_exit_11.thread:                              ; preds = %case_0
  %10 = alloca { i64, i64, i8* }, align 8
  %.repack351 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %10, i64 0, i32 0
  %.repack352 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %10, i64 0, i32 1
  %.repack353 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %10, i64 0, i32 2
  %11 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack352, align 8
  store i8* %11, i8** %.repack353, align 8
  store i64 5, i64* %.repack351, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %11, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_48, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %10)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_13.thread:                              ; preds = %case_0
  %12 = alloca { i64, i64, i8* }, align 8
  %.repack330 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %12, i64 0, i32 0
  %.repack331 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %12, i64 0, i32 1
  %.repack332 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %12, i64 0, i32 2
  %13 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack331, align 8
  store i8* %13, i8** %.repack332, align 8
  store i64 5, i64* %.repack330, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %13, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_49, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %12)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_15.thread:                              ; preds = %case_0
  %14 = alloca { i64, i64, i8* }, align 8
  %.repack309 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 0
  %.repack310 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 1
  %.repack311 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %14, i64 0, i32 2
  %15 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack310, align 8
  store i8* %15, i8** %.repack311, align 8
  store i64 5, i64* %.repack309, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %15, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_50, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %14)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_17.thread:                              ; preds = %case_0
  %16 = alloca { i64, i64, i8* }, align 8
  %.repack288 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %16, i64 0, i32 0
  %.repack289 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %16, i64 0, i32 1
  %.repack290 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %16, i64 0, i32 2
  %17 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack289, align 8
  store i8* %17, i8** %.repack290, align 8
  store i64 5, i64* %.repack288, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %17, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_51, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %16)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_19.thread:                              ; preds = %case_0
  %18 = alloca { i64, i64, i8* }, align 8
  %.repack267 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %18, i64 0, i32 0
  %.repack268 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %18, i64 0, i32 1
  %.repack269 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %18, i64 0, i32 2
  %19 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack268, align 8
  store i8* %19, i8** %.repack269, align 8
  store i64 5, i64* %.repack267, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %19, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_52, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %18)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_21.thread:                              ; preds = %case_0
  %20 = alloca { i64, i64, i8* }, align 8
  %.repack246 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 0
  %.repack247 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 1
  %.repack248 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %20, i64 0, i32 2
  %21 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack247, align 8
  store i8* %21, i8** %.repack248, align 8
  store i64 5, i64* %.repack246, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %21, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_53, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %20)
  call void @lexer.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* %s, i64 %lexeme.elt20)
  br label %switch_exit_0

when_exit_24:                                     ; preds = %case_0
  %22 = alloca { i64, i64, i8* }, align 8
  %.repack120 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 0
  %.repack121 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 1
  %.repack122 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %22, i64 0, i32 2
  %23 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack121, align 8
  store i8* %23, i8** %.repack122, align 8
  store i64 5, i64* %.repack120, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %23, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_44, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %22)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 39)
  switch i8 %.sroa.15.8.extract.trunc, label %case_21 [
    i8 9, label %when_exit_25.thread
    i8 0, label %when_exit_27.thread
    i8 10, label %when_exit_29.thread
    i8 39, label %when_exit_31.thread
    i8 92, label %when_exit_33.thread
  ]

when_exit_25.thread:                              ; preds = %when_exit_24
  %24 = alloca { i64, i64, i8* }, align 8
  %.repack210 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 0
  %.repack211 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 1
  %.repack212 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %24, i64 0, i32 2
  %25 = call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack211, align 8
  store i8* %25, i8** %.repack212, align 8
  store i64 2, i64* %.repack210, align 8
  %26 = bitcast i8* %25 to i16*
  store i16 29788, i16* %26, align 1
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %24)
  br label %when_exit_23.thread

when_exit_27.thread:                              ; preds = %when_exit_24
  %27 = alloca { i64, i64, i8* }, align 8
  %.repack189 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %27, i64 0, i32 0
  %.repack190 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %27, i64 0, i32 1
  %.repack191 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %27, i64 0, i32 2
  %28 = call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack190, align 8
  store i8* %28, i8** %.repack191, align 8
  store i64 2, i64* %.repack189, align 8
  %29 = bitcast i8* %28 to i16*
  store i16 12380, i16* %29, align 1
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %27)
  br label %when_exit_23.thread

when_exit_29.thread:                              ; preds = %when_exit_24
  %30 = alloca { i64, i64, i8* }, align 8
  %.repack168 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %30, i64 0, i32 0
  %.repack169 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %30, i64 0, i32 1
  %.repack170 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %30, i64 0, i32 2
  %31 = call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack169, align 8
  store i8* %31, i8** %.repack170, align 8
  store i64 2, i64* %.repack168, align 8
  %32 = bitcast i8* %31 to i16*
  store i16 28252, i16* %32, align 1
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %30)
  br label %when_exit_23.thread

when_exit_31.thread:                              ; preds = %when_exit_24
  %33 = alloca { i64, i64, i8* }, align 8
  %.repack147 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %33, i64 0, i32 0
  %.repack148 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %33, i64 0, i32 1
  %.repack149 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %33, i64 0, i32 2
  %34 = call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack148, align 8
  store i8* %34, i8** %.repack149, align 8
  store i64 2, i64* %.repack147, align 8
  %35 = bitcast i8* %34 to i16*
  store i16 10076, i16* %35, align 1
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %33)
  br label %when_exit_23.thread

when_exit_33.thread:                              ; preds = %when_exit_24
  %36 = alloca { i64, i64, i8* }, align 8
  %.repack126 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %36, i64 0, i32 0
  %.repack127 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %36, i64 0, i32 1
  %.repack128 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %36, i64 0, i32 2
  %37 = call i8* @GC_malloc(i64 4)
  store i64 4, i64* %.repack127, align 8
  store i8* %37, i8** %.repack128, align 8
  store i64 2, i64* %.repack126, align 8
  %38 = bitcast i8* %37 to i16*
  store i16 23644, i16* %38, align 1
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %36)
  br label %when_exit_23.thread

case_21:                                          ; preds = %when_exit_24
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sroa.15.8.extract.trunc)
  br label %when_exit_23.thread

when_exit_23.thread:                              ; preds = %case_21, %when_exit_25.thread, %when_exit_27.thread, %when_exit_29.thread, %when_exit_31.thread, %when_exit_33.thread
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 39)
  br label %switch_exit_0

when_exit_36:                                     ; preds = %case_0
  %39 = alloca { i64, i64, i8* }, align 8
  %.repack99 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %39, i64 0, i32 0
  %.repack100 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %39, i64 0, i32 1
  %.repack101 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %39, i64 0, i32 2
  %40 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack100, align 8
  store i8* %40, i8** %.repack101, align 8
  store i64 5, i64* %.repack99, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %40, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @lexer.str_55, i64 0, i64 0), i64 5, i1 false)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %39)
  %41 = call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %lexeme.elt20)
  %42 = alloca { i64, i8 }, align 8
  store { i64, i8 } %41, { i64, i8 }* %42, align 8
  %43 = extractvalue { i64, i8 } %41, 0
  %44 = icmp eq i64 %43, 1
  br i1 %44, label %when_true_37, label %switch_exit_0

when_true_37:                                     ; preds = %when_exit_36, %while_cond_0.backedge
  %45 = phi { i64, i8 }* [ %51, %while_cond_0.backedge ], [ %42, %when_exit_36 ]
  %46 = phi i64 [ %49, %while_cond_0.backedge ], [ %lexeme.elt20, %when_exit_36 ]
  %47 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %45, i64 0, i32 1
  %48 = load i8, i8* %47, align 8
  switch i8 %48, label %case_stmt_26 [
    i8 10, label %while_cond_0.backedge
    i8 9, label %when_exit_39.thread
    i8 32, label %when_exit_40.thread
  ]

when_exit_39.thread:                              ; preds = %when_true_37
  br label %while_cond_0.backedge

when_exit_40.thread:                              ; preds = %when_true_37
  br label %while_cond_0.backedge

case_stmt_26:                                     ; preds = %when_true_37
  call void @llvm.trap()
  unreachable

while_cond_0.backedge:                            ; preds = %when_true_37, %when_exit_40.thread, %when_exit_39.thread
  %.sink = phi i8 [ 115, %when_exit_40.thread ], [ 116, %when_exit_39.thread ], [ 110, %when_true_37 ]
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 %.sink)
  %49 = add i64 %46, 1
  %50 = call { i64, i8 } @lexer.at({ i64, i64, i8* }* %s, i64 %49)
  %51 = alloca { i64, i8 }, align 8
  store { i64, i8 } %50, { i64, i8 }* %51, align 8
  %52 = extractvalue { i64, i8 } %50, 0
  %53 = icmp eq i64 %52, 1
  br i1 %53, label %when_true_37, label %switch_exit_0

when_exit_41:                                     ; preds = %case_0
  %54 = tail call i8* @GC_malloc(i64 32)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %54, i8* align 16 getelementptr inbounds ([17 x i8], [17 x i8]* @lexer.str_56, i64 0, i64 0), i64 16, i1 false)
  %55 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @lexer.str_57, i64 0, i64 0), i64 16, i8* %54)
  br label %switch_exit_0

switch_exit_0:                                    ; preds = %while_cond_0.backedge, %when_exit_36, %case_0, %when_exit_23.thread, %when_exit_21.thread, %when_exit_19.thread, %when_exit_17.thread, %when_exit_15.thread, %when_exit_13.thread, %when_exit_11.thread, %when_exit_9.thread, %when_exit_7.thread, %when_exit_5.thread, %when_exit_3.thread, %when_exit_0.thread, %when_exit_41
  ret void
}

define void @lexer.writeTextPos({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %idx, i64 %line, i64 %column) local_unnamed_addr {
  tail call void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %idx)
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 58)
  tail call void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %line)
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i8 58)
  tail call void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %column)
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #3

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #3

attributes #0 = { norecurse nounwind readonly }
attributes #1 = { nofree norecurse nounwind }
attributes #2 = { nofree nounwind }
attributes #3 = { argmemonly nounwind }
