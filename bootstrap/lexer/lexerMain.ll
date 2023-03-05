; ModuleID = '<string>'
source_filename = "<string>"

@lexerMain.str = unnamed_addr constant [8 x i8] c"\09ind: N\00"
@lexerMain.str_1 = unnamed_addr constant [8 x i8] c"\09ind: I\00"
@lexerMain.str_2 = unnamed_addr constant [15 x i8] c"\09INDENT ERRROR\00"
@lexerMain.str_3 = unnamed_addr constant [8 x i8] c"\09ind: D\00"
@lexerMain.str_4 = unnamed_addr constant [14 x i8] c"lex error at \00"
@lexerMain.str_5 = unnamed_addr constant [8 x i8] c"%-.*s, \00"
@lexerMain.str_6 = unnamed_addr constant [5 x i8] c"%ld\0A\00"

declare { i64, i8 } @io.read({ { i64, i64, i64* }, { i64, i64, i64* } }*) local_unnamed_addr

declare void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }*, i8) local_unnamed_addr

declare void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }*, { i64, i64, i8* }*) local_unnamed_addr

declare { i64, i8 } @lexer.at({ i64, i64, i8* }*, i64) local_unnamed_addr

declare { i64, { { i64, i64 }, i64 } } @lexer.lex({ i64, i64, i8* }*, i64) local_unnamed_addr

declare void @lexer.read({ i64, i64, i8* }*, { i64, i64, i8* }*, i64) local_unnamed_addr

declare void @lexer.writeLexeme({ { i64, i64, i64* }, { i64, i64, i64* } }*, { i64, i64, i8* }*, { i64, i64 }) local_unnamed_addr

declare void @lexer.writeTextPos({ { i64, i64, i64* }, { i64, i64, i64* } }*, i64, i64, i64) local_unnamed_addr

declare i1 @strings.isPrefix({ i64, i64, i8* }*, { i64, i64, i8* }*) local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

declare void @llvm.trap()

declare i64 @memcmp(i8*, i8*, i64) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #0

define void @main() local_unnamed_addr {
  %1 = alloca { { i64, i64, i64* }, { i64, i64, i64* } }, align 8
  %2 = alloca { i64, i64, i8* }, align 8
  %.repack = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 0
  %3 = bitcast { { i64, i64, i64* }, { i64, i64, i64* } }* %1 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %3, i8 0, i64 48, i1 false)
  %.repack18 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 1
  %.repack19 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %2, i64 0, i32 2
  %4 = bitcast { i64, i64, i8* }* %2 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %4, i8 0, i64 24, i1 false)
  %5 = call { i64, i8 } @io.read({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1)
  %6 = alloca { i64, i8 }, align 8
  store { i64, i8 } %5, { i64, i8 }* %6, align 8
  %7 = extractvalue { i64, i8 } %5, 0
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %when_true_0, label %while_exit_0

when_true_0:                                      ; preds = %0, %while_cond_0.backedge
  %9 = phi { i64, i8 }* [ %23, %while_cond_0.backedge ], [ %6, %0 ]
  %10 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %9, i64 0, i32 1
  %11 = load i8, i8* %10, align 8
  %12 = load i64, i64* %.repack, align 8
  %13 = add i64 %12, 1
  %14 = load i64, i64* %.repack18, align 8
  %15 = icmp sgt i64 %13, %14
  br i1 %15, label %when_true_1, label %when_true_0.when_exit_0_crit_edge

when_true_0.when_exit_0_crit_edge:                ; preds = %when_true_0
  %.pre321 = load i8*, i8** %.repack19, align 8
  br label %while_cond_0.backedge

when_true_1:                                      ; preds = %when_true_0
  %16 = shl i64 %13, 1
  %17 = call i8* @GC_malloc(i64 %16)
  %18 = load i8*, i8** %.repack19, align 8
  %19 = load i64, i64* %.repack, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %17, i8* align 1 %18, i64 %19, i1 false)
  store i64 %16, i64* %.repack, align 8
  store i64 %16, i64* %.repack18, align 8
  store i8* %17, i8** %.repack19, align 8
  br label %while_cond_0.backedge

while_cond_0.backedge:                            ; preds = %when_true_1, %when_true_0.when_exit_0_crit_edge
  %20 = phi i8* [ %.pre321, %when_true_0.when_exit_0_crit_edge ], [ %17, %when_true_1 ]
  store i64 %13, i64* %.repack, align 8
  %21 = getelementptr i8, i8* %20, i64 %12
  store i8 %11, i8* %21, align 1
  %22 = call { i64, i8 } @io.read({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1)
  %23 = alloca { i64, i8 }, align 8
  store { i64, i8 } %22, { i64, i8 }* %23, align 8
  %24 = extractvalue { i64, i8 } %22, 0
  %25 = icmp eq i64 %24, 0
  br i1 %25, label %when_true_0, label %while_exit_0

while_exit_0:                                     ; preds = %while_cond_0.backedge, %0
  %26 = call i8* @GC_malloc(i64 48)
  call void @llvm.memset.p0i8.i64(i8* align 8 %26, i8 0, i64 24, i1 false)
  %27 = bitcast i8* %26 to { i64, i64, i8* }*
  %28 = call { i64, { { i64, i64 }, i64 } } @lexer.lex({ i64, i64, i8* }* nonnull %2, i64 0)
  %.elt336 = extractvalue { i64, { { i64, i64 }, i64 } } %28, 0
  %.elt28337 = extractvalue { i64, { { i64, i64 }, i64 } } %28, 1
  %.elt28.elt338 = extractvalue { { i64, i64 }, i64 } %.elt28337, 0
  %29 = icmp eq i64 %.elt336, 1
  br i1 %29, label %when_true_3.lr.ph, label %while_exit_1

when_true_3.lr.ph:                                ; preds = %while_exit_0
  %.elt28.elt30341 = extractvalue { { i64, i64 }, i64 } %.elt28337, 1
  %.elt28.elt.elt32340 = extractvalue { i64, i64 } %.elt28.elt338, 1
  %.elt28.elt.elt339 = extractvalue { i64, i64 } %.elt28.elt338, 0
  br label %when_true_3

when_true_3:                                      ; preds = %when_true_3.lr.ph, %while_cond_1.backedge
  %.elt28.elt30348 = phi i64 [ %.elt28.elt30341, %when_true_3.lr.ph ], [ %.elt28.elt30, %while_cond_1.backedge ]
  %.elt28.elt.elt32347 = phi i64 [ %.elt28.elt.elt32340, %when_true_3.lr.ph ], [ %.elt28.elt.elt32, %while_cond_1.backedge ]
  %.elt28.elt.elt346 = phi i64 [ %.elt28.elt.elt339, %when_true_3.lr.ph ], [ %.elt28.elt.elt, %while_cond_1.backedge ]
  %.elt28.elt345 = phi { i64, i64 } [ %.elt28.elt338, %when_true_3.lr.ph ], [ %.elt28.elt, %while_cond_1.backedge ]
  %30 = phi i64 [ 0, %when_true_3.lr.ph ], [ %.lcssa277285, %while_cond_1.backedge ]
  %.lcssa280290344 = phi i64 [ 1, %when_true_3.lr.ph ], [ %.lcssa280289, %while_cond_1.backedge ]
  %.lcssa281297342 = phi i64 [ 1, %when_true_3.lr.ph ], [ %.lcssa281296, %while_cond_1.backedge ]
  %31 = phi i64 [ 0, %when_true_3.lr.ph ], [ %.elt28.elt30348, %while_cond_1.backedge ]
  %32 = phi i64 [ 2, %when_true_3.lr.ph ], [ %135, %while_cond_1.backedge ]
  %33 = phi { i64, i64, i8* }* [ %27, %when_true_3.lr.ph ], [ %134, %while_cond_1.backedge ]
  %34 = phi { i64, i64, i8* }* [ %27, %when_true_3.lr.ph ], [ %133, %while_cond_1.backedge ]
  %35 = phi { i64, i64, i8* }* [ %27, %when_true_3.lr.ph ], [ %132, %while_cond_1.backedge ]
  %36 = phi i8* [ %26, %when_true_3.lr.ph ], [ %131, %while_cond_1.backedge ]
  %37 = phi { i64, i64, i8* }* [ %27, %when_true_3.lr.ph ], [ %130, %while_cond_1.backedge ]
  %38 = phi { i64, i64, i8* }* [ %27, %when_true_3.lr.ph ], [ %129, %while_cond_1.backedge ]
  %39 = phi i8* [ %26, %when_true_3.lr.ph ], [ %128, %while_cond_1.backedge ]
  %40 = icmp slt i64 %31, %.elt28.elt30348
  br i1 %40, label %for_body_0, label %for_exit_0

for_body_0:                                       ; preds = %when_true_3, %when_exit_4
  %41 = phi i64 [ %spec.select, %when_exit_4 ], [ %.lcssa280290344, %when_true_3 ]
  %42 = phi i64 [ %spec.select307, %when_exit_4 ], [ %30, %when_true_3 ]
  %storemerge276 = phi i64 [ %52, %when_exit_4 ], [ %31, %when_true_3 ]
  %43 = call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %2, i64 %storemerge276)
  %44 = alloca { i64, i8 }, align 8
  store { i64, i8 } %43, { i64, i8 }* %44, align 8
  %45 = extractvalue { i64, i8 } %43, 0
  %46 = icmp eq i64 %45, 1
  br i1 %46, label %when_exit_4, label %when_true_4

when_true_4:                                      ; preds = %for_body_0
  call void @llvm.trap()
  unreachable

when_exit_4:                                      ; preds = %for_body_0
  %47 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %44, i64 0, i32 1
  %48 = load i8, i8* %47, align 8
  %49 = add i64 %42, 1
  %50 = icmp eq i8 %48, 10
  %51 = zext i1 %50 to i64
  %spec.select = add i64 %41, %51
  %spec.select307 = select i1 %50, i64 0, i64 %49
  %52 = add nsw i64 %storemerge276, 1
  %53 = icmp slt i64 %52, %.elt28.elt30348
  br i1 %53, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_exit_4, %when_true_3
  %.lcssa280289 = phi i64 [ %.lcssa280290344, %when_true_3 ], [ %spec.select, %when_exit_4 ]
  %.lcssa277285 = phi i64 [ %30, %when_true_3 ], [ %spec.select307, %when_exit_4 ]
  switch i64 %.elt28.elt.elt346, label %when_true_16 [
    i64 13, label %while_cond_1.backedge
    i64 8, label %when_true_7
  ]

when_true_7:                                      ; preds = %for_exit_0
  %54 = alloca { i64, i64, i8* }, align 8
  %.repack85 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %54, i64 0, i32 0
  %.repack87 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %54, i64 0, i32 2
  %55 = bitcast { i64, i64, i8* }* %54 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %55, i8 0, i64 24, i1 false)
  call void @lexer.read({ i64, i64, i8* }* nonnull %2, { i64, i64, i8* }* nonnull %54, i64 %.elt28.elt.elt32347)
  call void @lexer.writeTextPos({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, i64 %31, i64 %.lcssa280289, i64 %.lcssa277285)
  %56 = add i64 %.lcssa281297342, -1
  %57 = load i64, i64* %.repack85, align 8
  %58 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %33, i64 %56, i32 0
  %59 = load i64, i64* %58, align 4
  %60 = icmp eq i64 %57, %59
  br i1 %60, label %eqeq_table_exit_0, label %if_false_1

eqeq_table_exit_0:                                ; preds = %when_true_7
  %61 = load i8*, i8** %.repack87, align 8
  %62 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %33, i64 %56, i32 2
  %63 = load i8*, i8** %62, align 8
  %64 = call i64 @memcmp(i8* %61, i8* %63, i64 %57)
  %65 = icmp eq i64 %64, 0
  br i1 %65, label %when_exit_8, label %if_false_1

when_exit_8:                                      ; preds = %eqeq_table_exit_0
  %66 = alloca { i64, i64, i8* }, align 8
  %.repack226 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %66, i64 0, i32 0
  %.repack227 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %66, i64 0, i32 1
  %.repack228 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %66, i64 0, i32 2
  %67 = bitcast { i64, i64, i8* }* %66 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %67, i8 0, i64 24, i1 false)
  %68 = call i8* @GC_malloc(i64 14)
  %69 = load i8*, i8** %.repack228, align 8
  %70 = load i64, i64* %.repack226, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %68, i8* align 1 %69, i64 %70, i1 false)
  store i64 14, i64* %.repack227, align 8
  store i8* %68, i8** %.repack228, align 8
  store i64 7, i64* %.repack226, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %68, i8* align 1 getelementptr inbounds ([8 x i8], [8 x i8]* @lexerMain.str, i64 0, i64 0), i64 7, i1 false)
  call void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, { i64, i64, i8* }* nonnull %66)
  br label %while_cond_1.backedge

if_false_1:                                       ; preds = %when_true_7, %eqeq_table_exit_0
  %71 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %34, i64 %56
  %72 = call i1 @strings.isPrefix({ i64, i64, i8* }* nonnull %54, { i64, i64, i8* }* %71)
  %73 = alloca { i64, i64, i8* }, align 8
  %.repack172 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %73, i64 0, i32 0
  %.repack173 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %73, i64 0, i32 1
  %.repack174 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %73, i64 0, i32 2
  %74 = bitcast { i64, i64, i8* }* %73 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %74, i8 0, i64 24, i1 false)
  %75 = call i8* @GC_malloc(i64 14)
  %76 = load i8*, i8** %.repack174, align 8
  %77 = load i64, i64* %.repack172, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %75, i8* align 1 %76, i64 %77, i1 false)
  store i64 14, i64* %.repack173, align 8
  store i8* %75, i8** %.repack174, align 8
  store i64 7, i64* %.repack172, align 8
  br i1 %72, label %when_exit_9, label %when_exit_12

when_exit_9:                                      ; preds = %if_false_1
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %75, i8* align 1 getelementptr inbounds ([8 x i8], [8 x i8]* @lexerMain.str_1, i64 0, i64 0), i64 7, i1 false)
  call void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, { i64, i64, i8* }* nonnull %73)
  %78 = add i64 %.lcssa281297342, 1
  %79 = icmp sgt i64 %78, %32
  br i1 %79, label %when_true_10, label %when_exit_10

when_true_10:                                     ; preds = %when_exit_9
  %80 = shl i64 %78, 1
  %81 = mul i64 %78, 48
  %82 = call i8* @GC_malloc(i64 %81)
  %83 = mul i64 %.lcssa281297342, 24
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %82, i8* align 1 %36, i64 %83, i1 false)
  %84 = bitcast i8* %82 to { i64, i64, i8* }*
  br label %when_exit_10

when_exit_10:                                     ; preds = %when_true_10, %when_exit_9
  %85 = phi i8* [ %82, %when_true_10 ], [ %39, %when_exit_9 ]
  %86 = phi { i64, i64, i8* }* [ %84, %when_true_10 ], [ %38, %when_exit_9 ]
  %87 = phi { i64, i64, i8* }* [ %84, %when_true_10 ], [ %37, %when_exit_9 ]
  %88 = phi i64 [ %80, %when_true_10 ], [ %32, %when_exit_9 ]
  %.repack175 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %87, i64 %.lcssa281297342, i32 0
  %89 = bitcast i64* %.repack175 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 8 %89, i8 0, i64 24, i1 false)
  %90 = load i64, i64* %.repack85, align 8
  %91 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %86, i64 %.lcssa281297342, i32 1
  %92 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %86, i64 %.lcssa281297342, i32 0
  %93 = load i64, i64* %91, align 4
  %94 = icmp sgt i64 %90, %93
  br i1 %94, label %when_true_11, label %when_exit_10.when_exit_11_crit_edge

when_exit_10.when_exit_11_crit_edge:              ; preds = %when_exit_10
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %86, i64 %.lcssa281297342, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_11

when_true_11:                                     ; preds = %when_exit_10
  %95 = shl i64 %90, 1
  %96 = call i8* @GC_malloc(i64 %95)
  %97 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %86, i64 %.lcssa281297342, i32 2
  %98 = load i8*, i8** %97, align 8
  %99 = load i64, i64* %92, align 4
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %96, i8* align 1 %98, i64 %99, i1 false)
  store i64 %95, i64* %92, align 8
  store i64 %95, i64* %91, align 8
  store i8* %96, i8** %97, align 8
  br label %when_exit_11

when_exit_11:                                     ; preds = %when_exit_10.when_exit_11_crit_edge, %when_true_11
  %100 = phi i8* [ %.pre, %when_exit_10.when_exit_11_crit_edge ], [ %96, %when_true_11 ]
  store i64 %90, i64* %92, align 4
  %101 = load i8*, i8** %.repack87, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %100, i8* align 1 %101, i64 %90, i1 false)
  br label %while_cond_1.backedge

when_exit_12:                                     ; preds = %if_false_1
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %75, i8* align 1 getelementptr inbounds ([8 x i8], [8 x i8]* @lexerMain.str, i64 0, i64 0), i64 7, i1 false)
  br label %while_cond_2

while_cond_2:                                     ; preds = %when_exit_15, %when_exit_12
  %.sink = phi { i64, i64, i8* }* [ %123, %when_exit_15 ], [ %73, %when_exit_12 ]
  %102 = phi i64 [ %103, %when_exit_15 ], [ %.lcssa281297342, %when_exit_12 ]
  call void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, { i64, i64, i8* }* nonnull %.sink)
  %103 = add i64 %102, -1
  %104 = load i64, i64* %.repack85, align 8
  %105 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %35, i64 %103, i32 0
  %106 = load i64, i64* %105, align 4
  %107 = icmp eq i64 %104, %106
  br i1 %107, label %eqeq_table_exit_2, label %while_body_0

eqeq_table_exit_2:                                ; preds = %while_cond_2
  %108 = load i8*, i8** %.repack87, align 8
  %109 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %35, i64 %103, i32 2
  %110 = load i8*, i8** %109, align 8
  %111 = call i64 @memcmp(i8* %108, i8* %110, i64 %104)
  %112 = icmp eq i64 %111, 0
  br i1 %112, label %while_cond_1.backedge, label %while_body_0

while_body_0:                                     ; preds = %while_cond_2, %eqeq_table_exit_2
  %113 = icmp eq i64 %102, 1
  br i1 %113, label %when_exit_13, label %if_exit_3

when_exit_13:                                     ; preds = %while_body_0
  %114 = alloca { i64, i64, i8* }, align 8
  %.repack136 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %114, i64 0, i32 0
  %.repack137 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %114, i64 0, i32 1
  %.repack138 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %114, i64 0, i32 2
  %115 = call i8* @GC_malloc(i64 28)
  store i64 28, i64* %.repack137, align 8
  store i8* %115, i8** %.repack138, align 8
  store i64 14, i64* %.repack136, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %115, i8* align 1 getelementptr inbounds ([15 x i8], [15 x i8]* @lexerMain.str_2, i64 0, i64 0), i64 14, i1 false)
  call void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, { i64, i64, i8* }* nonnull %114)
  ret void

if_exit_3:                                        ; preds = %while_body_0
  %116 = load i64, i64* %105, align 4
  %117 = icmp sgt i64 %116, 0
  br i1 %117, label %when_true_14, label %when_exit_15

when_true_14:                                     ; preds = %if_exit_3
  %118 = shl i64 %116, 1
  %119 = call i8* @GC_malloc(i64 %118)
  br label %when_exit_15

when_exit_15:                                     ; preds = %when_true_14, %if_exit_3
  %120 = phi i8* [ %119, %when_true_14 ], [ null, %if_exit_3 ]
  %121 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %35, i64 %103, i32 2
  %122 = load i8*, i8** %121, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %120, i8* align 1 %122, i64 %116, i1 false)
  call void @lexer.writeTextPos({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, i64 %31, i64 %.lcssa280289, i64 %.lcssa277285)
  %123 = alloca { i64, i64, i8* }, align 8
  %.repack100 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %123, i64 0, i32 0
  %.repack101 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %123, i64 0, i32 1
  %.repack102 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %123, i64 0, i32 2
  %124 = bitcast { i64, i64, i8* }* %123 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %124, i8 0, i64 24, i1 false)
  %125 = call i8* @GC_malloc(i64 14)
  %126 = load i8*, i8** %.repack102, align 8
  %127 = load i64, i64* %.repack100, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %125, i8* align 1 %126, i64 %127, i1 false)
  store i64 14, i64* %.repack101, align 8
  store i8* %125, i8** %.repack102, align 8
  store i64 7, i64* %.repack100, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %125, i8* align 1 getelementptr inbounds ([8 x i8], [8 x i8]* @lexerMain.str_3, i64 0, i64 0), i64 7, i1 false)
  br label %while_cond_2

when_true_16:                                     ; preds = %for_exit_0
  call void @lexer.writeTextPos({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, i64 %31, i64 %.lcssa280289, i64 %.lcssa277285)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, i8 9)
  call void @lexer.writeLexeme({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, { i64, i64, i8* }* nonnull %2, { i64, i64 } %.elt28.elt345)
  call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* nonnull %1, i8 10)
  br label %while_cond_1.backedge

while_cond_1.backedge:                            ; preds = %eqeq_table_exit_2, %for_exit_0, %when_true_16, %when_exit_8, %when_exit_11
  %128 = phi i8* [ %39, %for_exit_0 ], [ %39, %when_true_16 ], [ %39, %when_exit_8 ], [ %85, %when_exit_11 ], [ %39, %eqeq_table_exit_2 ]
  %129 = phi { i64, i64, i8* }* [ %38, %for_exit_0 ], [ %38, %when_true_16 ], [ %38, %when_exit_8 ], [ %86, %when_exit_11 ], [ %38, %eqeq_table_exit_2 ]
  %130 = phi { i64, i64, i8* }* [ %37, %for_exit_0 ], [ %37, %when_true_16 ], [ %37, %when_exit_8 ], [ %86, %when_exit_11 ], [ %37, %eqeq_table_exit_2 ]
  %131 = phi i8* [ %36, %for_exit_0 ], [ %36, %when_true_16 ], [ %36, %when_exit_8 ], [ %85, %when_exit_11 ], [ %36, %eqeq_table_exit_2 ]
  %132 = phi { i64, i64, i8* }* [ %35, %for_exit_0 ], [ %35, %when_true_16 ], [ %35, %when_exit_8 ], [ %86, %when_exit_11 ], [ %35, %eqeq_table_exit_2 ]
  %133 = phi { i64, i64, i8* }* [ %34, %for_exit_0 ], [ %34, %when_true_16 ], [ %34, %when_exit_8 ], [ %86, %when_exit_11 ], [ %35, %eqeq_table_exit_2 ]
  %134 = phi { i64, i64, i8* }* [ %33, %for_exit_0 ], [ %33, %when_true_16 ], [ %33, %when_exit_8 ], [ %86, %when_exit_11 ], [ %35, %eqeq_table_exit_2 ]
  %135 = phi i64 [ %32, %for_exit_0 ], [ %32, %when_true_16 ], [ %32, %when_exit_8 ], [ %88, %when_exit_11 ], [ %32, %eqeq_table_exit_2 ]
  %.lcssa281296 = phi i64 [ %.lcssa281297342, %for_exit_0 ], [ %.lcssa281297342, %when_true_16 ], [ %.lcssa281297342, %when_exit_8 ], [ %78, %when_exit_11 ], [ %102, %eqeq_table_exit_2 ]
  %136 = call { i64, { { i64, i64 }, i64 } } @lexer.lex({ i64, i64, i8* }* nonnull %2, i64 %.elt28.elt30348)
  %.elt = extractvalue { i64, { { i64, i64 }, i64 } } %136, 0
  %.elt28 = extractvalue { i64, { { i64, i64 }, i64 } } %136, 1
  %.elt28.elt = extractvalue { { i64, i64 }, i64 } %.elt28, 0
  %.elt28.elt.elt = extractvalue { i64, i64 } %.elt28.elt, 0
  %.elt28.elt.elt32 = extractvalue { i64, i64 } %.elt28.elt, 1
  %.elt28.elt30 = extractvalue { { i64, i64 }, i64 } %.elt28, 1
  %137 = icmp eq i64 %.elt, 1
  br i1 %137, label %when_true_3, label %while_exit_1

while_exit_1:                                     ; preds = %while_cond_1.backedge, %while_exit_0
  %.lcssa333 = phi i64 [ 0, %while_exit_0 ], [ %.elt28.elt30348, %while_cond_1.backedge ]
  %.lcssa330 = phi i64 [ 0, %while_exit_0 ], [ %.lcssa277285, %while_cond_1.backedge ]
  %138 = call { i64, i8 } @lexer.at({ i64, i64, i8* }* nonnull %2, i64 %.lcssa333)
  %139 = extractvalue { i64, i8 } %138, 0
  %140 = icmp eq i64 %139, 1
  br i1 %140, label %when_exit_18, label %when_exit_17

when_exit_18:                                     ; preds = %while_exit_1
  %141 = call i8* @GC_malloc(i64 26)
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %141, i8* align 1 getelementptr inbounds ([14 x i8], [14 x i8]* @lexerMain.str_4, i64 0, i64 0), i64 13, i1 false)
  %142 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @lexerMain.str_5, i64 0, i64 0), i64 13, i8* %141)
  %143 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @lexerMain.str_6, i64 0, i64 0), i64 %.lcssa330)
  br label %when_exit_17

when_exit_17:                                     ; preds = %when_exit_18, %while_exit_1
  ret void
}

define void @lexerMain..callMain() local_unnamed_addr {
  tail call void @main()
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

attributes #0 = { nofree nounwind }
attributes #1 = { argmemonly nounwind }
