; ModuleID = '<string>'
source_filename = "<string>"

@io.str = unnamed_addr constant [3 x i8] c"w\00\00"
@io.str_1 = unnamed_addr constant [4 x i8] c"a+\00\00"

; Function Attrs: nofree nounwind
declare i32 @fclose(i8* nocapture) local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare i32 @fgetc(i8* nocapture) local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare noalias i8* @fopen(i8* nocapture readonly, i8* nocapture readonly) local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare i32 @fputc(i32, i8* nocapture) local_unnamed_addr #0

declare i8* @freopen(i8*, i8*, i8*) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i32 @getchar() local_unnamed_addr #0

; Function Attrs: nofree nounwind
declare i32 @putchar(i32) local_unnamed_addr #0

declare void @strings.write({ i64, i64, i8* }*, i8) local_unnamed_addr

declare void @strings.write_3({ i64, i64, i8* }*, i64) local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

define void @io.catFile({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, i64 %key) local_unnamed_addr {
  %1 = alloca { i64, i64, i8* }, align 8
  %.repack = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 0
  %.repack1 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 1
  %.repack2 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 2
  %2 = bitcast { i64, i64, i8* }* %1 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %2, i8 0, i64 24, i1 false)
  %3 = call i1 @io.readLn({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %1, i64 %key)
  br i1 %3, label %while_body_0.preheader, label %while_exit_0

while_body_0.preheader:                           ; preds = %0
  %4 = bitcast { i64, i64, i8* }* %1 to i8*
  br label %while_body_0

while_body_0:                                     ; preds = %while_body_0.preheader, %when_exit_0
  call void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* undef, { i64, i64, i8* }* nonnull %1)
  %5 = load i64, i64* %.repack1, align 8
  %6 = icmp slt i64 %5, 0
  br i1 %6, label %when_true_0, label %when_exit_0

when_true_0:                                      ; preds = %while_body_0
  %7 = call i8* @GC_malloc(i64 0)
  %8 = load i8*, i8** %.repack2, align 8
  %9 = load i64, i64* %.repack, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %7, i8* align 1 %8, i64 %9, i1 false)
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %4, i8 0, i64 16, i1 false)
  store i8* %7, i8** %.repack2, align 8
  br label %when_exit_0

when_exit_0:                                      ; preds = %when_true_0, %while_body_0
  store i64 0, i64* %.repack, align 8
  %10 = call i1 @io.readLn({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, { i64, i64, i8* }* nonnull %1, i64 %key)
  br i1 %10, label %while_body_0, label %while_exit_0

while_exit_0:                                     ; preds = %when_exit_0, %0
  ret void
}

declare void @llvm.trap()

define void @io.clearFile({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, i64 %key) local_unnamed_addr {
  %1 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %2 = load i64*, i64** %1, align 8
  %3 = getelementptr i64, i64* %2, i64 %key
  %4 = load i64, i64* %3, align 4
  %5 = icmp slt i64 %4, 1
  br i1 %5, label %when_true_0, label %when_exit_2

when_true_0:                                      ; preds = %0
  tail call void @llvm.trap()
  unreachable

when_exit_2:                                      ; preds = %0
  %6 = tail call i8* @GC_malloc(i64 4)
  %7 = bitcast i8* %6 to i16*
  store i16 119, i16* %7, align 1
  %8 = inttoptr i64 %4 to i8*
  %9 = tail call i8* @freopen(i8* null, i8* %6, i8* %8)
  %10 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %10, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @io.str_1, i64 0, i64 0), i64 3, i1 false)
  %11 = tail call i8* @freopen(i8* null, i8* %10, i8* %8)
  ret void
}

define void @io.closeFile({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture %io, i64 %key) local_unnamed_addr {
  %1 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %2 = load i64*, i64** %1, align 8
  %3 = getelementptr i64, i64* %2, i64 %key
  %4 = load i64, i64* %3, align 4
  %5 = icmp slt i64 %4, 1
  br i1 %5, label %when_true_0, label %exit_0

when_true_0:                                      ; preds = %0
  tail call void @llvm.trap()
  unreachable

exit_0:                                           ; preds = %0
  %6 = inttoptr i64 %4 to i8*
  %7 = tail call i32 @fclose(i8* %6)
  %8 = load i64*, i64** %1, align 8
  %9 = getelementptr i64, i64* %8, i64 %key
  store i64 0, i64* %9, align 4
  %10 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 0
  %11 = load i64, i64* %10, align 4
  %12 = add i64 %11, -1
  %13 = icmp eq i64 %12, %key
  br i1 %13, label %if_exit_0, label %if_false_0

if_false_0:                                       ; preds = %exit_0
  %14 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 0
  %15 = load i64, i64* %14, align 4
  %16 = add i64 %15, 1
  %17 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 1
  %18 = load i64, i64* %17, align 4
  %19 = icmp sgt i64 %16, %18
  br i1 %19, label %when_true_1, label %if_false_0.when_exit_1_crit_edge

if_false_0.when_exit_1_crit_edge:                 ; preds = %if_false_0
  %.phi.trans.insert = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 2
  %.pre = load i64*, i64** %.phi.trans.insert, align 8
  br label %when_exit_1

when_true_1:                                      ; preds = %if_false_0
  %20 = shl i64 %16, 1
  %21 = shl i64 %16, 4
  %22 = tail call i8* @GC_malloc(i64 %21)
  %23 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 2
  %24 = bitcast i64** %23 to i8**
  %25 = load i8*, i8** %24, align 8
  %26 = load i64, i64* %14, align 4
  %27 = shl i64 %26, 3
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %22, i8* align 1 %25, i64 %27, i1 false)
  %28 = bitcast i8* %22 to i64*
  store i64 %20, i64* %14, align 8
  store i64 %20, i64* %17, align 8
  %29 = bitcast i64** %23 to i8**
  store i8* %22, i8** %29, align 8
  br label %when_exit_1

when_exit_1:                                      ; preds = %if_false_0.when_exit_1_crit_edge, %when_true_1
  %30 = phi i64* [ %.pre, %if_false_0.when_exit_1_crit_edge ], [ %28, %when_true_1 ]
  store i64 %16, i64* %14, align 4
  %31 = getelementptr i64, i64* %30, i64 %15
  br label %if_exit_0

if_exit_0:                                        ; preds = %exit_0, %when_exit_1
  %.sink = phi i64* [ %31, %when_exit_1 ], [ %10, %exit_0 ]
  store i64 %key, i64* %.sink, align 4
  ret void
}

define i64 @io.openFile({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture %io, { i64, i64, i8* }* nocapture %fileName) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %fileName, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = add i64 %2, 1
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %fileName, i64 0, i32 1
  %5 = load i64, i64* %4, align 4
  %6 = icmp sgt i64 %3, %5
  br i1 %6, label %when_true_0, label %.when_exit_1_crit_edge

.when_exit_1_crit_edge:                           ; preds = %0
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %fileName, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_1

when_true_0:                                      ; preds = %0
  %7 = shl i64 %3, 1
  %8 = tail call i8* @GC_malloc(i64 %7)
  %9 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %fileName, i64 0, i32 2
  %10 = load i8*, i8** %9, align 8
  %11 = load i64, i64* %1, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %8, i8* align 1 %10, i64 %11, i1 false)
  store i64 %7, i64* %1, align 8
  store i64 %7, i64* %4, align 8
  store i8* %8, i8** %9, align 8
  br label %when_exit_1

when_exit_1:                                      ; preds = %.when_exit_1_crit_edge, %when_true_0
  %12 = phi i8* [ %.pre, %.when_exit_1_crit_edge ], [ %8, %when_true_0 ]
  store i64 %3, i64* %1, align 4
  %13 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %fileName, i64 0, i32 2
  %14 = getelementptr i8, i8* %12, i64 %2
  store i8 0, i8* %14, align 1
  %15 = load i8*, i8** %13, align 8
  %16 = tail call i8* @GC_malloc(i64 6)
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %16, i8* align 1 getelementptr inbounds ([4 x i8], [4 x i8]* @io.str_1, i64 0, i64 0), i64 3, i1 false)
  %17 = tail call i8* @fopen(i8* %15, i8* %16)
  %.cast = ptrtoint i8* %17 to i64
  %18 = icmp sgt i8* %17, null
  br i1 %18, label %exit_0, label %when_true_2

when_true_2:                                      ; preds = %when_exit_1
  tail call void @llvm.trap()
  unreachable

exit_0:                                           ; preds = %when_exit_1
  %19 = load i64, i64* %1, align 4
  %20 = add i64 %19, -1
  store i64 %20, i64* %1, align 4
  %21 = load i8*, i8** %13, align 8
  %22 = getelementptr i8, i8* %21, i64 %20
  %23 = load i8, i8* %22, align 1
  %24 = icmp eq i8 %23, 0
  br i1 %24, label %exit_1, label %when_true_3

when_true_3:                                      ; preds = %exit_0
  tail call void @llvm.trap()
  unreachable

exit_1:                                           ; preds = %exit_0
  %25 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 0
  %26 = load i64, i64* %25, align 4
  %27 = icmp sgt i64 %26, 0
  br i1 %27, label %if_true_0, label %if_false_0

if_true_0:                                        ; preds = %exit_1
  %28 = add nsw i64 %26, -1
  store i64 %28, i64* %25, align 4
  %29 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 1, i32 2
  %30 = load i64*, i64** %29, align 8
  %31 = getelementptr i64, i64* %30, i64 %28
  %32 = load i64, i64* %31, align 4
  %33 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %34 = load i64*, i64** %33, align 8
  %35 = getelementptr i64, i64* %34, i64 %32
  br label %if_exit_0

if_false_0:                                       ; preds = %exit_1
  %36 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 0
  %37 = load i64, i64* %36, align 4
  %38 = add i64 %37, 1
  %39 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 1
  %40 = load i64, i64* %39, align 4
  %41 = icmp sgt i64 %38, %40
  br i1 %41, label %when_true_4, label %if_false_0.when_exit_4_crit_edge

if_false_0.when_exit_4_crit_edge:                 ; preds = %if_false_0
  %.phi.trans.insert50 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %.pre51 = load i64*, i64** %.phi.trans.insert50, align 8
  br label %when_exit_4

when_true_4:                                      ; preds = %if_false_0
  %42 = shl i64 %38, 1
  %43 = shl i64 %38, 4
  %44 = tail call i8* @GC_malloc(i64 %43)
  %45 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %46 = bitcast i64** %45 to i8**
  %47 = load i8*, i8** %46, align 8
  %48 = load i64, i64* %36, align 4
  %49 = shl i64 %48, 3
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %44, i8* align 1 %47, i64 %49, i1 false)
  %50 = bitcast i8* %44 to i64*
  store i64 %42, i64* %36, align 8
  store i64 %42, i64* %39, align 8
  %51 = bitcast i64** %45 to i8**
  store i8* %44, i8** %51, align 8
  br label %when_exit_4

when_exit_4:                                      ; preds = %if_false_0.when_exit_4_crit_edge, %when_true_4
  %52 = phi i64* [ %.pre51, %if_false_0.when_exit_4_crit_edge ], [ %50, %when_true_4 ]
  store i64 %38, i64* %36, align 4
  %53 = getelementptr i64, i64* %52, i64 %37
  br label %if_exit_0

if_exit_0:                                        ; preds = %when_exit_4, %if_true_0
  %.sink = phi i64* [ %53, %when_exit_4 ], [ %35, %if_true_0 ]
  %storemerge = phi i64 [ %37, %when_exit_4 ], [ %32, %if_true_0 ]
  store i64 %.cast, i64* %.sink, align 4
  ret i64 %storemerge
}

define { i64, i8 } @io.read({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io) local_unnamed_addr {
case_0:
  %0 = tail call i32 @getchar()
  %1 = trunc i32 %0 to i8
  %2 = icmp eq i8 %1, -1
  %3 = alloca { i64, i8 }, align 8
  %4 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %3, i64 0, i32 0
  br i1 %2, label %when_true_0, label %case_1

when_true_0:                                      ; preds = %case_0
  store i64 1, i64* %4, align 8
  %5 = load { i64, i8 }, { i64, i8 }* %3, align 8
  ret { i64, i8 } %5

case_1:                                           ; preds = %case_0
  store i64 0, i64* %4, align 8
  %6 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %3, i64 0, i32 1
  store i8 %1, i8* %6, align 8
  %7 = load { i64, i8 }, { i64, i8 }* %3, align 8
  ret { i64, i8 } %7
}

define { i64, i8 } @io.read_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, i64 %key) local_unnamed_addr {
  %1 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %2 = load i64*, i64** %1, align 8
  %3 = getelementptr i64, i64* %2, i64 %key
  %4 = load i64, i64* %3, align 4
  %5 = icmp slt i64 %4, 1
  br i1 %5, label %when_true_0, label %exit_0

when_true_0:                                      ; preds = %0
  tail call void @llvm.trap()
  unreachable

exit_0:                                           ; preds = %0
  %6 = inttoptr i64 %4 to i8*
  %7 = tail call i32 @fgetc(i8* %6)
  %8 = trunc i32 %7 to i8
  %9 = icmp eq i8 %8, -1
  %10 = alloca { i64, i8 }, align 8
  %11 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %10, i64 0, i32 0
  br i1 %9, label %when_true_1, label %case_1

when_true_1:                                      ; preds = %exit_0
  store i64 1, i64* %11, align 8
  %12 = load { i64, i8 }, { i64, i8 }* %10, align 8
  ret { i64, i8 } %12

case_1:                                           ; preds = %exit_0
  store i64 0, i64* %11, align 8
  %13 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %10, i64 0, i32 1
  store i8 %8, i8* %13, align 8
  %14 = load { i64, i8 }, { i64, i8 }* %10, align 8
  ret { i64, i8 } %14
}

define i1 @io.readLn({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, { i64, i64, i8* }* %line, i64 %key) local_unnamed_addr {
  br label %while_cond_0

while_cond_0:                                     ; preds = %switch_exit_0, %0
  %1 = tail call { i64, i8 } @io.read_1({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %key)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 0
  %4 = load i64, i64* %3, align 8
  switch i64 %4, label %case_stmt_4 [
    i64 1, label %when_true_0
    i64 0, label %when_true_1
  ]

when_true_0:                                      ; preds = %when_true_1, %when_true_1, %while_cond_0
  %merge = phi i1 [ false, %while_cond_0 ], [ true, %when_true_1 ], [ true, %when_true_1 ]
  ret i1 %merge

when_true_1:                                      ; preds = %while_cond_0
  %5 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 1
  %6 = load i8, i8* %5, align 8
  switch i8 %6, label %switch_exit_0 [
    i8 10, label %when_true_0
    i8 0, label %when_true_0
  ]

case_stmt_4:                                      ; preds = %while_cond_0
  tail call void @llvm.trap()
  unreachable

switch_exit_0:                                    ; preds = %when_true_1
  tail call void @strings.write({ i64, i64, i8* }* %line, i8 %6)
  br label %while_cond_0
}

define i1 @io.readLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io, { i64, i64, i8* }* %line) local_unnamed_addr {
  br label %while_cond_0

while_cond_0:                                     ; preds = %switch_exit_0, %0
  %1 = tail call { i64, i8 } @io.read({ { i64, i64, i64* }, { i64, i64, i64* } }* undef)
  %2 = alloca { i64, i8 }, align 8
  store { i64, i8 } %1, { i64, i8 }* %2, align 8
  %3 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 0
  %4 = load i64, i64* %3, align 8
  switch i64 %4, label %case_stmt_4 [
    i64 1, label %when_true_0
    i64 0, label %when_true_1
  ]

when_true_0:                                      ; preds = %when_true_1, %when_true_1, %while_cond_0
  %merge = phi i1 [ false, %while_cond_0 ], [ true, %when_true_1 ], [ true, %when_true_1 ]
  ret i1 %merge

when_true_1:                                      ; preds = %while_cond_0
  %5 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %2, i64 0, i32 1
  %6 = load i8, i8* %5, align 8
  switch i8 %6, label %switch_exit_0 [
    i8 10, label %when_true_0
    i8 0, label %when_true_0
  ]

case_stmt_4:                                      ; preds = %while_cond_0
  tail call void @llvm.trap()
  unreachable

switch_exit_0:                                    ; preds = %when_true_1
  tail call void @strings.write({ i64, i64, i8* }* %line, i8 %6)
  br label %while_cond_0
}

; Function Attrs: nofree nounwind
define void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io, i8 %c) local_unnamed_addr #0 {
  %1 = sext i8 %c to i32
  %2 = tail call i32 @putchar(i32 %1)
  ret void
}

define void @io.write_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io, i64 %i) local_unnamed_addr {
  %1 = alloca { i64, i64, i8* }, align 8
  %2 = bitcast { i64, i64, i8* }* %1 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %2, i8 0, i64 24, i1 false)
  call void @strings.write_3({ i64, i64, i8* }* nonnull %1, i64 %i)
  call void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* undef, { i64, i64, i8* }* nonnull %1)
  ret void
}

define void @io.write_2({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, i64 %key, i8 %c) local_unnamed_addr {
  %1 = getelementptr { { i64, i64, i64* }, { i64, i64, i64* } }, { { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 0, i32 0, i32 2
  %2 = load i64*, i64** %1, align 8
  %3 = getelementptr i64, i64* %2, i64 %key
  %4 = load i64, i64* %3, align 4
  %5 = icmp slt i64 %4, 1
  br i1 %5, label %when_true_0, label %exit_0

when_true_0:                                      ; preds = %0
  tail call void @llvm.trap()
  unreachable

exit_0:                                           ; preds = %0
  %6 = sext i8 %c to i32
  %7 = inttoptr i64 %4 to i8*
  %8 = tail call i32 @fputc(i32 %6, i8* %7)
  ret void
}

; Function Attrs: nofree nounwind
define void @io.write_3({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io, { i64, i64, i8* }* nocapture readonly %s) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %for_body_0
  %.01 = phi i64 [ 0, %for_body_0.lr.ph ], [ %8, %for_body_0 ]
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr i8, i8* %5, i64 %.01
  %7 = load i8, i8* %6, align 1
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* undef, i8 %7)
  %8 = add nuw nsw i64 %.01, 1
  %9 = load i64, i64* %1, align 4
  %10 = icmp slt i64 %8, %9
  br i1 %10, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %for_body_0, %0
  ret void
}

define void @io.write_4({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, { i64, i64, i8* }* nocapture readonly %s, i64 %key) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %for_body_0
  %.01 = phi i64 [ 0, %for_body_0.lr.ph ], [ %8, %for_body_0 ]
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr i8, i8* %5, i64 %.01
  %7 = load i8, i8* %6, align 1
  tail call void @io.write_2({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %key, i8 %7)
  %8 = add nuw nsw i64 %.01, 1
  %9 = load i64, i64* %1, align 4
  %10 = icmp slt i64 %8, %9
  br i1 %10, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %for_body_0, %0
  ret void
}

define void @io.writeLn({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readonly %io, { i64, i64, i8* }* nocapture readonly %line, i64 %key) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %line, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %line, i64 0, i32 2
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %for_body_0
  %.01 = phi i64 [ 0, %for_body_0.lr.ph ], [ %8, %for_body_0 ]
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr i8, i8* %5, i64 %.01
  %7 = load i8, i8* %6, align 1
  tail call void @io.write_2({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %key, i8 %7)
  %8 = add nuw nsw i64 %.01, 1
  %9 = load i64, i64* %1, align 4
  %10 = icmp slt i64 %8, %9
  br i1 %10, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %for_body_0, %0
  tail call void @io.write_2({ { i64, i64, i64* }, { i64, i64, i64* } }* %io, i64 %key, i8 10)
  ret void
}

; Function Attrs: nofree nounwind
define void @io.writeLn_1({ { i64, i64, i64* }, { i64, i64, i64* } }* nocapture readnone %io, { i64, i64, i8* }* nocapture readonly %line) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %line, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %line, i64 0, i32 2
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %for_body_0
  %.01 = phi i64 [ 0, %for_body_0.lr.ph ], [ %8, %for_body_0 ]
  %5 = load i8*, i8** %4, align 8
  %6 = getelementptr i8, i8* %5, i64 %.01
  %7 = load i8, i8* %6, align 1
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* undef, i8 %7)
  %8 = add nuw nsw i64 %.01, 1
  %9 = load i64, i64* %1, align 4
  %10 = icmp slt i64 %8, %9
  br i1 %10, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %for_body_0, %0
  tail call void @io.write({ { i64, i64, i64* }, { i64, i64, i64* } }* undef, i8 10)
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

attributes #0 = { nofree nounwind }
attributes #1 = { argmemonly nounwind }

