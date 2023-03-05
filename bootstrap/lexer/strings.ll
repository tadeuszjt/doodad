; ModuleID = '<string>'
source_filename = "<string>"

@strings.str = unnamed_addr constant [5 x i8] c"true\00"
@strings.str_1 = unnamed_addr constant [6 x i8] c"false\00"

declare i1 @chars.isAlpha(i8) local_unnamed_addr

declare i1 @chars.isDigit(i8) local_unnamed_addr

declare i1 @chars.isSpace(i8) local_unnamed_addr

; Function Attrs: norecurse nounwind readonly
define { i64, i8 } @strings.At({ i64, i64, i8* }* nocapture readonly %s, i64 %i) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %i, -1
  %4 = icmp sgt i64 %2, %i
  %5 = and i1 %3, %4
  br i1 %5, label %if_true_0, label %if_false_0

if_true_0:                                        ; preds = %0
  %6 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %7 = load i8*, i8** %6, align 8
  %8 = getelementptr i8, i8* %7, i64 %i
  %9 = alloca { i64, i8 }, align 8
  %10 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %9, i64 0, i32 0
  store i64 1, i64* %10, align 8
  %11 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %9, i64 0, i32 1
  %12 = load i8, i8* %8, align 1
  store i8 %12, i8* %11, align 8
  %13 = load { i64, i8 }, { i64, i8 }* %9, align 8
  ret { i64, i8 } %13

if_false_0:                                       ; preds = %0
  %14 = alloca { i64, i8 }, align 8
  %15 = getelementptr inbounds { i64, i8 }, { i64, i8 }* %14, i64 0, i32 0
  store i64 0, i64* %15, align 8
  %16 = load { i64, i8 }, { i64, i8 }* %14, align 8
  ret { i64, i8 } %16
}

; Function Attrs: norecurse nounwind readonly
define { i64, i64 } @strings.find({ i64, i64, i8* }* nocapture readonly %s, i8 %c) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %5 = load i8*, i8** %4, align 8
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %if_exit_0
  %.07 = phi i64 [ 0, %for_body_0.lr.ph ], [ %10, %if_exit_0 ]
  %6 = getelementptr i8, i8* %5, i64 %.07
  %7 = load i8, i8* %6, align 1
  %8 = icmp eq i8 %7, %c
  br i1 %8, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %for_body_0
  %9 = insertvalue { i64, i64 } { i64 1, i64 undef }, i64 %.07, 1
  ret { i64, i64 } %9

if_exit_0:                                        ; preds = %for_body_0
  %10 = add nuw nsw i64 %.07, 1
  %11 = icmp slt i64 %10, %2
  br i1 %11, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %if_exit_0, %0
  ret { i64, i64 } { i64 0, i64 undef }
}

; Function Attrs: norecurse nounwind readonly
define { i64, i64 } @strings.find_1({ i64, i64, i8* }* nocapture readonly %s, { i64, i64, i8* }* nocapture readonly %sub) local_unnamed_addr #0 {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %sub, i64 0, i32 0
  %5 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %6 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %sub, i64 0, i32 2
  %7 = load i64, i64* %4, align 4
  br label %for_body_1.lr.ph

for_body_1.lr.ph:                                 ; preds = %if_exit_0, %for_body_0.lr.ph
  %.014 = phi i64 [ 0, %for_body_0.lr.ph ], [ %24, %if_exit_0 ]
  br label %for_body_1

for_body_1:                                       ; preds = %for_body_1.lr.ph, %for_cond_1.backedge
  %8 = phi i64 [ 0, %for_body_1.lr.ph ], [ %18, %for_cond_1.backedge ]
  %9 = phi i64 [ %.014, %for_body_1.lr.ph ], [ %19, %for_cond_1.backedge ]
  %10 = icmp slt i64 %8, %7
  br i1 %10, label %when_exit_0, label %for_exit_1

when_exit_0:                                      ; preds = %for_body_1
  %11 = load i8*, i8** %5, align 8
  %12 = getelementptr i8, i8* %11, i64 %9
  %13 = load i8*, i8** %6, align 8
  %14 = getelementptr i8, i8* %13, i64 %8
  %15 = load i8, i8* %12, align 1
  %16 = load i8, i8* %14, align 1
  %17 = icmp eq i8 %15, %16
  br i1 %17, label %for_cond_1.backedge, label %for_exit_1

for_cond_1.backedge:                              ; preds = %when_exit_0
  %18 = add nuw nsw i64 %8, 1
  %19 = add nuw nsw i64 %9, 1
  %20 = icmp slt i64 %19, %2
  br i1 %20, label %for_body_1, label %for_exit_1

for_exit_1:                                       ; preds = %for_cond_1.backedge, %for_body_1, %when_exit_0
  %21 = phi i64 [ %8, %when_exit_0 ], [ %8, %for_body_1 ], [ %18, %for_cond_1.backedge ]
  %22 = icmp eq i64 %21, %7
  br i1 %22, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %for_exit_1
  %23 = insertvalue { i64, i64 } { i64 1, i64 undef }, i64 %.014, 1
  ret { i64, i64 } %23

if_exit_0:                                        ; preds = %for_exit_1
  %24 = add nuw nsw i64 %.014, 1
  %25 = icmp slt i64 %24, %2
  br i1 %25, label %for_body_1.lr.ph, label %for_exit_0

for_exit_0:                                       ; preds = %if_exit_0, %0
  ret { i64, i64 } { i64 0, i64 undef }
}

; Function Attrs: norecurse nounwind readonly
define i1 @strings.isPrefix({ i64, i64, i8* }* nocapture readonly %s, { i64, i64, i8* }* nocapture readonly %sub) local_unnamed_addr #0 {
exit_0:
  %0 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %sub, i64 0, i32 0
  %1 = load i64, i64* %0, align 4
  %2 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %3 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %sub, i64 0, i32 2
  %5 = icmp sgt i64 %1, 0
  br i1 %5, label %for_body_0.preheader, label %for_exit_0

for_body_0.preheader:                             ; preds = %exit_0
  %6 = load i64, i64* %2, align 4
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.preheader, %when_exit_0
  %.0.ph10 = phi i64 [ %15, %when_exit_0 ], [ 0, %for_body_0.preheader ]
  %.06.ph9 = phi i64 [ %16, %when_exit_0 ], [ 0, %for_body_0.preheader ]
  %7 = icmp slt i64 %.06.ph9, %6
  br i1 %7, label %when_true_0, label %for_exit_0

when_true_0:                                      ; preds = %for_body_0
  %8 = load i8*, i8** %3, align 8
  %9 = getelementptr i8, i8* %8, i64 %.06.ph9
  %10 = load i8*, i8** %4, align 8
  %11 = getelementptr i8, i8* %10, i64 %.06.ph9
  %12 = load i8, i8* %9, align 1
  %13 = load i8, i8* %11, align 1
  %14 = icmp eq i8 %12, %13
  br i1 %14, label %when_exit_0, label %for_exit_0

when_exit_0:                                      ; preds = %when_true_0
  %15 = add i64 %.0.ph10, 1
  %16 = add nuw nsw i64 %.06.ph9, 1
  %17 = icmp slt i64 %16, %1
  br i1 %17, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_exit_0, %for_body_0, %when_true_0, %exit_0
  %.0.ph.lcssa = phi i64 [ 0, %exit_0 ], [ %15, %when_exit_0 ], [ %.0.ph10, %for_body_0 ], [ %.0.ph10, %when_true_0 ]
  %18 = icmp eq i64 %.0.ph.lcssa, %1
  ret i1 %18
}

define { i8, i64 } @strings.read({ i64, i64, i8* }* nocapture readonly %s, i64 %start) local_unnamed_addr {
exit_0:
  %0 = icmp sgt i64 %start, 0
  %storemerge = select i1 %0, i64 %start, i64 0
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %4 = icmp slt i64 %storemerge, %2
  br i1 %4, label %for_body_0, label %for_exit_0

for_cond_0:                                       ; preds = %when_true_0
  %5 = icmp slt i64 %14, %2
  br i1 %5, label %for_body_0, label %for_exit_0

for_body_0:                                       ; preds = %exit_0, %for_cond_0
  %.0.ph15 = phi i64 [ %13, %for_cond_0 ], [ 0, %exit_0 ]
  %.05.ph14 = phi i64 [ %14, %for_cond_0 ], [ %storemerge, %exit_0 ]
  %6 = load i8*, i8** %3, align 8
  %7 = getelementptr i8, i8* %6, i64 %.05.ph14
  %8 = load i8, i8* %7, align 1
  %9 = tail call i1 @chars.isSpace(i8 %8)
  %10 = tail call i1 @chars.isAlpha(i8 %8)
  %11 = or i1 %9, %10
  br i1 %11, label %when_true_0, label %for_exit_0

when_true_0:                                      ; preds = %for_body_0
  %12 = tail call i1 @chars.isAlpha(i8 %8)
  %13 = add i64 %.0.ph15, 1
  %14 = add nuw nsw i64 %.05.ph14, 1
  br i1 %12, label %if_true_0, label %for_cond_0

if_true_0:                                        ; preds = %when_true_0
  %15 = alloca { i8, i64 }, align 8
  %16 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %15, i64 0, i32 0
  %17 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %15, i64 0, i32 1
  store i8 %8, i8* %16, align 8
  store i64 %13, i64* %17, align 4
  %18 = load { i8, i64 }, { i8, i64 }* %15, align 8
  ret { i8, i64 } %18

for_exit_0:                                       ; preds = %for_cond_0, %for_body_0, %exit_0
  %19 = alloca { i8, i64 }, align 8
  %20 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %19, i64 0, i32 0
  %21 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %19, i64 0, i32 1
  store i8 0, i8* %20, align 8
  store i64 0, i64* %21, align 4
  %22 = load { i8, i64 }, { i8, i64 }* %19, align 8
  ret { i8, i64 } %22
}

define { i64, i64 } @strings.read_1({ i64, i64, i8* }* nocapture readonly %s, i64 %start) local_unnamed_addr {
exit_0:
  %0 = icmp sgt i64 %start, 0
  %storemerge8 = select i1 %0, i64 %start, i64 0
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %4 = icmp slt i64 %storemerge8, %2
  br i1 %4, label %for_body_0, label %for_exit_0

for_body_0:                                       ; preds = %exit_0, %when_exit_0
  %.0.ph31 = phi i64 [ %9, %when_exit_0 ], [ 0, %exit_0 ]
  %.012.ph30 = phi i64 [ %10, %when_exit_0 ], [ %storemerge8, %exit_0 ]
  %5 = load i8*, i8** %3, align 8
  %6 = getelementptr i8, i8* %5, i64 %.012.ph30
  %7 = load i8, i8* %6, align 1
  %8 = tail call i1 @chars.isSpace(i8 %7)
  br i1 %8, label %when_exit_0, label %for_exit_0

when_exit_0:                                      ; preds = %for_body_0
  %9 = add i64 %.0.ph31, 1
  %10 = add nuw nsw i64 %.012.ph30, 1
  %11 = icmp slt i64 %10, %2
  br i1 %11, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_exit_0, %for_body_0, %exit_0
  %.0.ph.lcssa = phi i64 [ 0, %exit_0 ], [ %9, %when_exit_0 ], [ %.0.ph31, %for_body_0 ]
  %12 = add i64 %.0.ph.lcssa, %start
  %13 = icmp sgt i64 %12, 0
  %storemerge = select i1 %13, i64 %12, i64 0
  %14 = load i64, i64* %1, align 4
  %15 = icmp slt i64 %storemerge, %14
  br i1 %15, label %for_body_1, label %if_exit_0

for_body_1:                                       ; preds = %for_exit_0, %for_cond_1.backedge
  %16 = phi i64 [ %30, %for_cond_1.backedge ], [ 0, %for_exit_0 ]
  %17 = phi i64 [ %29, %for_cond_1.backedge ], [ 0, %for_exit_0 ]
  %18 = phi i64 [ %31, %for_cond_1.backedge ], [ %storemerge, %for_exit_0 ]
  %19 = load i8*, i8** %3, align 8
  %20 = getelementptr i8, i8* %19, i64 %18
  %21 = load i8, i8* %20, align 1
  %22 = tail call i1 @chars.isDigit(i8 %21)
  br i1 %22, label %for_cond_1.backedge, label %for_exit_1

for_cond_1.backedge:                              ; preds = %for_body_1
  %23 = mul i64 %17, 10
  %24 = load i8*, i8** %3, align 8
  %25 = getelementptr i8, i8* %24, i64 %18
  %26 = load i8, i8* %25, align 1
  %27 = add i8 %26, -48
  %28 = sext i8 %27 to i64
  %29 = add i64 %23, %28
  %30 = add i64 %16, 1
  %31 = add nuw nsw i64 %18, 1
  %32 = icmp slt i64 %31, %14
  br i1 %32, label %for_body_1, label %for_exit_1

for_exit_1:                                       ; preds = %for_cond_1.backedge, %for_body_1
  %33 = phi i64 [ %17, %for_body_1 ], [ %29, %for_cond_1.backedge ]
  %34 = phi i64 [ %16, %for_body_1 ], [ %30, %for_cond_1.backedge ]
  %35 = icmp sgt i64 %34, 0
  br i1 %35, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %for_exit_1
  %36 = add i64 %34, %.0.ph.lcssa
  %37 = insertvalue { i64, i64 } undef, i64 %33, 0
  %38 = insertvalue { i64, i64 } %37, i64 %36, 1
  ret { i64, i64 } %38

if_exit_0:                                        ; preds = %for_exit_0, %for_exit_1
  ret { i64, i64 } zeroinitializer
}

declare i8* @GC_malloc(i64) local_unnamed_addr

define void @strings.write({ i64, i64, i8* }* nocapture %s, i8 %c) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = add i64 %2, 1
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %5 = load i64, i64* %4, align 4
  %6 = icmp sgt i64 %3, %5
  br i1 %6, label %when_true_0, label %.when_exit_0_crit_edge

.when_exit_0_crit_edge:                           ; preds = %0
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_0

when_true_0:                                      ; preds = %0
  %7 = shl i64 %3, 1
  %8 = tail call i8* @GC_malloc(i64 %7)
  %9 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %10 = load i8*, i8** %9, align 8
  %11 = load i64, i64* %1, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %8, i8* align 1 %10, i64 %11, i1 false)
  store i64 %7, i64* %1, align 8
  store i64 %7, i64* %4, align 8
  store i8* %8, i8** %9, align 8
  br label %when_exit_0

when_exit_0:                                      ; preds = %.when_exit_0_crit_edge, %when_true_0
  %12 = phi i8* [ %.pre, %.when_exit_0_crit_edge ], [ %8, %when_true_0 ]
  store i64 %3, i64* %1, align 4
  %13 = getelementptr i8, i8* %12, i64 %2
  store i8 %c, i8* %13, align 1
  ret void
}

define void @strings.write_1({ i64, i64, i8* }* nocapture %s, i1 %b) local_unnamed_addr {
  %1 = alloca { i64, i64, i8* }, align 8
  %.repack23 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 0
  %.repack24 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 1
  %.repack25 = getelementptr inbounds { i64, i64, i8* }, { i64, i64, i8* }* %1, i64 0, i32 2
  %2 = bitcast { i64, i64, i8* }* %1 to i8*
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %2, i8 0, i64 24, i1 false)
  br i1 %b, label %when_exit_0, label %when_exit_1

when_exit_0:                                      ; preds = %0
  %3 = tail call i8* @GC_malloc(i64 8)
  store i64 8, i64* %.repack24, align 8
  store i8* %3, i8** %.repack25, align 8
  store i64 4, i64* %.repack23, align 8
  %4 = bitcast i8* %3 to i32*
  store i32 1702195828, i32* %4, align 1
  br label %if_exit_0

when_exit_1:                                      ; preds = %0
  %5 = tail call i8* @GC_malloc(i64 10)
  store i64 10, i64* %.repack24, align 8
  store i8* %5, i8** %.repack25, align 8
  store i64 5, i64* %.repack23, align 8
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %5, i8* align 1 getelementptr inbounds ([6 x i8], [6 x i8]* @strings.str_1, i64 0, i64 0), i64 5, i1 false)
  br label %if_exit_0

if_exit_0:                                        ; preds = %when_exit_1, %when_exit_0
  call void @strings.write_2({ i64, i64, i8* }* %s, { i64, i64, i8* }* nonnull %1)
  ret void
}

define void @strings.write_2({ i64, i64, i8* }* nocapture %s, { i64, i64, i8* }* nocapture readonly %s2) local_unnamed_addr {
  %1 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s2, i64 0, i32 0
  %2 = load i64, i64* %1, align 4
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %for_body_0.lr.ph, label %for_exit_0

for_body_0.lr.ph:                                 ; preds = %0
  %4 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s2, i64 0, i32 2
  %5 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %6 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %7 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  br label %for_body_0

for_body_0:                                       ; preds = %for_body_0.lr.ph, %when_exit_0
  %.012 = phi i64 [ 0, %for_body_0.lr.ph ], [ %21, %when_exit_0 ]
  %8 = load i8*, i8** %4, align 8
  %9 = getelementptr i8, i8* %8, i64 %.012
  %10 = load i8, i8* %9, align 1
  %11 = load i64, i64* %5, align 4
  %12 = add i64 %11, 1
  %13 = load i64, i64* %6, align 4
  %14 = icmp sgt i64 %12, %13
  br i1 %14, label %when_true_0, label %for_body_0.when_exit_0_crit_edge

for_body_0.when_exit_0_crit_edge:                 ; preds = %for_body_0
  %.pre = load i8*, i8** %7, align 8
  br label %when_exit_0

when_true_0:                                      ; preds = %for_body_0
  %15 = shl i64 %12, 1
  %16 = tail call i8* @GC_malloc(i64 %15)
  %17 = load i8*, i8** %7, align 8
  %18 = load i64, i64* %5, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %16, i8* align 1 %17, i64 %18, i1 false)
  store i64 %15, i64* %5, align 8
  store i64 %15, i64* %6, align 8
  store i8* %16, i8** %7, align 8
  br label %when_exit_0

when_exit_0:                                      ; preds = %for_body_0.when_exit_0_crit_edge, %when_true_0
  %19 = phi i8* [ %.pre, %for_body_0.when_exit_0_crit_edge ], [ %16, %when_true_0 ]
  store i64 %12, i64* %5, align 4
  %20 = getelementptr i8, i8* %19, i64 %11
  store i8 %10, i8* %20, align 1
  %21 = add nuw nsw i64 %.012, 1
  %22 = load i64, i64* %1, align 4
  %23 = icmp slt i64 %21, %22
  br i1 %23, label %for_body_0, label %for_exit_0

for_exit_0:                                       ; preds = %when_exit_0, %0
  ret void
}

define void @strings.write_3({ i64, i64, i8* }* %s, i64 %n) local_unnamed_addr {
  %1 = icmp eq i64 %n, 0
  br i1 %1, label %if_true_0, label %if_exit_0

if_true_0:                                        ; preds = %0
  %2 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %3 = load i64, i64* %2, align 4
  %4 = add i64 %3, 1
  %5 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %6 = load i64, i64* %5, align 4
  %7 = icmp sgt i64 %4, %6
  br i1 %7, label %when_true_0, label %if_true_0.when_exit_0_crit_edge

if_true_0.when_exit_0_crit_edge:                  ; preds = %if_true_0
  %.phi.trans.insert69 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre70 = load i8*, i8** %.phi.trans.insert69, align 8
  br label %when_exit_0

when_true_0:                                      ; preds = %if_true_0
  %8 = shl i64 %4, 1
  %9 = tail call i8* @GC_malloc(i64 %8)
  %10 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %11 = load i8*, i8** %10, align 8
  %12 = load i64, i64* %2, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %9, i8* align 1 %11, i64 %12, i1 false)
  store i64 %8, i64* %2, align 8
  store i64 %8, i64* %5, align 8
  store i8* %9, i8** %10, align 8
  br label %when_exit_0

when_exit_0:                                      ; preds = %if_true_0.when_exit_0_crit_edge, %when_true_0
  %13 = phi i8* [ %.pre70, %if_true_0.when_exit_0_crit_edge ], [ %9, %when_true_0 ]
  store i64 %4, i64* %2, align 4
  %14 = getelementptr i8, i8* %13, i64 %3
  store i8 48, i8* %14, align 1
  ret void

if_exit_0:                                        ; preds = %0
  %15 = icmp slt i64 %n, 0
  br i1 %15, label %if_true_1, label %while_body_0.preheader

while_body_0.preheader:                           ; preds = %when_exit_1, %if_exit_0
  %.163.ph = phi i64 [ %n, %if_exit_0 ], [ %29, %when_exit_1 ]
  br label %while_body_0

if_true_1:                                        ; preds = %if_exit_0
  %16 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %17 = load i64, i64* %16, align 4
  %18 = add i64 %17, 1
  %19 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %20 = load i64, i64* %19, align 4
  %21 = icmp sgt i64 %18, %20
  br i1 %21, label %when_true_1, label %if_true_1.when_exit_1_crit_edge

if_true_1.when_exit_1_crit_edge:                  ; preds = %if_true_1
  %.phi.trans.insert = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %.pre = load i8*, i8** %.phi.trans.insert, align 8
  br label %when_exit_1

when_true_1:                                      ; preds = %if_true_1
  %22 = shl i64 %18, 1
  %23 = tail call i8* @GC_malloc(i64 %22)
  %24 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  %25 = load i8*, i8** %24, align 8
  %26 = load i64, i64* %16, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %23, i8* align 1 %25, i64 %26, i1 false)
  store i64 %22, i64* %16, align 8
  store i64 %22, i64* %19, align 8
  store i8* %23, i8** %24, align 8
  br label %when_exit_1

when_exit_1:                                      ; preds = %if_true_1.when_exit_1_crit_edge, %when_true_1
  %27 = phi i8* [ %.pre, %if_true_1.when_exit_1_crit_edge ], [ %23, %when_true_1 ]
  store i64 %18, i64* %16, align 4
  %28 = getelementptr i8, i8* %27, i64 %17
  store i8 45, i8* %28, align 1
  %29 = sub i64 0, %n
  br label %while_body_0.preheader

while_cond_1.preheader:                           ; preds = %when_exit_2
  %30 = icmp sgt i64 %41, 0
  br i1 %30, label %while_body_1.lr.ph, label %while_exit_1

while_body_1.lr.ph:                               ; preds = %while_cond_1.preheader
  %31 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 0
  %32 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 1
  %33 = getelementptr { i64, i64, i8* }, { i64, i64, i8* }* %s, i64 0, i32 2
  br label %while_body_1

while_body_0:                                     ; preds = %while_body_0.preheader, %when_exit_2
  %34 = phi i64* [ %48, %when_exit_2 ], [ null, %while_body_0.preheader ]
  %35 = phi i8* [ %49, %when_exit_2 ], [ null, %while_body_0.preheader ]
  %36 = phi i64* [ %50, %when_exit_2 ], [ null, %while_body_0.preheader ]
  %37 = phi i64 [ %51, %when_exit_2 ], [ 0, %while_body_0.preheader ]
  %38 = phi i64 [ %41, %when_exit_2 ], [ 0, %while_body_0.preheader ]
  %.163 = phi i64 [ %39, %when_exit_2 ], [ %.163.ph, %while_body_0.preheader ]
  %39 = sdiv i64 %.163, 10
  %40 = mul i64 %39, 10
  %.decomposed = sub i64 %.163, %40
  %41 = add i64 %38, 1
  %42 = icmp sgt i64 %41, %37
  br i1 %42, label %when_true_2, label %when_exit_2

when_true_2:                                      ; preds = %while_body_0
  %43 = shl i64 %41, 1
  %44 = shl i64 %41, 4
  %45 = tail call i8* @GC_malloc(i64 %44)
  %46 = shl i64 %38, 3
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %45, i8* align 1 %35, i64 %46, i1 false)
  %47 = bitcast i8* %45 to i64*
  br label %when_exit_2

when_exit_2:                                      ; preds = %when_true_2, %while_body_0
  %48 = phi i64* [ %47, %when_true_2 ], [ %34, %while_body_0 ]
  %49 = phi i8* [ %45, %when_true_2 ], [ %35, %while_body_0 ]
  %50 = phi i64* [ %47, %when_true_2 ], [ %36, %while_body_0 ]
  %51 = phi i64 [ %43, %when_true_2 ], [ %37, %while_body_0 ]
  %52 = getelementptr i64, i64* %50, i64 %38
  store i64 %.decomposed, i64* %52, align 4
  %.163.off = add i64 %.163, 9
  %53 = icmp ult i64 %.163.off, 19
  br i1 %53, label %while_cond_1.preheader, label %while_body_0

while_body_1:                                     ; preds = %while_body_1.lr.ph, %when_exit_3
  %54 = phi i64 [ %41, %while_body_1.lr.ph ], [ %55, %when_exit_3 ]
  %55 = add nsw i64 %54, -1
  %56 = getelementptr i64, i64* %48, i64 %55
  %57 = load i64, i64* %56, align 4
  %58 = trunc i64 %57 to i8
  %59 = add i8 %58, 48
  %60 = load i64, i64* %31, align 4
  %61 = add i64 %60, 1
  %62 = load i64, i64* %32, align 4
  %63 = icmp sgt i64 %61, %62
  br i1 %63, label %when_true_3, label %while_body_1.when_exit_3_crit_edge

while_body_1.when_exit_3_crit_edge:               ; preds = %while_body_1
  %.pre68 = load i8*, i8** %33, align 8
  br label %when_exit_3

when_true_3:                                      ; preds = %while_body_1
  %64 = shl i64 %61, 1
  %65 = tail call i8* @GC_malloc(i64 %64)
  %66 = load i8*, i8** %33, align 8
  %67 = load i64, i64* %31, align 4
  tail call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %65, i8* align 1 %66, i64 %67, i1 false)
  store i64 %64, i64* %31, align 8
  store i64 %64, i64* %32, align 8
  store i8* %65, i8** %33, align 8
  br label %when_exit_3

when_exit_3:                                      ; preds = %while_body_1.when_exit_3_crit_edge, %when_true_3
  %68 = phi i8* [ %.pre68, %while_body_1.when_exit_3_crit_edge ], [ %65, %when_true_3 ]
  store i64 %61, i64* %31, align 4
  %69 = getelementptr i8, i8* %68, i64 %60
  store i8 %59, i8* %69, align 1
  %70 = icmp sgt i64 %55, 0
  br i1 %70, label %while_body_1, label %while_exit_1

while_exit_1:                                     ; preds = %when_exit_3, %while_cond_1.preheader
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1 immarg) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #1

attributes #0 = { norecurse nounwind readonly }
attributes #1 = { argmemonly nounwind }
