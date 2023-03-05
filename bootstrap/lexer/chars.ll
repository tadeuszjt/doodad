; ModuleID = '<string>'
source_filename = "<string>"

declare i32 @tolower(i32) local_unnamed_addr

declare i32 @toupper(i32) local_unnamed_addr

; Function Attrs: norecurse nounwind readnone
define i1 @chars.isAlpha(i8 %c) local_unnamed_addr #0 {
  %1 = and i8 %c, -33
  %2 = add i8 %1, -65
  %3 = icmp ult i8 %2, 26
  ret i1 %3
}

; Function Attrs: norecurse nounwind readnone
define i1 @chars.isDigit(i8 %c) local_unnamed_addr #0 {
  %c.off = add i8 %c, -48
  %1 = icmp ult i8 %c.off, 10
  ret i1 %1
}

; Function Attrs: norecurse nounwind readnone
define i1 @chars.isSpace(i8 %c) local_unnamed_addr #0 {
  %1 = icmp eq i8 %c, 32
  %2 = add i8 %c, -9
  %3 = icmp ult i8 %2, 2
  %4 = or i1 %1, %3
  ret i1 %4
}

; Function Attrs: norecurse nounwind readnone
define i64 @chars.readDigit(i8 %c) local_unnamed_addr #0 {
when_exit_0:
  %c.off = add i8 %c, -48
  %0 = icmp ult i8 %c.off, 10
  br i1 %0, label %if_true_0, label %if_false_0

if_true_0:                                        ; preds = %when_exit_0
  %1 = sext i8 %c.off to i64
  ret i64 %1

if_false_0:                                       ; preds = %when_exit_0
  ret i64 0
}

define i8 @chars.toLower(i8 %c) local_unnamed_addr {
  %1 = sext i8 %c to i32
  %2 = tail call i32 @tolower(i32 %1)
  %3 = trunc i32 %2 to i8
  ret i8 %3
}

define i8 @chars.toUpper(i8 %c) local_unnamed_addr {
  %1 = sext i8 %c to i32
  %2 = tail call i32 @toupper(i32 %1)
  %3 = trunc i32 %2 to i8
  ret i8 %3
}

attributes #0 = { norecurse nounwind readnone }
