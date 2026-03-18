; ModuleID = 'mfl'
source_filename = "mfl"

declare void @printint(i32)

declare void @printbool(i1)

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 0, ptr %a, align 4
  %b = alloca i32, align 4
  store i32 1, ptr %b, align 4
  br label %for_init

for_init:                                         ; preds = %entry
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %for_cond

for_cond:                                         ; preds = %for_incr, %for_init
  %i1 = load i32, ptr %i, align 4
  %lttmp = icmp slt i32 %i1, 10
  br i1 %lttmp, label %for_body, label %for_after

for_incr:                                         ; preds = %for_body
  %i7 = load i32, ptr %i, align 4
  %incdec = add i32 %i7, 1
  store i32 %incdec, ptr %i, align 4
  br label %for_cond

for_body:                                         ; preds = %for_cond
  %a2 = load i32, ptr %a, align 4
  call void @printint(i32 %a2)
  %next = alloca i32, align 4
  %a3 = load i32, ptr %a, align 4
  %b4 = load i32, ptr %b, align 4
  %addtmp = add i32 %a3, %b4
  store i32 %addtmp, ptr %next, align 4
  %b5 = load i32, ptr %b, align 4
  store i32 %b5, ptr %a, align 4
  %next6 = load i32, ptr %next, align 4
  store i32 %next6, ptr %b, align 4
  br label %for_incr

for_after:                                        ; preds = %for_cond
  ret i32 0
}
