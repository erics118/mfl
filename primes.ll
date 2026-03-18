; ModuleID = 'mfl'
source_filename = "mfl"

declare void @printint(i32)

declare void @printbool(i1)

define i1 @is_prime(i32 %n) {
entry:
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  br label %for_init

for_init:                                         ; preds = %entry
  %d = alloca i32, align 4
  store i32 2, ptr %d, align 4
  br label %for_cond

for_cond:                                         ; preds = %for_incr, %for_init
  %d2 = load i32, ptr %d, align 4
  %d3 = load i32, ptr %d, align 4
  %multmp = mul i32 %d2, %d3
  %n4 = load i32, ptr %n1, align 4
  %letmp = icmp sle i32 %multmp, %n4
  br i1 %letmp, label %for_body, label %for_after

for_incr:                                         ; preds = %merge
  %d7 = load i32, ptr %d, align 4
  %incdec = add i32 %d7, 1
  store i32 %incdec, ptr %d, align 4
  br label %for_cond

for_body:                                         ; preds = %for_cond
  %n5 = load i32, ptr %n1, align 4
  %d6 = load i32, ptr %d, align 4
  %modtmp = srem i32 %n5, %d6
  %eqtmp = icmp eq i32 %modtmp, 0
  br i1 %eqtmp, label %then, label %else

for_after:                                        ; preds = %for_cond
  ret i1 true

then:                                             ; preds = %for_body
  ret i1 false

else:                                             ; preds = %for_body
  br label %merge

merge:                                            ; preds = %else
  br label %for_incr
}

define i32 @main() {
entry:
  br label %for_init

for_init:                                         ; preds = %entry
  %n = alloca i32, align 4
  store i32 2, ptr %n, align 4
  br label %for_cond

for_cond:                                         ; preds = %for_incr, %for_init
  %n1 = load i32, ptr %n, align 4
  %lttmp = icmp slt i32 %n1, 100
  br i1 %lttmp, label %for_body, label %for_after

for_incr:                                         ; preds = %merge
  %n4 = load i32, ptr %n, align 4
  %incdec = add i32 %n4, 1
  store i32 %incdec, ptr %n, align 4
  br label %for_cond

for_body:                                         ; preds = %for_cond
  %n2 = load i32, ptr %n, align 4
  %calltmp = call i1 @is_prime(i32 %n2)
  br i1 %calltmp, label %then, label %else

for_after:                                        ; preds = %for_cond
  ret i32 0

then:                                             ; preds = %for_body
  %n3 = load i32, ptr %n, align 4
  call void @printint(i32 %n3)
  br label %merge

else:                                             ; preds = %for_body
  br label %merge

merge:                                            ; preds = %else, %then
  br label %for_incr
}
