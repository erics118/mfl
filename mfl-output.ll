; ModuleID = 'mfl'
source_filename = "mfl"

declare void @printint(i32)

declare void @printbool(i8)

define i8 @f() {
entry:
  ret i8 0
}

define i32 @main() {
entry:
  %x = alloca i8, align 1
  %calltmp = call i8 @f()
  %callb = trunc i8 %calltmp to i1
  %storeb = zext i1 %callb to i8
  store i8 %storeb, ptr %x, align 1
  %a = alloca i8, align 1
  ret i32 0
}
