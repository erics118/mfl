; ModuleID = 'calc'

declare external ccc i32 @printint(i32)

define external ccc i32 @main()    {
  %1 =  call ccc i32 @printint(i32  42)
  ret i32 0
}
