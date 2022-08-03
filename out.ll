; ModuleID = 'out'


 


define external ccc  i64 @_sp1f4add3flip(i64  %param0, i64  %param1, i64  %param2)    {
  %1 =  call ccc  i64  @add3(i64  %param0, i64  %param2, i64  %param1)  
  ret i64 %1 
}


define external ccc  i64 @add3(i64  %param0, i64  %param1, i64  %param2)    {
  %1 = add   i64 %param0, %param1 
  %2 = add   i64 %1, %param2 
  ret i64 %2 
}


define external ccc  i64 @increment(i64  %param0)    {
  %1 =  call ccc  i64  @one()  
  %2 =  call ccc  i64  @_sp1f4add3flip(i64  0, i64  %param0, i64  %1)  
  ret i64 %2 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @increment(i64  123)  
  ret i64 %1 
}


define external ccc  i64 @one()    {
  ret i64 1 
}
