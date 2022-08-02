; ModuleID = 'out'


 


define external ccc  i64 @_s1_4flip3add(i64  %param0, i64  %param1)    {
  %1 =  call ccc  i64  @add(i64  %param1, i64  %param0)  
  ret i64 %1 
}


define external ccc  i64 @add(i64  %param0, i64  %param1)    {
  %1 = add   i64 %param0, %param1 
  ret i64 %1 
}


define external ccc  i64 @increment(i64  %param0)    {
  %1 =  call ccc  i64  @one()  
  %2 =  call ccc  i64  @_s1_4flip3add(i64  %param0, i64  %1)  
  ret i64 %2 
}


define external ccc  i64 @one()    {
  ret i64 1 
}
