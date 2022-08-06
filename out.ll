; ModuleID = 'out'


 


define external ccc  i64 @_1__lambda(i64  %param0, i64  %param1)    {
  %1 = add   i64 %param1, %param0 
  ret i64 %1 
}


define external ccc  i64 @_2__lambda(i64  %param0, i64  %param1)    {
  %1 = add   i64 %param1, %param0 
  ret i64 %1 
}


define external ccc  i64 @_sp1f10_2__lambda_sp1f10_1__lambdaaddBoth(i64  %param0, i64  %param1, i64  %param2)    {
  %1 =  call ccc  i64  @_1__lambda(i64  %param0, i64  %param2)  
  %2 =  call ccc  i64  @_2__lambda(i64  %param1, i64  %param2)  
  %3 = add   i64 %1, %2 
  ret i64 %3 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @weirdAdd(i64  1, i64  2, i64  3)  
  ret i64 %1 
}


define external ccc  i64 @weirdAdd(i64  %param0, i64  %param1, i64  %param2)    {
  %1 =  call ccc  i64  @_sp1f10_2__lambda_sp1f10_1__lambdaaddBoth(i64  %param0, i64  %param1, i64  %param2)  
  ret i64 %1 
}
