; ModuleID = 'out'


 


define external ccc  i64 @_1__lambda(i64  %i1, i64  %l1)    {
  %1 = add   i64 %l1, %i1 
  ret i64 %1 
}


define external ccc  i64 @_2__lambda(i64  %i2, i64  %l1)    {
  %1 = add   i64 %l1, %i2 
  ret i64 %1 
}


define external ccc  i64 @_sp1f10_2__lambda_sp1f10_1__lambdaaddBoth(i64  %c, i64  %c2, i64  %i)    {
  %1 =  call ccc  i64  @_1__lambda(i64  %c, i64  %i)  
  %2 =  call ccc  i64  @_2__lambda(i64  %c2, i64  %i)  
  %3 = add   i64 %1, %2 
  ret i64 %3 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @weirdAdd(i64  1, i64  2, i64  3)  
  ret i64 %1 
}


define external ccc  i64 @weirdAdd(i64  %i1, i64  %i2, i64  %i3)    {
  %1 =  call ccc  i64  @_sp1f10_2__lambda_sp1f10_1__lambdaaddBoth(i64  %i1, i64  %i2, i64  %i3)  
  ret i64 %1 
}
