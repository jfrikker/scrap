; ModuleID = 'out'


 


define external ccc  i64 @add(i64  %i1, i64  %i2)    {
  %1 = add   i64 %i2, %i1 
  %2 = add   i64 %i1, %1 
  ret i64 %2 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @add(i64  999, i64  123)  
  ret i64 %1 
}
