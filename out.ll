; ModuleID = 'out'


 


define external ccc  i64 @add(i64  %i1, i64  %i2)    {
  %1 = add   i64 %i1, %i2 
  ret i64 %1 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @add(i64  999, i64  123, i64  456)  
  ret i64 %1 
}
