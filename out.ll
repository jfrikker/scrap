; ModuleID = 'out'


 


define external ccc  i64 @add(i64  %i1, i64  %i2)    {
  %1 = add   i64 %i2, %i1 
  %2 = add   i64 %i1, %1 
  %3 = add   i64 %i2, %i1 
  %4 = add   i64 %3, 1 
  %5 = add   i64 %i2, %4 
  %6 = add   i64 %2, %5 
  ret i64 %6 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @add(i64  999, i64  123)  
  ret i64 %1 
}
