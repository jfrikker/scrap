; ModuleID = 'out'


 


define external ccc  i64 @I64.incrementBy(i64  %this, i64  %other)    {
  %1 = add   i64 %this, %other 
  ret i64 %1 
}


define external ccc  i64 @I64.incremented(i64  %this)    {
  %1 =  call ccc  i64  @I64.incrementBy(i64  %this, i64  1)  
  ret i64 %1 
}


define external ccc  i64 @I64.incrementedTwice(i64  %this)    {
  %1 =  call ccc  i64  @I64.incremented(i64  %this)  
  %2 =  call ccc  i64  @I64.incremented(i64  %1)  
  ret i64 %2 
}


define external ccc  i64 @_1__lambda(i64  %i1, i64  %i2)    {
  %1 = add   i64 %i1, %i2 
  ret i64 %1 
}


define external ccc  i64 @_sp0f10_1__lambdaflip(i64  %a1, i64  %a2)    {
  %1 =  call ccc  i64  @_1__lambda(i64  %a2, i64  %a1)  
  ret i64 %1 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @I64.incrementedTwice(i64  2)  
  %2 =  call ccc  i64  @_sp0f10_1__lambdaflip(i64  1, i64  %1)  
  ret i64 %2 
}
