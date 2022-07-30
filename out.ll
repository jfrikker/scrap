; ModuleID = 'out'


 


define external ccc  i64 @_4main()    {
  %1 =  call ccc  i64  @_9increment(i64  123)  
  ret i64 %1 
}


define external ccc  i64 @_3one()    {
  ret i64 1 
}


define external ccc  i64 @_9increment(i64  %param0)    {
  %1 =  call ccc  i64  @_3one()  
  %2 = add   i64 %param0, %1 
  ret i64 %2 
}
