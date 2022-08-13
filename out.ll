; ModuleID = 'out'


 


define external ccc  i64 @_s3addflip(i64  %a1, i64  %a2)    {
  %1 =  call ccc  i64  @add(i64  %a2, i64  %a1)  
  ret i64 %1 
}


define external ccc  i64 @add(i64  %i1, i64  %i2)    {
  %1 = add   i64 %i1, %i2 
  ret i64 %1 
}


define external ccc  i64 @main()    {
  %1 =  call ccc  i64  @_s3addflip(i64  1, i64  2)  
  ret i64 %1 
}
