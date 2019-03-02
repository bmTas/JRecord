 /* rexx */
 
/* return 1*/

     env = uname('S')
   
     if env = 'UNIX' | env = 'Linux' then do
     	   say env '....'
  	  return 1
     end
     
       say env '###'
     return 0
