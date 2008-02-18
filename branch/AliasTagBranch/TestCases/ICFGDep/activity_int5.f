



       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
                         
         i=1
         
c                            ImplicitRemoves:
c                            ================
c                            [1: head::i]

 
         do while (i<3)

           if (i<2) then

             y(2)=x(1)
             
c                            mUses             =>  mDefs
c                            ========================================
c                            [2: head::x()]    =>  [3: head::y()]

c                            ImplicitRemoves:
c                            =================
c

           else

             y(1)=x(2)
             
c                            mUses            => mDefs
c                            =======================================
c                            [2: head::x()]   => [3: head::y()]

c                            ImplicitRemoves: 
c                            ================

           end if 

           i=i+1                  
c                            The reflexive pairs (eg. <i,i>) 
c                            are implicitly assumed to be dependent.
 

         end do

         y(2)=y(1)*y(2)    
c                            The reflexive pairs (eg. <y(),y()>) are 
c                            implicitly assumed to be dependent.
 

       end subroutine

