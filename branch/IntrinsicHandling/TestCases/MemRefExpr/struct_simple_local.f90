! F8ISTORE 0 T<30,anon_ptr.,8>
!  F8F8LDID 0 <2,2,X> T<11,.predef_F8,8>
!  U8U8STRCTFLD T<11,.predef_F8,8> T<29,MYTYPE,8> <field_id:1>
!   U8LDA 0 <2,1,TYPED_Y> T<30,anon_ptr.,8>

module myTypeModule

        implicit none
        private
        public :: myType

        type myType
sequence
double precision :: field1 
end type myType
end module

subroutine head() 
  use myTypeModule
  double precision  x
  type(myType) :: typed_y
  typed_y%field1=x
end subroutine

