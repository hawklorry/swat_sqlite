      subroutine stdaa_sqlite_writebasin(bname,bvalue)

      use parm
      
      character(len=*), intent(in)  :: bname
      REAL, intent(in)              :: bvalue

      call sqlite3_set_column( colabn(1),bname)
      call sqlite3_set_column( colabn(2),bvalue )
      call sqlite3_insert( db, tblabn, colabn )

      end
