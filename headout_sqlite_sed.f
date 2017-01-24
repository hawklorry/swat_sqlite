      subroutine headout_sqlite_sed

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table sed in SQLite database for reach sediment
!!     The file handle for output.sed is 84

      use parm

      integer :: j,sedbasiccolnum,sedvaluecolnum

      tblsed = 'sed'

      sedbasiccolnum = 1 + datecol_num
      sedvaluecolnum = size(hedsed)

      allocate( colsed(sedbasiccolnum + sedvaluecolnum) )

      call sqlite3_column_props( colsed(1), "RCH", SQLITE_INT)
      call headout_sqlite_adddate(colsed,
     &          sedbasiccolnum + sedvaluecolnum,2)

      do j=1,sedvaluecolnum
        call sqlite3_column_props( colsed(sedbasiccolnum + j),
     &                                          hedsed(j), SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblsed, colsed )
      call headout_sqlite_createindex("sed_index",tblsed,"RCH",1)
      call sqlite3_insert_sql_statement(db,tblsed, colsed, stmtsed)
      end
