      subroutine headout_sqlite_pot

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table pot in SQLite database for potholes
!!     The file handle for output.pot is 125

      use parm

      integer :: j,potbasiccolnum,potvaluecolnum

      !!create table pot
      if (iwtr == 1) then
          tblpot = 'pot'

          potvaluecolnum = size(hedpot)
          potbasiccolnum = 1 + datecol_num

          allocate( colpot(potbasiccolnum + potvaluecolnum) )

          call sqlite3_column_props( colpot(1), "HRU", SQLITE_INT)
          call headout_sqlite_adddate(colpot,
     &     potbasiccolnum + potvaluecolnum,2)
          do j = 1, potvaluecolnum
             call sqlite3_column_props(colpot(potbasiccolnum+j),
     &                                          hedpot(j),SQLITE_REAL)
          end do
          call sqlite3_create_table( db, tblpot, colpot )
         call headout_sqlite_createindex("pot_index",tblpot,"HRU",1)
         call sqlite3_insert_sql_statement(db,tblpot, colpot, stmtpot)
      end if
      end
