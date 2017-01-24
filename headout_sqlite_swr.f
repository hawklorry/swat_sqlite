      subroutine headout_sqlite_swr

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create three tables in SQLite database for management data
!!     which is previously in output.swr (output.sol)
!!     The file handle for output.swr is 129

      use parm

      integer :: j,basiccolnum,valuecolnum

      tblswr = 'swr'

      if(isto > 0) then
          valuecolnum = size(hedswr)
          basiccolnum = 4
          allocate( colswr(basiccolnum + valuecolnum) )
          call sqlite3_column_props(colswr(1),"HRU",SQLITE_INT)
          call sqlite3_column_props(colswr(2),"YR",SQLITE_INT)
          call sqlite3_column_props(colswr(3),"MO",SQLITE_INT)
          call sqlite3_column_props(colswr(4),"DA",SQLITE_INT)
          do j=1,valuecolnum
            call sqlite3_column_props( colswr(basiccolnum + j),
     &                                          hedswr(j), SQLITE_REAL)
          end do

          call sqlite3_create_table( db, tblswr, colswr )
          call headout_sqlite_createindex( "swr_index",tblswr,
     &                                              "HRU,YR,MO,DA",0)
          call sqlite3_insert_sql_statement(db,tblswr, colswr, stmtswr)
      end if

      end
