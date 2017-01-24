      subroutine headout_sqlite_snu

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create three tables in SQLite database for management data
!!     which is previously in output.snu (output.sol)
!!     The file handle for output.snu is 121

      use parm

      integer :: j,basiccolnum,valuecolnum

      tblsnu = 'snu'

      if(isol == 1) then
          valuecolnum = size(hedsnu)
          basiccolnum = 4
          allocate( colsnu(basiccolnum + valuecolnum) )
          call sqlite3_column_props(colsnu(1),"HRU",SQLITE_INT)
          call sqlite3_column_props(colsnu(2),"YR",SQLITE_INT)
          call sqlite3_column_props(colsnu(3),"MO",SQLITE_INT)
          call sqlite3_column_props(colsnu(4),"DA",SQLITE_INT)
          do j=1,valuecolnum
            call sqlite3_column_props( colsnu(basiccolnum + j),
     &                                          hedsnu(j), SQLITE_REAL)
          end do

          call sqlite3_create_table( db, tblsnu, colsnu )
          call headout_sqlite_createindex( "snu_index",tblsnu,
     &                                            "HRU,YR,MO,DA",0)
          call sqlite3_insert_sql_statement(db,tblsnu, colsnu, stmtsnu)
      end if

      end
