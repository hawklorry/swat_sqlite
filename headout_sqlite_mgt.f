      subroutine headout_sqlite_mgt

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create three tables in SQLite database for management data
!!     which is previously in output.mgt
!!     The file handle for output.mgt is 143

      use parm

      integer :: j,basiccolnum,valuecolnum

      tblmgt = 'mgt'

      if(imgt == 1) then
          valuecolnum = size(hedmgt)
          basiccolnum = 6
          allocate( colmgt(basiccolnum + valuecolnum + 2) )
          call sqlite3_column_props(colmgt(1),"HRU",SQLITE_INT)
          call sqlite3_column_props(colmgt(2),"YR",SQLITE_INT)
          call sqlite3_column_props(colmgt(3),"MO",SQLITE_INT)
          call sqlite3_column_props(colmgt(4),"DA",SQLITE_INT)
          call sqlite3_column_props(colmgt(5),"CROP_FERT_PEST",
     &                                               SQLITE_CHAR,10)
          call sqlite3_column_props(colmgt(6),"OPERATION",
     &                                               SQLITE_CHAR,10)
          do j=1,valuecolnum
            call sqlite3_column_props( colmgt(basiccolnum + j),
     &                                          hedmgt(j), SQLITE_REAL)
          end do
          call sqlite3_column_props(colmgt(basiccolnum +valuecolnum +1),
     &                                               "IRRSC",SQLITE_INT)
          call sqlite3_column_props(colmgt(basiccolnum +valuecolnum +2),
     &                                               "IRRNO",SQLITE_INT)

          call sqlite3_create_table( db, tblmgt, colmgt )
          call headout_sqlite_createindex( "mgt_index",tblmgt,
     &                                             "HRU,YR,MO,DA",0)
          call sqlite3_insert_sql_statement(db,tblmgt, colmgt, stmtmgt)

      end if
      end
