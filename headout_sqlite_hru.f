      subroutine headout_sqlite_hru

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table hru in SQLite database
!!     The file handle for output.hru is 28

      use parm

      integer :: j
      integer :: colhrunum

      tblhru = 'hru'

      !!hru table
      !!The number of common columns
      tblhru_num = 3 + datecol_num

      !!get number of columns of hru
      colhrunum = 0
      if (ipdvas(1) > 0) then
        colhrunum = tblhru_num + itots
      else
        colhrunum = tblhru_num + mhruo
      end if

      !!create table hru
      allocate( colhru(colhrunum) )
      call sqlite3_column_props( colhru(1), "HRU", SQLITE_INT)
      call sqlite3_column_props( colhru(2), "LULC", SQLITE_CHAR,4)
      call sqlite3_column_props( colhru(3), "MGT", SQLITE_INT)
      call headout_sqlite_adddate(colhru,colhrunum,4)

      if (ipdvas(1) > 0) then
        do j = 1, itots
         call sqlite3_column_props(colhru(tblhru_num+j),heds(ipdvas(j)),
     &                                                      SQLITE_REAL)
        end do
      else
        do j = 1, mhruo
            call sqlite3_column_props(colhru(tblhru_num+j), heds(j),
     &                                                      SQLITE_REAL)
        end do
      end if
      call sqlite3_create_table( db, tblhru, colhru )
      call headout_sqlite_createindex("hru_index",tblhru,"HRU",1)
      call sqlite3_insert_sql_statement(db,tblhru, colhru, stmthru)
      end
