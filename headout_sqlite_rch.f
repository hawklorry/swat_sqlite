      subroutine headout_sqlite_rch

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table rch in SQLite database
!!     The file handle for output.rch is 7
      use parm

      integer :: j
      integer :: colrchnum

      !table name
      tblrch = 'rch'

      !!The number of common columns
      tblrch_num = 1 + datecol_num

      !!get number of columns of reach
      colrchnum = 0
      if (ipdvar(1) > 0) then
        colrchnum = tblrch_num + itotr
      else
        colrchnum = tblrch_num + mrcho
      end if

      !!create table rch
      allocate( colrch(colrchnum) )
      call sqlite3_column_props( colrch(1), "RCH", SQLITE_INT)
      call headout_sqlite_adddate(colrch,colrchnum,2)

      if (ipdvar(1) > 0) then
        do j = 1, itotr
           call sqlite3_column_props(colrch(tblrch_num+j),
     &                                  hedr(ipdvar(j)), SQLITE_REAL)
        end do
      else
        do j = 1, mrcho
            call sqlite3_column_props(colrch(tblrch_num+j), hedr(j),
     &                                                      SQLITE_REAL)
         end do
      end if
      call sqlite3_create_table( db, tblrch, colrch )
      call headout_sqlite_createindex("rch_index",tblrch,"RCH",1)
      call sqlite3_insert_sql_statement(db,tblrch, colrch, stmtrch)  
      end
