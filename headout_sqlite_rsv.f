      subroutine headout_sqlite_rsv

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table rsv in SQLite database for reservoirs
!!     The file handle for output.rsv is 8

      use parm

      integer :: j,rsvbasiccolnum,rsvvaluecolnum

      tblrsv = 'rsv'

      !!only create this table when reservoirs exist
      if(nres > 0) then

      rsvvaluecolnum = 41
      rsvbasiccolnum = 1 + datecol_num
      allocate( colrsv(rsvbasiccolnum + rsvvaluecolnum) )

      call sqlite3_column_props( colrsv(1), "RES", SQLITE_INT)
      call headout_sqlite_adddate(colrsv,
     &                          rsvbasiccolnum + rsvvaluecolnum,2)

      do j = 1, rsvvaluecolnum
         call sqlite3_column_props(colrsv(rsvbasiccolnum+j),hedrsv(j),
     &                                                      SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblrsv, colrsv )
      call headout_sqlite_createindex("rsv_index",tblrsv,"RES",1)
      call sqlite3_insert_sql_statement(db,tblrsv, colrsv, stmtrsv)
      end if
      end
