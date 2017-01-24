      subroutine headout_sqlite_sub

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create table sub in SQLite database for subbasin
!!     The file handle for output.sub is 31

      use parm

      integer :: j,colsubnum

      !table name
      tblsub = 'sub'

      !!subbasin table
      !!The number of common columns
      tblsub_num = 1 + datecol_num

      !!get number of columns of sub
      colsubnum = 0
      if (ipdvab(1) > 0) then
        colsubnum = tblsub_num + itotb
      else
        colsubnum = tblsub_num + msubo
      end if

      !!create table sub
      allocate( colsub(colsubnum) )
      call sqlite3_column_props( colsub(1), "SUB", SQLITE_INT)
      call headout_sqlite_adddate(colsub,colsubnum,2)

      if (ipdvab(1) > 0) then
        do j = 1, itotb
         call sqlite3_column_props(colsub(tblsub_num+j),hedb(ipdvab(j)),
     &                                                      SQLITE_REAL)
        end do
      else
        do j = 1, msubo
            call sqlite3_column_props(colsub(tblsub_num+j), hedb(j),
     &                                                      SQLITE_REAL)
        end do
      end if
      call sqlite3_create_table( db, tblsub, colsub )
      call headout_sqlite_createindex("sub_index",tblsub,"SUB",1)
      call sqlite3_insert_sql_statement(db,tblsub, colsub, stmtsub)
      end
