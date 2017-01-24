      subroutine sqlite_writeinfo

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes some basic watershed information into SQLite database
!!     these tables are used to avoid duplicated information in regular file output
!!     three tables are created: hru_info, sub_info and rch_info
!!     more information could be added in the future

      use parm


      type(SQLITE_COLUMN), dimension(:), pointer :: col
      character(len=20) :: tbl
      integer :: colnum
      integer :: j

      !!HRU info
      colnum = 7
      tbl = "hru_info"
      allocate( col(colnum))
      call sqlite3_column_props( col(1), "HRU", SQLITE_INT)
      call sqlite3_column_props( col(2), "SEQ", SQLITE_INT)
      call sqlite3_column_props( col(3), "SUB", SQLITE_INT)
      call sqlite3_column_props( col(4), "AREA_KM2", SQLITE_REAL)
      call sqlite3_column_props( col(5), "AREA_FR_SUB", SQLITE_REAL)
      call sqlite3_column_props( col(6), "AREA_FR_WSHD", SQLITE_REAL)
      call sqlite3_column_props( col(7), "SLOPE", SQLITE_REAL)
      call sqlite3_create_table( db, tbl, col )
      call headout_sqlite_createindex("hru_info_index",
     &                                              tbl,"HRU,SEQ,SUB",0)

      do j = 1,nhru
        call sqlite3_set_column( col(1), j )
        call sqlite3_set_column( col(2), hru_seq(j) )
        call sqlite3_set_column( col(3), hru_sub(j) )
        call sqlite3_set_column( col(4), hru_km(j) )
        call sqlite3_set_column( col(5), hru_fr(j) )
        call sqlite3_set_column( col(6), hru_dafr(j) )
        call sqlite3_set_column( col(7), hru_slp(j) )
        call sqlite3_insert(db,tbl,col)
      end do
      deallocate(col)

      !!SUB info
      colnum = 3
      tbl = "sub_info"
      allocate( col(colnum))
      call sqlite3_column_props( col(1), "SUB", SQLITE_INT)
      call sqlite3_column_props( col(2), "AREA_KM2", SQLITE_REAL)
      call sqlite3_column_props( col(3), "AREA_FR_WSHD", SQLITE_REAL)
      call sqlite3_create_table( db, tbl, col )
      call headout_sqlite_createindex("sub_info_index",
     &                                                     tbl,"SUB",0)

      do j = 1,subtot
        call sqlite3_set_column( col(1), j )
        call sqlite3_set_column( col(2), sub_km(j) )
        call sqlite3_set_column( col(3), sub_fr(j) )
        call sqlite3_insert(db,tbl,col)
      end do
      deallocate(col)

      !!RCH info
      colnum = 2
      tbl = "rch_info"
      allocate( col(colnum))
      call sqlite3_column_props( col(1), "RCH", SQLITE_INT)
      call sqlite3_column_props( col(2), "AREA_KM2", SQLITE_REAL)
      call sqlite3_create_table( db, tbl, col )
      call headout_sqlite_createindex("rch_info_index",
     &                                                     tbl,"RCH",0)

      do j = 1,subtot
        call sqlite3_set_column( col(1), j )
        call sqlite3_set_column( col(2), rch_dakm(j) )
        call sqlite3_insert(db,tbl,col)
      end do
      deallocate(col)

      !!RSV info
      if(nres > 0) then
          colnum = 1
          tbl = "rsv_info"
          allocate( col(colnum))
          call sqlite3_column_props( col(1), "RES", SQLITE_INT)
          call sqlite3_create_table( db, tbl, col )
          call headout_sqlite_createindex("rsv_info_index",
     &                                                     tbl,"RES",0)

          do j = 1,nres
            call sqlite3_set_column( col(1), res_sub(j) )
            call sqlite3_insert(db,tbl,col)
          end do
          deallocate(col)
      end if

      end
