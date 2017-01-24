      subroutine headout_sqlite_wshd

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create three tables in SQLite database for watershed summary
!!     which is previously in output.std
!!     The file handle for output.std is 26

      use parm

      integer :: j,basiccolnum,valuecolnum

      tblwshd_dy = 'watershed_daily'
      tblwshd_mn = 'watershed_monthly'
      tblwshd_yr = 'watershed_yearly'

      valuecolnum = size(hedwshd)

      !!daily table
      basiccolnum = 3
      allocate( colwshd_dy(basiccolnum + valuecolnum) )
      call sqlite3_column_props(colwshd_dy(1),"YR",SQLITE_INT)
      call sqlite3_column_props(colwshd_dy(2),"MO",SQLITE_INT)
      call sqlite3_column_props(colwshd_dy(3),"DA",SQLITE_INT)
      do j=1,valuecolnum
        call sqlite3_column_props( colwshd_dy(basiccolnum + j),
     &                                          hedwshd(j), SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblwshd_dy, colwshd_dy )
      call headout_sqlite_createindex("watershed_daily_index",
     &                                       tblwshd_dy,"YR,MO,DA",0)

      !!monthly table
      basiccolnum = 2
      allocate( colwshd_mn(basiccolnum + valuecolnum) )
      call sqlite3_column_props(colwshd_mn(1),"YR",SQLITE_INT)
      call sqlite3_column_props(colwshd_mn(2),"MO",SQLITE_INT)
      do j=1,valuecolnum
        call sqlite3_column_props( colwshd_mn(basiccolnum + j),
     &                                          hedwshd(j), SQLITE_REAL)
      end do
      call sqlite3_create_table( db, tblwshd_mn, colwshd_mn )
      call headout_sqlite_createindex("watershed_monthly_index",
     &                                            tblwshd_mn,"YR,MO",0)

      !!yearly table
      basiccolnum = 1
      allocate( colwshd_yr(basiccolnum + valuecolnum) )
      call sqlite3_column_props(colwshd_yr(1),"YR",SQLITE_INT)
      do j=1,valuecolnum
        call sqlite3_column_props( colwshd_yr(basiccolnum + j),
     &                                          hedwshd(j), SQLITE_REAL)
      end do
      call sqlite3_create_table(db,tblwshd_yr, colwshd_yr)
      call headout_sqlite_createindex("watershed_yearly_index",
     &                                              tblwshd_yr,"YR",0)

      end
