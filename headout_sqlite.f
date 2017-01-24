      subroutine headoutsqlite

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hedb(:)     |NA            |column titles in subbasin output files
!!    hedr(:)     |NA            |column titles in reach output files
!!    hedrsv(:)   |NA            |column titles in reservoir output files
!!    heds(:)     |NA            |column titles in HRU output files
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in 
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ipdvab(:)   |none          |output variable codes for output.sub file
!!    ipdvar(:)   |none          |output variable codes for .rch file
!!    ipdvas(:)   |none          |output variable codes for output.hru file
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotb       |none          |number of output variables printed (output.sub)
!!    itotr       |none          |number of output variables printed (.rch)
!!    itots       |none          |number of output variables printed (output.hru)
!!    msubo       |none          |maximum number of variables written to
!!                               |subbasin output file (output.sub)
!!    mhruo       |none          |maximum number of variables written to 
!!                               |HRU output file (output.hru)
!!    mrcho       |none          |maximum number of variables written to
!!                               |reach output file (.rch)
!!    prog        |NA            |program name and version
!!    title       |NA            |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilen        |none          |width of data columns in output file
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      integer :: stat
      character(len=22) :: rfilename

      datecol_num = 1                    !!yearly
      if(iprint == 1) datecol_num = 3    !!daily
      if(iprint == 0) datecol_num = 2    !!monthly

      !!get result file name based on output interval
      if(iprint == 1) rfilename = "result_627_daily.db3";
      if(iprint == 0) rfilename = "result_627_monthly.db3";
      if(iprint == 2) rfilename = "result_627_yearly.db3";
      write(*,*) rfilename

      !!try to delete first
      open(19820428,file=rfilename,status='old',iostat=stat)
      if(stat .eq. 0) close(19820428,status = 'delete')
      
      !!create the result database
      call sqlite3_open( rfilename, db )
      call sqlite3_do(db,"pragma synchronous = 0")
      call sqlite3_do(db,"pragma journal_mode = memory")
      
      !!if it's not deleted for some reason, clear the database and clean up
      call outprocess("start clear")
      call sqlite3_clear(db)
      call outprocess("end clear")
      
      !!vacuum should be executed after clean up rather than after all data is writen.
      !!it will take lots of time when database is large
      call outprocess("vacuum")
      call sqlite3_do( db, "VACUUM")  !!collect all unnecessary space

      !!create tables
      call header_sqlite
      call outprocess("start create tables")
      call headout_sqlite_rch
      call headout_sqlite_hru
      call headout_sqlite_sub
      call headout_sqlite_rsv
      call headout_sqlite_wtr
      call headout_sqlite_pot
      call headout_sqlite_sed
      call headout_sqlite_wshd
      call headout_sqlite_mgt
      call headout_sqlite_snu
      call headout_sqlite_swr
      call headout_sqlite_deg
      call outprocess("end create tables")
      call sqlite3_begin( db )
      call sqlite_writeinfo

!! write headings to pesticide output file (output.pst)
      if (iprp /= 0) then
        write (30,1000)prog, values(2), values(3), values(1), values(5),
     &                values(6), values(7)
        write (30,1010) title
        write (30,3000)
        write (30,3001) (npno(j),npno(j), j = 1, npmx)
        write (30,3002) (pname(npno(j)),pname(npno(j)), j = 1, npmx)
        write (30,3003) (("SOLUBLE mg       SORBED mg"), j = 1, npmx)
      end if
!! Jaehak subdaily bmp output header
!bmp-sedfil.out
      write(77778,'(a21)') 'SED-FIL Basins output'                      
      write(77778,'(a170)') '------------------------------   ----------
     &------------ Sedimentation Pond --------------------------   -----
     &----------------------- Sand Filter ------------------------------
     &' 
      write(77778,'(5a6,30a12)') 'year', 'day','time','sub','SFnum',
     & 'inflw(m3)','outflw(m3)','bypass(m3)','sedin(kg)','sedout(kg)',
     & 'sbypass(kg)','inflw(m3)','outflw(m3)','bypass(m3)','sedin(kg)',
     & 'sedout(kg)','sbypass(kg)'

!bmp-ri.out
      write(77779,'(a21)') 'Retention-Irrigation output'                
      write(77779,'(5a6,30a12)') 'year', 'day','time','sub','RInum',
     & 'inflw(m3)','qbypass(m3)','pmpflw(m3)','sedin(kg)','sbypass(kg)',
     & 'pmpsed(kg)'

      return
 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))
 1020 format (//'LULC  HRU       GIS  SUB  MGT  MON','   AREAkm2',      
     & 78(a10))
 !    * 76(a10),"    GISnum")
 1021  format (//'LULC  HRU       GIS  SUB  MGT MO DA   YR',            
     &'   AREAkm2', 78(a10))	   
 1030 format (//6x,' SUB      GIS  MON   AREAkm2',22(a10))
 1040 format (//7x,'RCH      GIS   MON     AREAkm2',56a12)
 1041 format (//7x,'RCH      GIS   DAY   DET     AREAkm2',45a12)    
 1050 format (//6x,'     RES  MON',41a12)
 1060 format (//6x,'RCH GIS  MON',26a12)
 2000 format (a12,12x,i4,4x,i4)
 3000 format ("Pesticide loadings to main channel by HRU",/)
 3001 format ("Pesticide #",250(18x,i3,1x))
 3002 format ("Pesticide name:      ",250(a16,1x))
 3003 format (4x,'GISnum YEAR MON',7x,125(a26,8x))
      end
