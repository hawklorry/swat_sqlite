      subroutine headout_sqlite_createindex(indexname,tblname,indexcols,
     &                                          needadddate)

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     create index for given table on given columns
!!     The time columns would be added automatically

      use parm

      character(len=*), intent(in)           :: indexname
      character(len=*), intent(in)           :: tblname
      character(len=*), intent(in)           :: indexcols
      integer, intent(in)                    :: needadddate

      character(len=10+len(indexcols))       :: indexs

      !!get all index columns
      if(needadddate == 1) then
          if(iprint == 2) write(indexs,*) indexcols,',','YR'
          if(iprint == 1) write(indexs,*) indexcols,',','YR,MO,DA'
          if(iprint == 0) write(indexs,*) indexcols,',','YR,MO'
      else
          write(indexs,*) indexcols
      end if

      !!create the index if not existing
      sq_indexnum = sq_indexnum + 1
      write(sq_tablename(sq_indexnum),*) tblname
      write(sq_indexname(sq_indexnum),*) indexname
      write(sq_indexs(sq_indexnum),*) indexs

      end subroutine headout_sqlite_createindex
