      subroutine sqlite_finalize

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes some basic watershed information into SQLite database
!!     these tables are used to avoid duplicated information in regular file output
!!     three tables are created: hru_info, sub_info and rch_info
!!     more information could be added in the future

      use parm


      if(ioutput == 1) then
        !!commit inserts
        call outprocess("sqlite commit")
        call sqlite3_commit( db )
        
        !!release sql statements
        call sqlite3_finalize(stmtrch)
        call sqlite3_finalize(stmthru)
        call sqlite3_finalize(stmtsub)
        call sqlite3_finalize(stmtrsv)
        call sqlite3_finalize(stmtwtr)
        call sqlite3_finalize(stmtsed)
        call sqlite3_finalize(stmtpot)
        call sqlite3_finalize(stmtmgt)
        call sqlite3_finalize(stmtsnu)
        call sqlite3_finalize(stmtswr)
        
        !create index
        call outprocess("create index")
        do i=1,sq_indexnum
            call sqlite3_create_index(db,sq_indexname(i),
     &                                sq_tablename(i),sq_indexs(i))
        end do
        
        !clean up
        !call outprocess("analyze")
        !call sqlite3_do( db, "ANALYZE") !!update talbe and indexes statistis for better query performance
        !call outprocess("vacuum")
        !call sqlite3_do( db, "VACUUM")  !!collect all unnecessary space
        
        !!close
        call outprocess("sqlite close")
        call sqlite3_close( db)
        call outprocess("sqlite done")
      end if

      end
