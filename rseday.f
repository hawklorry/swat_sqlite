      subroutine rseday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the daily reach output to the .sed file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j

      do j = 1, subtot
          !!~ ~ ~ SQLITE ~ ~ ~
          if(ioutput == 1) then
            !!use calender day format by default
            call sqlite3_set_column( colsed(1), j )
            call sqlite3_set_column( colsed(2), iyr )
            call sqlite3_set_column( colsed(3), i_mo )
            call sqlite3_set_column( colsed(4), icl(iida) )
            call sqlite3_set_column( colsed(5), rchdy(5,j) )
            call sqlite3_set_column( colsed(6), rchdy(6,j) )
            do ii = 43,59
                call sqlite3_set_column( colsed(ii-36), rchdy(ii,j) )
            end do
            call sqlite3_insert_stmt( db, stmtsed, colsed )
          else
            write (84,5000) j, subgis(j), iida, rch_dakm(j),
     &       rchdy(5,j), rchdy(6,j),(rchdy(ii,j),ii=43,59)
          end if
          !!~ ~ ~ SQLITE ~ ~ ~
	end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,20e12.4)
      end
