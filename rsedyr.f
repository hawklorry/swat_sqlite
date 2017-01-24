      subroutine rsedyr 

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the yearly reach output to the .sed file

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
          rchyro(58,j) = rchyro(58,j)/Real(idlast)
          !!~ ~ ~ SQLITE ~ ~ ~
          if(ioutput == 1) then
            call sqlite3_set_column( colsed(1), j )
            call sqlite3_set_column( colsed(2), iyr )
            call sqlite3_set_column( colsed(3), rchyro(3,j) )
            call sqlite3_set_column( colsed(4), rchyro(4,j) )
            do ii = 42,58
                call sqlite3_set_column( colsed(ii-37), rchyro(ii,j) )
            end do
            call sqlite3_insert_stmt( db, stmtsed, colsed )
          else
            write (84,5000) j, subgis(j), iyr, rch_dakm(j),
     &       rchyro(3,j), rchyro(4,j),(rchyro(ii,j),ii=42,58)
          end if
          !!~ ~ ~ SQLITE ~ ~ ~
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,20e12.4)
      end
