      subroutine soil_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, l
      real :: solp_t, solno3_t, solorgn_t, solorgp_t

      do j = 1,nhru
        solp_t = 0.
	  solno3_t = 0.
	  solorgn_t = 0.
	  solorgp_t = 0.
         do l = 1,sol_nly(j)
           solp_t = solp_t + sol_solp(l,j)
           solno3_t = solno3_t + sol_no3(l,j)
	     !if (cswat == 0) then
		   !	solorgn_t = solorgn_t + sol_orgn(l,j)
	     !else
		   !	solorgn_t = solorgn_t + sol_n(l,j)
		   !end if
		   
		   !!By Zhang
		   !!============
	     if (cswat == 0) then
			solorgn_t = solorgn_t + sol_orgn(l,j)
	     end if
	     if (cswat == 1) then
			solorgn_t = solorgn_t + sol_n(l,j)
		   end if		   
		   if (cswat ==2) then
		    solorgn_t = solorgn_t + sol_HSN(l,j) + sol_HPN(l,j)
		   end if
		   !!By Zhang
		   !!============		   
		   
           solorgp_t = solorgp_t + sol_orgp(l,j)
         end do
         !!~ ~ ~ SQLite ~ ~ ~
         if(ioutput == 1) then
            !!save to SQLite database
            call sqlite3_set_column( colsnu(1), j )
            call sqlite3_set_column( colsnu(2), iyr )
            call sqlite3_set_column( colsnu(3), i_mo )
            call sqlite3_set_column( colsnu(4), icl(iida) )
            call sqlite3_set_column( colsnu(5), sol_rsd(1,j) )
            call sqlite3_set_column( colsnu(6), solp_t )
            call sqlite3_set_column( colsnu(7), solno3_t )
            call sqlite3_set_column( colsnu(8), solorgn_t )
            call sqlite3_set_column( colsnu(9), solorgp_t )
            call sqlite3_set_column( colsnu(10), cnday(j) )
            call sqlite3_insert_stmt( db, stmtsnu, colsnu )
         else
         write (121,1000) i, subnum(j), hruno(j), sol_rsd(1,j), solp_t, 
     &    solno3_t, solorgn_t, solorgp_t, cnday(j)
         end if
         !!~ ~ ~ SQLite ~ ~ ~
      end do
      
      return
 1000 format ('SNU   ',i4,1x,a5,a4,1x,6f10.2)
      end
