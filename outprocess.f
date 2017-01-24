      !!output time and the subroutine name
      !!just used to test which subroutine need more time
      subroutine outprocess(pname)
          implicit none

          character(len=*):: pname
          integer, dimension(8) :: values
          
          
          call date_and_time(VALUES=values)
          write ( *, 1004 )  values(1),values(2),values(3),values(5),
     & values(6),values(7),values(8), pname
          return
 1004 format ( '--- ', i4.4, '-', i2.2, '-', i2.2, ' ',
     &         i2.2, ':', i2.2, ':', i2.2, ' ', i3.3, ' process:', a )
      end
