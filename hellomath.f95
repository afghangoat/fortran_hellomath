MODULE constants

    IMPLICIT NONE
    REAL, PARAMETER :: PI = 3.14159265
    REAL, PARAMETER :: E  = 2.71828183
    
END MODULE constants

PROGRAM main
    USE constants
    
    IMPLICIT NONE
    INTERFACE 
    
     FUNCTION v_itanh(n)
        INTEGER, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_itanh
     
     FUNCTION v_isinh(n)
        INTEGER, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_isinh
     
     FUNCTION v_icosh(n)
        INTEGER, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_icosh
     
     FUNCTION v_rtanh(n)
        REAL, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_rtanh
     
     FUNCTION v_rsinh(n)
        REAL, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_rsinh
     
     FUNCTION v_rcosh(n)
        REAL, INTENT(in) :: n
        REAL :: result
     END FUNCTION v_rcosh
     
     FUNCTION transponation(n)
        REAL, INTENT(in) :: n
        REAL :: result
     END FUNCTION transponation
     
    END INTERFACE

    INTEGER :: n
    REAL :: t
    REAL, DIMENSION(5, 5) :: matrix

    ! Initialize variables
    matrix = 0.0  !default value
    matrix(1:1,2:2)=1.0 !col,row

    ! Print outputs
    PRINT *, "This is a fortran implementation of some math functions."
    PRINT *, matrix
    
    PRINT *, "Tanh of: ", n, ": ", t
    
END PROGRAM main

SUBROUTINE ADD_ONE()

  IMPLICIT NONE
  PRINT *, "Hello World"
  
END SUBROUTINE

!#if 0
!recursive function facHelper(n, acc) result(returner)
!  implicit none
!  integer, intent(in) :: n, acc
!  integer :: returner
!  if (n <= 1) then
!    returner = acc
!  else
!    returner = facHelper(n - 1, n * acc)
!  endif
!end function facHelper
!
!function factorial(n)
!  implicit none
!  integer, intent(in) :: n
!  integer :: factorial
!  interface 
!     function facHelper(n,acc) 
!        integer, intent(in) :: n, acc
!        integer :: facHelper
!     end function
!  end interface
!  factorial = facHelper(n, 1)
!end function factorial
!#endif

!for integers
FUNCTION v_itanh(n) RESULT(res)
  USE constants
  
  
  INTEGER, INTENT(in) :: n
  REAL :: res
  
  res= (E**n - E**(-n))/(E**n + E**(-n))
  
  
END FUNCTION v_itanh

FUNCTION v_isinh(n) RESULT(res)
  USE constants
  
  
  INTEGER, INTENT(in) :: n
  REAL :: res
  
  res= (E**n - E**(-n))/2.0
  
  
END FUNCTION v_isinh

FUNCTION v_icosh(n) RESULT(res)
  USE constants
  
  
  INTEGER, INTENT(in) :: n
  REAL :: res
  
  res= (E**n + E**(-n))/2.0
  
  
END FUNCTION v_icosh

!for reals
FUNCTION v_rtanh(n) RESULT(res)
  USE constants
  
  
  REAL, INTENT(in) :: n
  REAL :: res
  
  res= (E**n - E**(-n))/(E**n + E**(-n))
  
  
END FUNCTION v_rtanh

FUNCTION v_rsinh(n) RESULT(res)
  USE constants
  
  
  REAL, INTENT(in) :: n
  REAL :: res
  
  res= (E**n - E**(-n))/2.0
  
  
END FUNCTION v_rsinh

FUNCTION v_rcosh(n) RESULT(res)
  USE constants
  
  
  REAL, INTENT(in) :: n
  REAL :: res
  
  res= (E**n + E**(-n))/2.0
  
  
END FUNCTION v_rcosh
