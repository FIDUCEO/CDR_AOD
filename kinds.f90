MODULE kinds
!=====#################################################################
! NAME:
!		kinds.f90
! PURPOSE:
! 		Module with definitions of datatypes
! CALLING SEQUENCE:
!		-
! NEEDS:
!		-
! INCLUDES:
!		-
! REVISION HISTORY:
! 		written by Miriam Kosmale 2018
!=====#################################################################
  Implicit none 
  ! Find integer sizes
  
  ! byte		-128...128									8 Bit
  ! short		-32768...32768								16 Bit
  ! long		-2147483648..2147483647						32 Bit
  ! long64		-9223372036854775808..9223372036854775807	64 Bit
  
  integer, parameter :: p_byte		= selected_int_kind ( 8 )	! byte
  integer, parameter :: p_short		= selected_int_kind ( 16 )	! short
  integer, parameter :: p_long		= selected_int_kind ( 32 ) 	! long
  integer, parameter :: p_long64	= selected_int_kind ( 64 )	! long64
  
  ! real		6 digits after the .
  ! double		15 digits after the .
  integer, parameter :: p_real		= selected_real_kind ( 6 )	! real
  integer, parameter :: sp			= selected_real_kind ( 6 )	! real
  integer, parameter :: p_double	= selected_real_kind ( 15 )	! double
  integer, parameter :: dp			= selected_real_kind ( 15,307 )	! double

  ! the length of a long character
  integer, parameter :: c_len		= 200
!=====#################################################################
END MODULE kinds
