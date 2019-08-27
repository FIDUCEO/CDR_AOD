      subroutine ngeo (thetb,phib,sonhoe,isun,ibmue,ibphi)
!=====#################################################################
! NAME:
!		fiduceo_angles.f90
! PURPOSE:
!		select angle field pointers
! INCLUDES:
!		---
! NEEDS:
!		kinds.f90
!		functions_ascii.f90
!		functions_runtime.f90
! AUTHOR:
!		written by Thomas Popp, 10.01.2019
! PROJECT:
!		FIDUCEO
!=====#################################################################

      USE kinds
      USE fiduceo_config
      USE fiduceo_coeffs

IMPLICIT NONE

      integer*2    i,isun,ibmue,ibphi
      real*4       deltasun,deltamue,deltaphi,delta
      real*4       sonhoe,thetb,phib
                                          
      deltasun=20.                                                      
      do 1 i=1,nmm                                                      
         delta=abs(sonhoe-hmess(i))                                     
         if (delta.lt.deltasun) then                                    
            isun=i                                                      
            deltasun=delta                                              
         endif                                                          
    1 continue                                                          
                                 
    
      deltamue=20.                                                      
      do 2 i=1,nmm                                                      
         delta=abs(90.-thetb-hmess(i))                                  
         if (delta.lt.deltamue) then                                    
            ibmue=i                                                     
            deltamue=delta                                              
         endif                                                          
    2 continue                                                          
     
      deltaphi=20.                                                      
      do 3 i=1,ind5                                                    
         delta=abs(phib-azidg(i))                                       
         if (delta.lt.deltaphi) then                                    
            ibphi=i                                                     
            deltaphi=delta                                              
         endif
    3 continue
     
      return
      end
