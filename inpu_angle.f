c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine inpu_angle (hm,azg,fhmess)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c-function:
c --------
c this module reads all angle parameters
c
c-calls from: 
c -----------
c inpu
c
c-subroutines:
c -----------
c
c-date: creation: 7-feb-1995 tp1,last update: 19-jul-1996
c ---- 
c writer: dr. thomas popp, -1382 
c ------
c ============================================================
c
      include './inc/param.h'
         parameter (nmm=nm-1)
         parameter (ind3n=(ind3+1)/2)
      include './inc/system.h'
      include './inc/rte.h'
c
      real*4       azg(ind5),hm(nmm)
c
      common /path/ pfad,npfad,impfad,nimpfad,progname,np
c
      namelist/list5h/hmess                                             07980000
      namelist /list7/nazid,azidg                                       08040000
c                                                                       07630000
c einlesen der winkelfelder von kanal 11 und 12                         07650000
c                                                                       07660000
c *** read angle layers from sos.data(nmu27b) and sos.data(naz19)
c     other muefields for test cases
c                                                                       07970000
      open (unit=11, file=pfad(1:npfad)//'angl/'//fhmess)
      read (11,list5h)                                                  07990000
      close(unit=11)
c                                                                       08030000
      open (unit=12, file=pfad(1:npfad)//'angl/NAZ19')
      read(12,list7)                                                    08050000
      close(unit=12)
c                                                                       08060000
      do 1 ix=1,ind5                                                    08070000
    1    azg(ix)=azidg(ix)                                              08080000
c                                                                       08000000
      do 2 ix=1,nm-1                                                    08010000
    2    hm(ix)=hmess(ix)   
c                                                                       08110000
c *** end inpu_angle ***                                                08120006
      return                                                            08130000
      end                                                               08140000
