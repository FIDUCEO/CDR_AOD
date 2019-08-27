# make -f Makefile_Fiduceo clean
# make -f Makefile_Fiduceo 
# ./fid_avhrr_aod
# ./fiduceo_read.sh

LIB1 = -I/usr/include/ -lnetcdff -lnetcdf  -llapack -lblas -L/usr/lib64/

# externe libraries einbinden (2 Moeglichkeiten):
#/users/kosm_mi/MK.arbeit/projekte/lib_fortran/toolkit/lib/libspicelib.a
#-L/users/kosm_mi/MK.arbeit/projekte/lib_fortran/toolkit/lib/ -lspicelib

 FF  = gfortran -fopenmp

#OPT    = -g -O3 -mcmodel=large 
#OPT    = -O3 -Wall
#OPT    = -O3 -Wconversion -fcheck=all -fbounds-check 
#OPT    = -O3 -Wall -fcheck=all -fbounds-check 
#OPT    = -O3 -fbounds-check 
#OPT    = -O5 -fast
#OPT    = -O3 -mcmodel=large

OPT    = -O3 -mcmodel=large
#OPT     = -O3


SOURCE1 = \
kinds.f90 \
functions_math.f90 \
functions_array.f90 \
functions_ascii.f90 \
functions_date.f90 \
functions_runtime.f90 \
fiduceo_config.f90 \
fiduceo_data.f90 \
fiduceo_coeffs.f90 \
fiduceo_readcoeffs.f90 \
fiduceo_prepare_darkfields.f90 \
fiduceo_aod_retrieval.f90 \
fiduceo_writeforapollo.f90 \
fiduceo_readfromapollo.f90 \
fiduceo_avhrr_read.f90 \
fiduceo_average.f90 \
fiduceo_write_netcdf.f90 \
fiduceo_write_netcdf2.f90 \
ngeo.f90 \
fiduceo_main.f90


.SUFFIXES:          .o .f .f90


fid_avhrr_aod:
	$(FF) $(SOURCE1) $(OPT) $(LIB1) -g -o fid_avhrr_aod


test:
	$(FF) $(SOURCE1) $(OPT) $(LIB1) -g -o fid_avhrr_aod_test

clean:
	rm -f *.o
	rm -f *.mod
	rm -f fid_avhrr_aod

cleantest:
	rm -f *.o
	rm -f *.mod
	rm -f fid_avhrr_aod_test
