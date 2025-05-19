executables = xmatrix_inverse_gfort.exe xequicorr_gfort.exe xequicorr_port_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o linear_solve.o xmatrix_inverse.o util.o xequicorr.o xequicorr_port.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xmatrix_inverse_gfort.exe: kind.o linear_solve.o xmatrix_inverse.o
	$(FC) -o xmatrix_inverse_gfort.exe kind.o linear_solve.o xmatrix_inverse.o $(FFLAGS)

xequicorr_gfort.exe: kind.o linear_solve.o util.o xequicorr.o
	$(FC) -o xequicorr_gfort.exe kind.o linear_solve.o util.o xequicorr.o $(FFLAGS)

xequicorr_port_gfort.exe: kind.o linear_solve.o util.o xequicorr_port.o
	$(FC) -o xequicorr_port_gfort.exe kind.o linear_solve.o util.o xequicorr_port.o $(FFLAGS)

run: $(executables)
	./xmatrix_inverse_gfort.exe
	./xequicorr_gfort.exe
	./xequicorr_port_gfort.exe

clean:
	rm -f $(executables) $(obj)

