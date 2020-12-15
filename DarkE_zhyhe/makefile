OBJS=gw170817.o constants.o fiducial.o Fisher.o imsl.o
MODS=constants.mod fiducial.mod Fisher.mod imsl.mod
DE: $(OBJS) $(MODS)
	ifort -o DE $(OBJS)

gw170817.o: gw170817.F90 Fisher.mod
	ifort -c gw170817.F90
imsl.o:
	ifort -c imsl.F90
constants.o constants.mod: constants.F90
	ifort -c constants.F90
fiducial.o fiducial.mod: fiducial.F90 constants.mod
	ifort -c fiducial.F90
Fisher.o Fisher.mod: Fisher.F90 fiducial.mod
	ifort -c Fisher.F90
.PHONY: clean
clean:
	rm *.o *.mod

