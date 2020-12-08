FC=ifort
CFLAGS=-O2

SRCS=$(wildcard *.f)
SRCSa=$(wildcard *.F90)
OBJS=$(SRCS:.f=.o)
OBJSa=$(SRCSa:.F90=.o)
TARGET=main.out

$(TARGET): $(OBJSa) $(OBJS)
	$(FC) $(CFLAGS) $^ -o $@
%.o: %.F
	$(FC) $(CFLAGS) -c $^
clean:
	rm $(OBJS) $(OBJSa) $(TARGET)
