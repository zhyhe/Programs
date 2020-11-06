test: test.o
	ifort -o test test.o
test.o:%o:%F90
	ifort -c $<
clean:
	rm test.o test

