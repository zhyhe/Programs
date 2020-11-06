test: test.o
	ifort -o test test.o
test.o: test.F90
	ifort -c test.F90
clean:
	rm test.o test
