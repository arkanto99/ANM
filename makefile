gf= gfortran -Wall -fcheck=all

MAIN= ejecfort

SRCS= probas.f90 Sarrus.f90

OBJS=$(SRCS:.f95=.o)

$(MAIN): $(OBJS)
	$(gf) -o $(MAIN) $(OBJS) 

%.o: %.f95 
	$(gf) -c $(SRCS)


cleanall:clean
	rm -f $(MAIN)
clean:
	rm -f *.o	

segmentation: 
	$(gf) -fcheck=all -o $(MAIN) $(SRCS)
