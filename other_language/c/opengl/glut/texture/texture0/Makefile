LDLIBS = -L/usr/X11R6/lib -lglut -lGLU -lGL -lXmu -lXi -lXext -lX11 -lpthread -lm
TARGETS = texture0
OBJECTS = main.o trackball.o

$(TARGETS): $(OBJECTS)
	$(CXX) $(CFLAGS) -o $@ $(OBJECTS) $(LDLIBS)

main.o: trackball.h
trackball.o: trackball.h

clean:
	-rm -f $(TARGETS) *.o *~
