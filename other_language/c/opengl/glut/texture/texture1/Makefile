LDLIBS = -L/usr/X11R6/lib -lglut -lGLU -lGL -lXmu -lXi -lXext -lX11 -lpthread -lm
TARGETS = texture1
OBJECTS = main.o trackball.o box.o

$(TARGETS): $(OBJECTS)
	$(CXX) $(CFLAGS) -o $@ $(OBJECTS) $(LDLIBS)

main.o: trackball.h box.h
trackball.o: trackball.h
box.o: box.h

clean:
	-rm -f $(TARGETS) *.o *~
