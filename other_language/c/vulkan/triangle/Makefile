APPNAME = Triangle
CFLAGS = -std=c++17 -O2
LDFLAGS = -lglfw -lvulkan -ldl -lpthread -lX11 -lXxf86vm -lXrandr -lXi

${APPNAME}: main.cpp
	g++ $(CFLAGS) -o ${APPNAME} main.cpp $(LDFLAGS)

test: ${APPNAME}
	./${APPNAME}

clean:
	rm -f ${APPNAME}
