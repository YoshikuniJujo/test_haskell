APPNAME = More
TINYOBJ_INCLUDE_PATH = tinyobjloader/

CFLAGS = -std=c++17 -O2 -I$(TINYOBJ_INCLUDE_PATH)
LDFLAGS = -lglfw -lvulkan -ldl -lpthread -lX11 -lXxf86vm -lXrandr -lXi

${APPNAME}: main.cpp
	g++ $(CFLAGS) -o ${APPNAME} main.cpp $(LDFLAGS)

test: ${APPNAME}
	./${APPNAME}

tryStbImage: tryStbImage.cpp
	g++ tryStbImage.cpp -o tryStbImage

tryTinyObjLoader: tryTinyObjLoader.cpp
	g++ -I$(TINYOBJ_INCLUDE_PATH) tryTinyObjLoader.cpp -o tryTinyObjLoader

clean:
	rm -f ${APPNAME}
	rm -f tryStbImage
	rm -f tryTinyObjLoader
