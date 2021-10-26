APPNAME = More
TINYOBJ_INCLUDE_PATH = tinyobjloader/

CFLAGS = -std=c++17 -O3 -I$(TINYOBJ_INCLUDE_PATH)
LDFLAGS = -lglfw -lvulkan -ldl -lpthread -lX11 -lXxf86vm -lXrandr -lXi

${APPNAME}: main.cpp
	g++ $(CFLAGS) -o ${APPNAME} main.cpp $(LDFLAGS)

${APPNAME}_tutor: main_tutor.cpp
	g++ $(CFLAGS) -o ${APPNAME}_tutor main_tutor.cpp $(LDFLAGS)

test: ${APPNAME}
	./${APPNAME}

test_tutor: ${APPNAME}_tutor
	./${APPNAME}_tutor

tryStbImage: tryStbImage.cpp
	g++ tryStbImage.cpp -o tryStbImage

tryTinyObjLoader: tryTinyObjLoader.cpp
	g++ -I$(TINYOBJ_INCLUDE_PATH) tryTinyObjLoader.cpp -o tryTinyObjLoader

clean:
	rm -f ${APPNAME}
	rm -f ${APPNAME}_tutor
	rm -f tryStbImage
	rm -f tryTinyObjLoader
