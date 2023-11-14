name=${1%.*}

glib-compile-resources $name.gresource.xml --target=build/$name.gresource.c --generate-source
gcc `pkg-config --cflags gtk4` build/$name.gresource.c $name.c `pkg-config --libs gtk4` -lm -o bin/$name
