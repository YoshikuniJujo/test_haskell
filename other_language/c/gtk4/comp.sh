#!/bin/sh
gcc `pkg-config --cflags gtk4` $1 `pkg-config --libs gtk4` -o bin/${1%.*}
