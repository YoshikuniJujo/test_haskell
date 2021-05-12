#include "foo.h"

Foo sampleFoo = { 123 };

Foo *sample_foo() { return &sampleFoo; }
