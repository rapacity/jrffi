CC       := gcc
INCLUDES := -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux
LIBS     := -L$(JAVA_HOME)/jre/lib/$(ARCH) -ljava 

all:
	$(CC) jrffi.c -fPIC $(INCLUDES) $(LIBS) -shared -o jrffi.so
	raco make *.rkt private/*.rkt

clean:
	rm -rf compiled tests/compiled jrffi.so
