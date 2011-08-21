CC       := gcc
INCLUDES := -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux
LIBS     := -L$(JAVA_HOME)/jre/lib/$(ARCH) -ljava 

all:
	$(CC) jrffi.c -fPIC $(INCLUDES) $(LIBS) -shared -o jrffi.so

clean:
	rm jrffi.so
