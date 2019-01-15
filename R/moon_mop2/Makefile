CC    = gcc
CPP   = g++

TARGET  = ./moon_mop
SRCDIR  = . \

HDRDIR     = $(SRCDIR) 
ODIR       = objs

FLAGS       = -O3 -std=c++11

#Include direcotory flags
FLAGS += $(patsubst %,-I%, $(HDRDIR))

# Defined symbols flags, -D options
#VARIABLES   = MINIMIZE_ALL
FLAGS += $(patsubst %,-D%, $(VARIABLES))

# Libraries flags for linking, -l options
#LIBS        = 
#LIBSFLAGS = $(patsubst %,-l%, $(LIBS))

# Libraries flags for linking, -L options
#LIBSDIR     =
#LIBSDIRFLAGS = $(patsubst %,-L%, $(LIBSDIR))

vpath %.cpp $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.h $(HDRDIR)

# Get a list of all the *.c and *.cpp (path included)
SOURCES = $(foreach dir,$(SRCDIR),$(wildcard $(dir)/*.cpp $(dir)/*.c))

# Remove the path so that only the names of the sources are kept.
SOURCESNAMES=$(notdir $(SOURCES))

# To get the list of generated objects change the file extension to *.o and append the directory of the object files.
OBJECTS    = $(patsubst %, $(ODIR)/%.o, $(basename $(SOURCESNAMES)))

all: $(TARGET)

$(TARGET): $(OBJECTS)
	@echo ' '
	@echo 'Building target: $@'
	@echo ' '
	$(CPP) $(LIBSFLAGS) $(LIBSDIRFLAGS) $(FLAGS) -o $@ $^
	@echo ' '
	@echo 'Finished building target: $@'
	@echo ' '

$(ODIR)/%.o: %.cpp
	$(CPP) $(VARFLAGS) $(IFLAGS) $(FLAGS) -c -o $@ $<

$(ODIR)/%.o: %.c
	$(CC) $(VARFLAGS) $(IFLAGS) $(FLAGS) -c -o $@ $<

clean:
	-rm -f $(OBJECTS) $(TARGET)

print:
	-@echo $(IFLAGS)

.PHONY : all clean print
