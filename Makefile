# enables c++11 on CAEN
PATH := /usr/um/gcc-4.8.2/bin:$(PATH)
LD_LIBRARY_PATH := /usr/um/gcc-4.8.2/lib64
LD_RUN_PATH := /usr/um/gcc-4.8.2/lib64

# designate which compiler to use
CXX             = g++
# list of sources used in project
SOURCES         = $(wildcard *.cpp)
# list of objects used in project
OBJECTS         = $(SOURCES:%.cpp=%.o)
# name of the executable produced by the top level
EXECUTABLE      = exec
# name of the tar ball created for submission
SUBMIT_FILE = submit.tar.gz

#Default Flags
CXXFLAGS = -std=c++11 -Wall -Wextra -pedantic #-Werror

# make release - will compile "all" with $(CXXFLAGS) and the -O3 flag
#                also defines NDEBUG so that asserts will not check
release: CXXFLAGS += -O3 -DNDEBUG
release: all

# make debug - will compile "all" with $(CXXFLAGS) and the -g flag
#              also defines DEBUG so that "#ifdef DEBUG /*...*/ #endif" works
debug: CXXFLAGS += -g3 -DDEBUG
debug: clean all

# make profile - will compile "all" with $(CXXFLAGS) and the -pg flag
profile: CXXFLAGS += -pg
profile: clean all

# highest target; sews together all objects into executable
all: $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(OBJECTS) -o $(EXECUTABLE)


# individual dependencies for objects
#example.o: example.cpp included.h


# rule for creating objects
%.o:
	$(CXX) $(CXXFLAGS) -c $*.cpp

# make clean - remove .o files, executable, tarball
clean:
	rm -f $(OBJECTS) $(EXECUTABLE) $(SUBMIT_FILE)

# Make test - run tests
TESTS = $(wildcard test*.txt)
test: release
#example
#for each testing file, use it as input to the executable
#	@$(foreach test, $(TESTS), ./$(EXECUTABLE) < $(TESTS);)


# make submit.tar.gz - cleans, runs dos2unix, creates tarball
# put things to submit in MY_FILES
MY_FILES=$(wildcard Makefile *.h *.cpp test*.txt)
$(SUBMIT_FILE): $(MY_FILES)
	dos2unix $(MY_FILES)
	tar -vczf $(SUBMIT_FILE) $(MY_FILES)

# shortcut for make submitfile (make submit)
submit: $(SUBMIT_FILE)

# these targets do not create any files
.PHONY: all release debug profile clean submit test
# disable built-in rules
.SUFFIXES:
