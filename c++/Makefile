CXX=g++
CXXFLAGS=-pthread
INCLUDE=-I.
LIB=

I_PROTOBUF=-I/usr/local/include
L_PROTOBUF=-Wl,-rpath,/usr/local/lib -L/usr/local/lib

BUILD=./build
OBJ=$(BUILD)/obj
BIN=$(BUILD)/bin

all: main
	$(CXX) -o $(BIN)/app \
		$(OBJ)/main.o \
			$(L_PROTOBUF) -lprotobuf -pthread -lpthread

main: mkdir_obj protobuf
	$(CXX) $(CXXFLAGS) $(INCLUDE) $(I_PROTOBUF) -c main.cpp -o $(OBJ)/main.o


protobuf:
	protoc --cpp_out=. message.proto

mkdir_build:
	mkdir -p $(BUILD)
	
mkdir_obj:
	mkdir -p $(OBJ)
  
mkdir_bin:
	mkdir -p $(BIN)

