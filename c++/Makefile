CXX=g++ -std=c++14
CXXFLAGS=-pthread
INCLUDE=-I.
LIB=

I_PROTOBUF=-I/usr/local/include
L_PROTOBUF=-Wl,-rpath,/usr/local/lib -L/usr/local/lib

BUILD=./build
OBJ=$(BUILD)/obj
BIN=$(BUILD)/bin

all: mkdir_bin message echo read write main
	$(CXX) -o $(BIN)/app \
		$(OBJ)/main.o \
		$(OBJ)/message.o \
		$(OBJ)/message.pb.o \
		$(OBJ)/echo.o \
		$(OBJ)/read.o \
		$(OBJ)/write.o \
			$(L_PROTOBUF) -lprotobuf -pthread -lpthread

main: mkdir_obj protobuf
	$(CXX) $(CXXFLAGS) $(INCLUDE) $(I_PROTOBUF) -c main.cpp -o $(OBJ)/main.o

message: mkdir_obj protobuf
	$(CXX) $(CXXFLAGS) $(INCLUDE) $(I_PROTOBUF) -c message.cpp -o $(OBJ)/message.o

echo: mkdir_obj
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c message/echo.cpp -o $(OBJ)/echo.o

read: mkdir_obj
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c message/read.cpp -o $(OBJ)/read.o

write: mkdir_obj
	$(CXX) $(CXXFLAGS) $(INCLUDE) -c message/write.cpp -o $(OBJ)/write.o

protobuf:
	protoc --cpp_out=. message.proto
	$(CXX) $(CXXFLAGS) $(INCLUDE) $(I_PROTOBUF) -c message.pb.cc -o $(OBJ)/message.pb.o

mkdir_build:
	mkdir -p $(BUILD)
	
mkdir_obj:
	mkdir -p $(OBJ)
  
mkdir_bin:
	mkdir -p $(BIN)

