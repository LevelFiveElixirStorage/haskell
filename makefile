TARGET=conway
.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(TARGET).hs
	ghc $(TARGET).hs

run: $(TARGET)
	./$(TARGET) < seeds/1

clean:
	rm *.hi *.o $(TARGET)
