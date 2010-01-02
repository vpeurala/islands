default: build

BUILD = build

$(BUILD):
	mkdir $(BUILD)
	ghc -odir $(BUILD) -c src/Opcodes.hs 

build: $(BUILD)

clean: 
	rm -rf $(BUILD)
