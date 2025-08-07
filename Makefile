.PHONY: clean gen build run debug
BIN = blam

build: clean gen
	go build -o $(BIN)

gen:
	go generate ./...

run:
	@go run .

debug:
	go build -tags=debug -o $(BIN)

clean:
	$(RM) $(BIN) ./{vm,token}/*_string.go
