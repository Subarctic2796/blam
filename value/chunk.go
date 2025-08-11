package value

type LineInfo struct {
	offset, line int
}

type Chunk struct {
	Code      []byte
	Constants []Value
	Lines     []LineInfo
}

func NewChunk() *Chunk {
	return &Chunk{
		make([]byte, 0),
		make([]Value, 0),
		make([]LineInfo, 0),
	}
}

func (c *Chunk) WriteChunk(b byte, line int) {
	c.Code = append(c.Code, b)

	// rle encode line info
	if len(c.Lines) > 0 && c.Lines[len(c.Lines)-1].line == line {
		return
	}

	c.Lines = append(c.Lines, LineInfo{len(c.Code) - 1, line})
}

func (c *Chunk) AddConst(value Value) int {
	c.Constants = append(c.Constants, value)
	return len(c.Constants) - 1
}

func (c *Chunk) GetLine(inst int) int {
	start, end, length := 0, len(c.Lines)-1, len(c.Lines)-1

	for {
		mid := (start + end) / 2
		line := c.Lines[mid]
		if inst < line.offset {
			end = mid - 1
		} else if mid == length || inst < c.Lines[mid+1].offset {
			return line.line
		} else {
			start = mid + 1
		}
	}
}
