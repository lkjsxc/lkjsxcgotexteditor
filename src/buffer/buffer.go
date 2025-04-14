package buffer

type Buffer struct {
	lines []string
}

type Result int

const (
	ResultOk Result = iota
	ResultErr
)

func NewBuffer() *Buffer {
	return &Buffer{
		lines: []string{""}, // Initialize with one empty line
	}
}

func (b *Buffer) InsertChar(r rune, pos Position) Result {
	if pos.Line >= len(b.lines) || pos.Line < 0 {
		return ResultErr // Invalid line number
	}
	line := b.lines[pos.Line]
	if pos.Col > len(line) || pos.Col < 0 {
		return ResultErr // Invalid column number
	}

	b.lines[pos.Line] = line[:pos.Col] + string(r) + line[pos.Col:]
	return ResultOk
}

func (b *Buffer) DeleteChar(pos Position) Result {
	if pos.Line >= len(b.lines) || pos.Line < 0 {
		return ResultErr // Invalid line number
	}
	line := b.lines[pos.Line]
	if pos.Col >= len(line) || pos.Col < 0 {
		return ResultErr // Invalid column number
	}
	if pos.Col == 0 && pos.Line == 0 && len(b.lines) == 1 && len(line) == 0 {
		return ResultErr // Cannot delete in empty buffer
	}
	if pos.Col == 0 && pos.Line > 0 {
		// Delete newline, join lines
		prevLine := b.lines[pos.Line-1]
		currentLine := b.lines[pos.Line]
		b.lines[pos.Line-1] = prevLine + currentLine
		b.lines = append(b.lines[:pos.Line], b.lines[pos.Line+1:]...) // Remove current line
		return ResultOk
	}

	b.lines[pos.Line] = line[:pos.Col-1] + line[pos.Col:]
	return ResultOk
}

func (b *Buffer) InsertLine(pos Position) Result {
	if pos.Line > len(b.lines) || pos.Line < 0 {
		return ResultErr // Invalid line number
	}
	if pos.Col > len(b.lines[pos.Line]) || pos.Col < 0 {
		return ResultErr // Invalid column number
	}
	line := b.lines[pos.Line]
	b.lines = append(b.lines[:pos.Line+1], b.lines[pos.Line:]...) // Insert empty line
	b.lines[pos.Line+1] = line[pos.Col:]
	b.lines[pos.Line] = line[:pos.Col]
	return ResultOk
}

func (b *Buffer) GetLine(lineNum int) (string, Result) {
	if lineNum >= len(b.lines) || lineNum < 0 {
		return "", ResultErr // Invalid line number
	}
	return b.lines[lineNum], ResultOk
}

func (b *Buffer) GetLines() []string {
	return b.lines
}

type Position struct {
	Line int
	Col  int
}