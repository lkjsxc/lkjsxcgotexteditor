package editor

import (
	"lkjsxcgoeditor/src/buffer"
	"lkjsxcgoeditor/src/terminal"
)

type Editor struct {
	terminal *terminal.Terminal
	buffer   *buffer.Buffer
	cursor   buffer.Position
	mode     EditorMode
}

type Result int

const (
	ResultOk Result = iota
	ResultErr
)

type EditorMode int

const (
	NormalMode EditorMode = iota
	InsertMode
)

func NewEditor() (*Editor, Result) {
	t, result := terminal.NewTerminal()
	if result != terminal.ResultOk {
		return nil, ResultErr
	}
	
	return &Editor{
		terminal: t,
		buffer:   buffer.NewBuffer(),
		cursor:   buffer.Position{Line: 0, Col: 0},
	}, ResultOk
}

func (e *Editor) Run() Result {
	defer e.terminal.RestoreTerminal()
	e.terminal.Print("lkjsxcgoeditor - v0.0.1\n")
	e.terminal.Print("Press Ctrl+Q to quit.\n")

	for {
		e.refreshScreen()
		key, result := e.terminal.ReadKey()
		if result != terminal.ResultOk {
			return ResultErr
		}

		if key == 'q' && key & 0x1f == 0x11 { // Ctrl+Q to quit
			break
		}

		e.processKey(key)
	}
	return ResultOk
}

func (e *Editor) refreshScreen() {
	e.terminal.Print("\033[2J") // Clear screen
	e.terminal.Print("\033[H")  // Move cursor to top-left

	lines := e.buffer.GetLines()
	for i, line := range lines {
		if i == e.cursor.Line {
			e.terminal.Print(line[:e.cursor.Col])
			e.terminal.Print("\033[7m") // Invert colors for cursor
			if len(line) > 0 { // Check if line is not empty
				e.terminal.Print(string(line[e.cursor.Col]))
			}
			e.terminal.Print("\033[0m")     // Reset attributes
			if e.cursor.Col+1 <= len(line) { // Check bounds before slicing
				e.terminal.Println(line[e.cursor.Col+1:])
			} else {
				e.terminal.Println("") // Print empty line if cursor is at the end
			}
			e.terminal.Println(line[e.cursor.Col+1:])
		} else {
			e.terminal.Println(line)
		}
	}
	// e.terminal.Print(fmt.Sprintf("\033[%d;%dH", e.cursor.Line+1, e.cursor.Col+1)) // Set cursor position
}

func (e *Editor) processKey(key byte) {
	switch key {
	case terminal.KeyLeft:
		if e.cursor.Col > 0 {
			e.cursor.Col--
		}
	case terminal.KeyRight:
		lineLen := len(e.buffer.GetLines()[e.cursor.Line])
		if e.cursor.Col < lineLen {
			e.cursor.Col++
		}
	case terminal.KeyUp:
		if e.cursor.Line > 0 {
			e.cursor.Line--
		}
	case terminal.KeyDown:
		if e.cursor.Line < len(e.buffer.GetLines())-1 {
			e.cursor.Line++
		}
	default:
		e.buffer.InsertChar(rune(key), e.cursor)
		e.cursor.Col++
	}
}