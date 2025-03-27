//go:build linux || darwin
// +build linux darwin

package main

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"
	"unsafe"
)

// termios holds terminal settings.
type termios syscall.Termios

// getTermios retrieves current terminal settings.
func getTermios(fd int) (*termios, error) {
	var t termios
	_, _, errno := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd),
		uintptr(syscall.TCGETS), uintptr(unsafe.Pointer(&t)), 0, 0, 0)
	if errno != 0 {
		return nil, errno
	}
	return &t, nil
}

// setTermios sets the terminal settings.
func setTermios(fd int, t *termios) error {
	_, _, errno := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd),
		uintptr(syscall.TCSETS), uintptr(unsafe.Pointer(t)), 0, 0, 0)
	if errno != 0 {
		return errno
	}
	return nil
}

// enableRawMode puts the terminal into raw mode.
func enableRawMode(fd int) (*termios, error) {
	orig, err := getTermios(fd)
	if err != nil {
		return nil, err
	}
	raw := *orig

	// Input modes: no break, no CR to NL, no parity check, no strip char,
	// no start/stop output control.
	raw.Iflag &^= syscall.BRKINT | syscall.ICRNL | syscall.INPCK | syscall.ISTRIP | syscall.IXON

	// Output modes: disable post processing.
	raw.Oflag &^= syscall.OPOST

	// Control modes: set 8 bit chars.
	raw.Cflag |= syscall.CS8

	// Local modes: echoing off, canonical off, no extended functions,
	// no signal chars.
	raw.Lflag &^= syscall.ECHO | syscall.ICANON | syscall.IEXTEN | syscall.ISIG

	// Control characters: set read timeout.
	raw.Cc[syscall.VMIN] = 0  // minimum number of characters for noncanonical read
	raw.Cc[syscall.VTIME] = 1 // timeout in deciseconds for noncanonical read

	if err := setTermios(fd, &raw); err != nil {
		return nil, err
	}
	return orig, nil
}

// clearScreen uses ANSI escape sequences to clear the terminal.
func clearScreen() {
	fmt.Print("\x1b[2J")
	fmt.Print("\x1b[H")
}

// Editor holds the document and cursor state.
type Editor struct {
	document   [][]rune
	cx, cy     int // cursor x (column) and y (row)
	showCursor bool
}

// newEditor initializes an editor with one empty line.
func newEditor() *Editor {
	return &Editor{
		document:   [][]rune{[]rune{}},
		cx:         0,
		cy:         0,
		showCursor: true,
	}
}

// insertChar inserts a rune at the current cursor position.
func (e *Editor) insertChar(r rune) {
	line := e.document[e.cy]
	if e.cx > len(line) {
		e.cx = len(line)
	}
	// Insert r at position cx.
	newLine := append(line[:e.cx], append([]rune{r}, line[e.cx:]...)...)
	e.document[e.cy] = newLine
	e.cx++
}

// backspace removes a character to the left of the cursor.
func (e *Editor) backspace() {
	if e.cx > 0 {
		line := e.document[e.cy]
		e.document[e.cy] = append(line[:e.cx-1], line[e.cx:]...)
		e.cx--
	} else if e.cy > 0 {
		// Merge with previous line.
		prevLine := e.document[e.cy-1]
		curLine := e.document[e.cy]
		e.cx = len(prevLine)
		e.document[e.cy-1] = append(prevLine, curLine...)
		// Remove current line.
		e.document = append(e.document[:e.cy], e.document[e.cy+1:]...)
		e.cy--
	}
}

// insertNewline splits the current line at the cursor.
func (e *Editor) insertNewline() {
	line := e.document[e.cy]
	newLine := append([]rune{}, line[e.cx:]...)
	e.document[e.cy] = line[:e.cx]
	// Insert newLine below current line.
	e.document = append(e.document[:e.cy+1], append([][]rune{newLine}, e.document[e.cy+1:]...)...)
	e.cy++
	e.cx = 0
}

// moveCursorLeft moves the cursor left.
func (e *Editor) moveCursorLeft() {
	if e.cx > 0 {
		e.cx--
	} else if e.cy > 0 {
		e.cy--
		e.cx = len(e.document[e.cy])
	}
}

// moveCursorRight moves the cursor right.
func (e *Editor) moveCursorRight() {
	lineLen := len(e.document[e.cy])
	if e.cx < lineLen {
		e.cx++
	} else if e.cy < len(e.document)-1 {
		e.cy++
		e.cx = 0
	}
}

// moveCursorUp moves the cursor up.
func (e *Editor) moveCursorUp() {
	if e.cy > 0 {
		e.cy--
		if e.cx > len(e.document[e.cy]) {
			e.cx = len(e.document[e.cy])
		}
	}
}

// moveCursorDown moves the cursor down.
func (e *Editor) moveCursorDown() {
	if e.cy < len(e.document)-1 {
		e.cy++
		if e.cx > len(e.document[e.cy]) {
			e.cx = len(e.document[e.cy])
		}
	}
}

// draw renders the document and positions the cursor.
func (e *Editor) draw() {
	clearScreen()
	// Draw document lines.
	for _, line := range e.document {
		fmt.Println(string(line))
	}
	// Draw status bar.
	fmt.Printf("\x1b[7m") // Invert colors for status bar.
	status := fmt.Sprintf(" CTRL-S:Save | CTRL-Q:Quit | Ln %d, Col %d ", e.cy+1, e.cx+1)
	fmt.Print(status)
	// Clear remaining line.
	fmt.Print("\x1b[K")
	fmt.Printf("\x1b[0m\r\n")

	// Position the cursor.
	// ANSI escape: \x1b[row;colH positions the cursor.
	// We add 1 to both row and column since ANSI coordinates are 1-indexed.
	fmt.Printf("\x1b[%d;%dH", e.cy+1, e.cx+1)
}

// saveFile writes the document to a file.
func (e *Editor) saveFile(filename string) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()
	for i, line := range e.document {
		_, err := f.WriteString(string(line))
		if err != nil {
			return err
		}
		// Don't add a newline on the last line.
		if i < len(e.document)-1 {
			_, err = f.WriteString("\n")
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// processInput handles key events, including escape sequences.
func (e *Editor) processInput(b byte, inputCh chan byte, fd int, orig *termios) {
	// Check for escape sequences.
	if b == 27 {
		// Read the next two bytes to determine the sequence.
		seq := make([]byte, 3)
		seq[0] = b
		seq[1] = <-inputCh
		seq[2] = <-inputCh
		if seq[1] == '[' {
			switch seq[2] {
			case 'A':
				e.moveCursorUp()
			case 'B':
				e.moveCursorDown()
			case 'C':
				e.moveCursorRight()
			case 'D':
				e.moveCursorLeft()
			}
		}
		return
	}

	switch b {
	case 17: // Ctrl-Q to quit.
		setTermios(fd, orig)
		os.Exit(0)
	case 19: // Ctrl-S to save.
		if err := e.saveFile("output.txt"); err != nil {
			// Optionally, handle error.
		}
	case 127, 8: // Backspace.
		e.backspace()
	case '\r', '\n': // Enter.
		e.insertNewline()
	default:
		if b >= 32 && b < 127 {
			e.insertChar(rune(b))
		}
	}
}

func main() {
	// File descriptor for standard input.
	fd := int(os.Stdin.Fd())
	origTerm, err := enableRawMode(fd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error enabling raw mode: %v\n", err)
		os.Exit(1)
	}
	// Restore terminal on exit.
	defer setTermios(fd, origTerm)

	// Handle SIGINT (Ctrl-C) to exit gracefully.
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt)
	go func() {
		<-sigCh
		setTermios(fd, origTerm)
		os.Exit(0)
	}()

	editor := newEditor()

	inputCh := make(chan byte)
	// Goroutine to continuously read from stdin.
	go func() {
		buf := make([]byte, 1)
		for {
			n, err := os.Stdin.Read(buf)
			if err == nil && n == 1 {
				inputCh <- buf[0]
			}
		}
	}()

	// Use a ticker for asynchronous screen refresh (and cursor flash if desired).
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case b := <-inputCh:
			editor.processInput(b, inputCh, fd, origTerm)
			editor.draw()
		case <-ticker.C:
			// Here you could toggle a blinking cursor flag if desired.
			editor.showCursor = !editor.showCursor
			editor.draw()
		}
	}
}
