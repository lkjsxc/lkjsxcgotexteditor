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

	// Input modes: no break, no CR-to-NL, no parity check, no strip char, no start/stop control.
	raw.Iflag &^= syscall.BRKINT | syscall.ICRNL | syscall.INPCK | syscall.ISTRIP | syscall.IXON

	// Output modes: disable post processing.
	raw.Oflag &^= syscall.OPOST

	// Control modes: set 8-bit chars.
	raw.Cflag |= syscall.CS8

	// Local modes: echoing off, canonical off, no extended functions, no signal chars.
	raw.Lflag &^= syscall.ECHO | syscall.ICANON | syscall.IEXTEN | syscall.ISIG

	// Control characters: set read timeout.
	raw.Cc[syscall.VMIN] = 0  // minimum characters for noncanonical read
	raw.Cc[syscall.VTIME] = 1 // timeout in deciseconds

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

// Editor represents the document, cursor position and current mode.
type Editor struct {
	document      [][]rune
	cx, cy        int    // cursor position: column and row
	mode          string // "NORMAL", "INSERT", or "COMMAND"
	commandBuffer string // holds the command when in command mode
	showCursor    bool
}

// Modes.
const (
	ModeNormal  = "NORMAL"
	ModeInsert  = "INSERT"
	ModeCommand = "COMMAND"
)

// newEditor creates an editor with one empty line in Normal mode.
func newEditor() *Editor {
	return &Editor{
		document:   [][]rune{[]rune{}},
		cx:         0,
		cy:         0,
		mode:       ModeNormal,
		showCursor: true,
	}
}

// insertChar inserts a rune at the current cursor position.
func (e *Editor) insertChar(r rune) {
	line := e.document[e.cy]
	if e.cx > len(line) {
		e.cx = len(line)
	}
	// Insert the rune at the cursor.
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
		e.document = append(e.document[:e.cy], e.document[e.cy+1:]...)
		e.cy--
	}
}

// insertNewline splits the current line at the cursor position.
func (e *Editor) insertNewline() {
	line := e.document[e.cy]
	newLine := append([]rune{}, line[e.cx:]...)
	e.document[e.cy] = line[:e.cx]
	// Insert new line below.
	e.document = append(e.document[:e.cy+1], append([][]rune{newLine}, e.document[e.cy+1:]...)...)
	e.cy++
	e.cx = 0
}

// moveCursorLeft moves the cursor left (or to previous line if at beginning).
func (e *Editor) moveCursorLeft() {
	if e.cx > 0 {
		e.cx--
	} else if e.cy > 0 {
		e.cy--
		e.cx = len(e.document[e.cy])
	}
}

// moveCursorRight moves the cursor right (or to next line if at end).
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

// draw renders the document along with a status bar (or command line) showing mode and cursor info.
func (e *Editor) draw() {
	clearScreen()
	// Draw document lines.
	for _, line := range e.document {
		fmt.Println(string(line))
	}
	// Draw status bar or command prompt.
	if e.mode == ModeCommand {
		// Show command prompt at the bottom.
		fmt.Print("\x1b[7m")
		cmdLine := fmt.Sprintf(":%s", e.commandBuffer)
		fmt.Print(cmdLine)
		fmt.Print("\x1b[K")
	} else {
		// Show normal status bar.
		fmt.Print("\x1b[7m")
		status := fmt.Sprintf(" %s | Ln %d, Col %d ", e.mode, e.cy+1, e.cx+1)
		fmt.Print(status)
		fmt.Print("\x1b[K")
	}
	fmt.Print("\x1b[0m\r\n")
	// Position the cursor.
	if e.mode == ModeCommand {
		// Cursor at the end of the command prompt.
		fmt.Printf("\x1b[%d;%dH", len(e.document)+1, len(e.commandBuffer)+2)
	} else {
		fmt.Printf("\x1b[%d;%dH", e.cy+1, e.cx+1)
	}
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
		if i < len(e.document)-1 {
			_, err = f.WriteString("\n")
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// processCommand processes the completed command entered in command mode.
func (e *Editor) processCommand(fd int, orig *termios) {
	cmd := e.commandBuffer
	// Reset command buffer and return to normal mode.
	e.commandBuffer = ""
	e.mode = ModeNormal

	// Process known commands.
	switch cmd {
	case "q":
		setTermios(fd, orig)
		os.Exit(0)
	case "w":
		if err := e.saveFile("output.txt"); err != nil {
			// Optionally, display an error message in status bar.
		}
	case "wq":
		if err := e.saveFile("output.txt"); err != nil {
			// Optionally, display an error message in status bar.
			return
		}
		setTermios(fd, orig)
		os.Exit(0)
	}
}

// processNormalInput handles keystrokes in Normal mode.
func (e *Editor) processNormalInput(b byte, inputCh chan byte, fd int, orig *termios) {
	// Handle escape sequences (arrow keys).
	if b == 27 {
		seq := make([]byte, 3)
		seq[0] = b
		seq[1] = <-inputCh
		seq[2] = <-inputCh
		if seq[1] == '[' {
			switch seq[2] {
			case 'A': // Up arrow.
				e.moveCursorUp()
			case 'B': // Down arrow.
				e.moveCursorDown()
			case 'C': // Right arrow.
				e.moveCursorRight()
			case 'D': // Left arrow.
				e.moveCursorLeft()
			}
		}
		return
	}

	switch b {
	case ':':
		// Enter command mode.
		e.mode = ModeCommand
		e.commandBuffer = ""
	case 'h':
		e.moveCursorLeft()
	case 'j':
		e.moveCursorDown()
	case 'k':
		e.moveCursorUp()
	case 'l':
		e.moveCursorRight()
	case 'i': // Enter Insert mode.
		e.mode = ModeInsert
	}
}

// processInsertInput handles keystrokes in Insert mode.
func (e *Editor) processInsertInput(b byte, inputCh chan byte, fd int, orig *termios) {
	// Exit insert mode with Esc.
	if b == 27 {
		e.mode = ModeNormal
		return
	}

	switch b {
	case 127, 8: // Backspace.
		e.backspace()
	case '\r', '\n': // Enter.
		e.insertNewline()
	default:
		// Only add printable characters.
		if b >= 32 && b < 127 {
			e.insertChar(rune(b))
		}
	}
}

// processCommandInput handles keystrokes in Command mode.
func (e *Editor) processCommandInput(b byte, inputCh chan byte, fd int, orig *termios) {
	switch b {
	case '\r', '\n':
		// Process the command.
		e.processCommand(fd, orig)
	case 127, 8: // Backspace.
		if len(e.commandBuffer) > 0 {
			e.commandBuffer = e.commandBuffer[:len(e.commandBuffer)-1]
		}
	default:
		// Append only printable characters.
		if b >= 32 && b < 127 {
			e.commandBuffer += string(b)
		}
	}
}

// processInput dispatches the input based on the current mode.
func (e *Editor) processInput(b byte, inputCh chan byte, fd int, orig *termios) {
	switch e.mode {
	case ModeNormal:
		e.processNormalInput(b, inputCh, fd, orig)
	case ModeInsert:
		e.processInsertInput(b, inputCh, fd, orig)
	case ModeCommand:
		e.processCommandInput(b, inputCh, fd, orig)
	}
}

func main() {
	fd := int(os.Stdin.Fd())
	origTerm, err := enableRawMode(fd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error enabling raw mode: %v\n", err)
		os.Exit(1)
	}
	defer setTermios(fd, origTerm)

	// Ensure that the terminal is restored on interrupt.
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

	// Ticker for asynchronous screen refresh.
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case b := <-inputCh:
			editor.processInput(b, inputCh, fd, origTerm)
			editor.draw()
		case <-ticker.C:
			// Toggle cursor flash or simply refresh the screen.
			editor.showCursor = !editor.showCursor
			editor.draw()
		}
	}
}
