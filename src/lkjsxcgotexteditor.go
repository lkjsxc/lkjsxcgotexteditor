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

// enableRawMode puts the terminal into raw mode and returns the original settings.
func enableRawMode(fd int) (*termios, error) {
	orig, err := getTermios(fd)
	if err != nil {
		return nil, err
	}
	raw := *orig

	// Disable input modes: no break, no CR-to-NL, no parity check, no strip char, no start/stop control.
	raw.Iflag &^= syscall.BRKINT | syscall.ICRNL | syscall.INPCK | syscall.ISTRIP | syscall.IXON

	// Disable output post processing.
	raw.Oflag &^= syscall.OPOST

	// Set control modes: 8-bit chars.
	raw.Cflag |= syscall.CS8

	// Disable local modes: echo, canonical mode, extended functions and signal characters.
	raw.Lflag &^= syscall.ECHO | syscall.ICANON | syscall.IEXTEN | syscall.ISIG

	// Set read timeout: minimum characters and decisecond timeout.
	raw.Cc[syscall.VMIN] = 0  // Minimum number of bytes
	raw.Cc[syscall.VTIME] = 1 // Timeout (in deciseconds)

	if err := setTermios(fd, &raw); err != nil {
		return nil, err
	}
	return orig, nil
}

// clearScreen clears the terminal using ANSI escape sequences.
func clearScreen() {
	fmt.Print("\x1b[2J\x1b[H")
}

// Editor represents the document, cursor state, and current mode.
type Editor struct {
	document      [][]rune
	cx, cy        int    // Cursor position (column, row)
	mode          string // "NORMAL", "INSERT", or "COMMAND"
	commandBuffer string // Command buffer when in command mode
	showCursor    bool   // For cursor blinking
	fd            int
	origTerm      *termios
}

// Supported modes.
const (
	ModeNormal  = "NORMAL"
	ModeInsert  = "INSERT"
	ModeCommand = "COMMAND"
)

// newEditor initializes the editor with an empty document in Normal mode.
func newEditor(fd int, origTerm *termios) *Editor {
	return &Editor{
		document:   [][]rune{[]rune{}},
		cx:         0,
		cy:         0,
		mode:       ModeNormal,
		showCursor: true,
		fd:         fd,
		origTerm:   origTerm,
	}
}

// insertChar inserts a character at the current cursor position.
func (e *Editor) insertChar(r rune) {
	line := e.document[e.cy]
	if e.cx > len(line) {
		e.cx = len(line)
	}
	// Insert the rune.
	newLine := append(line[:e.cx], append([]rune{r}, line[e.cx:]...)...)
	e.document[e.cy] = newLine
	e.cx++
}

// backspace removes a character before the cursor.
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

// insertNewline splits the current line at the cursor.
func (e *Editor) insertNewline() {
	line := e.document[e.cy]
	newLine := append([]rune{}, line[e.cx:]...)
	e.document[e.cy] = line[:e.cx]
	// Insert a new line below.
	e.document = append(e.document[:e.cy+1], append([][]rune{newLine}, e.document[e.cy+1:]...)...)
	e.cy++
	e.cx = 0
}

// Cursor movement functions.
func (e *Editor) moveCursorLeft() {
	if e.cx > 0 {
		e.cx--
	} else if e.cy > 0 {
		e.cy--
		e.cx = len(e.document[e.cy])
	}
}

func (e *Editor) moveCursorRight() {
	lineLen := len(e.document[e.cy])
	if e.cx < lineLen {
		e.cx++
	} else if e.cy < len(e.document)-1 {
		e.cy++
		e.cx = 0
	}
}

func (e *Editor) moveCursorUp() {
	if e.cy > 0 {
		e.cy--
		if e.cx > len(e.document[e.cy]) {
			e.cx = len(e.document[e.cy])
		}
	}
}

func (e *Editor) moveCursorDown() {
	if e.cy < len(e.document)-1 {
		e.cy++
		if e.cx > len(e.document[e.cy]) {
			e.cx = len(e.document[e.cy])
		}
	}
}

// draw renders the document and a status bar.
func (e *Editor) draw() {
	clearScreen()
	// Render document lines.
	for _, line := range e.document {
		fmt.Println(string(line))
		fmt.Print("\r")
	}
	// Draw status or command bar with inverted colors.
	fmt.Print("\x1b[7m")
	if e.mode == ModeCommand {
		fmt.Printf(":%s", e.commandBuffer)
	} else {
		status := fmt.Sprintf(" %s | Ln %d, Col %d ", e.mode, e.cy+1, e.cx+1)
		fmt.Print(status)
	}
	fmt.Print("\x1b[K")
	fmt.Print("\x1b[0m\r\n")

	// Position the cursor.
	if e.mode == ModeCommand {
		// Cursor at the end of the command prompt.
		fmt.Printf("\x1b[%d;%dH", len(e.document)+1, len(e.commandBuffer)+2)
	} else {
		fmt.Printf("\x1b[%d;%dH", e.cy+1, e.cx+1)
	}
}

// saveFile writes the document content to a file.
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

// processCommand interprets and executes a completed command.
func (e *Editor) processCommand() {
	cmd := e.commandBuffer
	e.commandBuffer = ""
	e.mode = ModeNormal

	switch cmd {
	case "q":
		setTermios(e.fd, e.origTerm)
		os.Exit(0)
	case "w":
		if err := e.saveFile("output.txt"); err != nil {
			// Optionally display an error message on the status bar.
		}
	case "wq":
		if err := e.saveFile("output.txt"); err != nil {
			// Optionally display an error message on the status bar.
			return
		}
		setTermios(e.fd, e.origTerm)
		os.Exit(0)
	}
}

// processNormalInput handles input in Normal mode.
func (e *Editor) processNormalInput(b byte, inputCh chan byte) {
	// Handle escape sequences for arrow keys.
	if b == 27 {
		seq := []byte{b, <-inputCh, <-inputCh}
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
	case 'i':
		e.mode = ModeInsert
	}
}

// processInsertInput handles input in Insert mode.
func (e *Editor) processInsertInput(b byte) {
	// Exit Insert mode with Esc.
	if b == 27 {
		e.mode = ModeNormal
		return
	}

	switch b {
	case 127, 8: // Backspace.
		e.backspace()
	case '\r', '\n': // Newline.
		e.insertNewline()
	default:
		if b >= 32 && b < 127 {
			e.insertChar(rune(b))
		}
	}
}

// processCommandInput handles input in Command mode.
func (e *Editor) processCommandInput(b byte) {
	switch b {
	case '\r', '\n':
		e.processCommand()
	case 127, 8: // Backspace.
		if len(e.commandBuffer) > 0 {
			e.commandBuffer = e.commandBuffer[:len(e.commandBuffer)-1]
		}
	default:
		if b >= 32 && b < 127 {
			e.commandBuffer += string(b)
		}
	}
}

// processInput routes the input to the correct handler based on mode.
func (e *Editor) processInput(b byte, inputCh chan byte) {
	switch e.mode {
	case ModeNormal:
		e.processNormalInput(b, inputCh)
	case ModeInsert:
		e.processInsertInput(b)
	case ModeCommand:
		e.processCommandInput(b)
	}
}

// Run starts the main input loop and refreshes the screen periodically.
func (e *Editor) Run(inputCh chan byte) {
	// Use a ticker to periodically refresh the screen (simulate cursor blinking).
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	e.draw() // Initial draw.

	for {
		select {
		case b := <-inputCh:
			e.processInput(b, inputCh)
			e.draw()
		case <-ticker.C:
			e.showCursor = !e.showCursor
			e.draw()
		}
	}
}

// readInput continuously reads from os.Stdin and sends bytes to inputCh.
func readInput(inputCh chan byte) {
	buf := make([]byte, 1)
	for {
		n, err := os.Stdin.Read(buf)
		if err == nil && n == 1 {
			inputCh <- buf[0]
		}
	}
}

// setupSignalHandler ensures the terminal settings are restored on exit signals.
func setupSignalHandler(fd int, orig *termios) {
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-sigCh
		setTermios(fd, orig)
		os.Exit(0)
	}()
}

func main() {
	fd := int(os.Stdin.Fd())
	origTerm, err := enableRawMode(fd)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error enabling raw mode: %v\n", err)
		os.Exit(1)
	}
	// Ensure terminal is restored on exit.
	defer setTermios(fd, origTerm)
	setupSignalHandler(fd, origTerm)

	editor := newEditor(fd, origTerm)
	inputCh := make(chan byte)

	// Start the input reader goroutine.
	go readInput(inputCh)

	// Run the editor main loop.
	editor.Run(inputCh)
}
