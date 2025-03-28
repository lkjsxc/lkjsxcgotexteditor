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
		return nil, os.NewSyscallError("ioctl TCGETS", errno)
	}
	return &t, nil
}

// setTermios sets the terminal settings.
func setTermios(fd int, t *termios) error {
	_, _, errno := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(fd),
		uintptr(syscall.TCSETS), uintptr(unsafe.Pointer(t)), 0, 0, 0)
	if errno != 0 {
		return os.NewSyscallError("ioctl TCSETS", errno)
	}
	return nil
}

// enableRawMode puts the terminal into raw mode and returns the original settings.
func enableRawMode(fd int) (*termios, error) {
	orig, err := getTermios(fd)
	if err != nil {
		return nil, fmt.Errorf("getTermios: %w", err)
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
	fd            int
	origTerm      *termios
	normalModeState string // State for normal mode commands, e.g., for 'dd'
	history       []EditorState
	historyIndex  int
}

// EditorState represents the state of the editor for history.
type EditorState struct {
	document      [][]rune
	cx, cy        int
	mode          string
	commandBuffer string
	normalModeState string
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
		fd:         fd,
		origTerm:   origTerm,
		normalModeState: "", // Initialize normalModeState
		history:      []EditorState{},
		historyIndex: 0,
	}
}

// recordHistory saves the current editor state to the history stack.
func (e *Editor) recordHistory() {
	if e.historyIndex < len(e.history) {
		e.history = e.history[:e.historyIndex] // Truncate history after current index
	}
	currentState := EditorState{
		document:      e.cloneDocument(),
		cx:            e.cx,
		cy:            e.cy,
		mode:          e.mode,
		commandBuffer: e.commandBuffer,
		normalModeState: e.normalModeState,
	}
	e.history = append(e.history, currentState)
	e.historyIndex++
}

// cloneDocument creates a deep copy of the document.
func (e *Editor) cloneDocument() [][]rune {
	docClone := make([][]rune, len(e.document))
	for i, line := range e.document {
		docClone[i] = make([]rune, len(line))
		copy(docClone[i], line)
	}
	return docClone
}

// insertChar inserts a character at the current cursor position.
func (e *Editor) insertChar(r rune) {
	e.recordHistory()
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
	e.recordHistory()
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
	e.recordHistory()
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

// deleteLine deletes the current line.
func (e *Editor) deleteLine() {
	e.recordHistory()
	if len(e.document) > 1 {
		e.document = append(e.document[:e.cy], e.document[e.cy+1:]...)
		if e.cy > len(e.document)-1 {
			e.cy = len(e.document) - 1
		}
		if e.cx > len(e.document[e.cy]) {
			e.cx = len(e.document[e.cy])
		}
	} else {
		e.document[0] = []rune{} // Clear the only line
		e.cx = 0
	}
}

// moveCursorToNextWord moves the cursor to the start of the next word.
func (e *Editor) moveCursorToNextWord() {
	line := e.document[e.cy]
	if e.cx < len(line) {
		e.cx++ // Skip current char to avoid infinite loop
	}
	for e.cx < len(line) && line[e.cx] == ' ' {
		e.cx++
	}
	for e.cx < len(line) && line[e.cx] != ' ' {
		e.cx++
	}
	if e.cx > len(line) {
		e.cx = len(line)
	}
}

// moveCursorToPrevWord moves the cursor to the start of the previous word.
func (e *Editor) moveCursorToPrevWord() {
	if e.cx > 0 {
		e.cx-- // Move once to ensure we are not on a word boundary
	}
	for e.cx > 0 && e.document[e.cy][e.cx] == ' ' {
		e.cx--
	}
	for e.cx > 0 && e.document[e.cy][e.cx] != ' ' {
		e.cx--
	}
	if e.cx > 0 && e.document[e.cy][e.cx] != ' ' {
		e.cx++ // Move to the start of the word
	}
}

// moveCursorToEndOfWord moves the cursor to the end of the current word.
func (e *Editor) moveCursorToEndOfWord() {
	line := e.document[e.cy]
	for e.cx < len(line) && line[e.cx] != ' ' {
		e.cx++
	}
}

// moveCursorToLineStart moves the cursor to the beginning of the line.
func (e *Editor) moveCursorToLineStart() {
	e.cx = 0
}

// moveCursorToLineEnd moves the cursor to the end of the line.
func (e *Editor) moveCursorToLineEnd() {
	e.cx = len(e.document[e.cy])
}

// deleteChar deletes the character at the cursor position.
func (e *Editor) deleteChar() {
	e.recordHistory()
	if e.cx < len(e.document[e.cy]) {
		line := e.document[e.cy]
		e.document[e.cy] = append(line[:e.cx], line[e.cx+1:]...)
	}
}

// insertLineBelowAndEnterInsertMode inserts a new line below and enters insert mode.
func (e *Editor) insertLineBelowAndEnterInsertMode() {
	e.insertNewline()
	e.mode = ModeInsert
}

// insertLineAboveAndEnterInsertMode inserts a new line above and enters insert mode.
func (e *Editor) insertLineAboveAndEnterInsertMode() {
	line := e.document[e.cy]
	e.document = append(e.document[:e.cy], append([][]rune{[]rune{}}, e.document[e.cy:]...)...)
	e.document[e.cy+1] = line
	e.mode = ModeInsert
}

// undo reverts the editor state to the previous history state.
func (e *Editor) undo() {
	if e.historyIndex > 1 {
		e.historyIndex--
		prevState := e.history[e.historyIndex-1]
		e.document = prevState.document
		e.cx = prevState.cx
		e.cy = prevState.cy
		// e.mode = prevState.mode
		e.commandBuffer = prevState.commandBuffer
		e.normalModeState = prevState.normalModeState
	}
}

// redo applies the next history state if available.
func (e *Editor) redo() {
	if e.historyIndex < len(e.history) {
		e.historyIndex++
		nextState := e.history[e.historyIndex-1]
		e.document = nextState.document
		e.cx = nextState.cx
		e.cy = nextState.cy
		// e.mode = nextState.mode
		e.commandBuffer = nextState.commandBuffer
		e.normalModeState = nextState.normalModeState
	}
}

// joinLines joins the current line with the next line.
func (e *Editor) joinLines() {
	if e.cy < len(e.document)-1 {
		currentLine := e.document[e.cy]
		nextLine := e.document[e.cy+1]
		e.document[e.cy] = append(currentLine, nextLine...)
		e.document = append(e.document[:e.cy+1], e.document[e.cy+2:]...)
	}
}


// draw renders the document and a status bar.
func (e *Editor) draw(showCursor bool) {
	clearScreen()
	// Render document lines.
	for y, line := range e.document {
		for x, r := range line {
			if showCursor && y == e.cy && x == e.cx && e.mode != ModeCommand {
				// Invert colors for cursor
				fmt.Print("\x1b[7m")
				fmt.Print(string(r))
				fmt.Print("\x1b[0m")
			} else {
				fmt.Print(string(r))
			}
		}
		fmt.Println("\r")
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
	if showCursor {
		if e.mode == ModeCommand {
			// Cursor at the end of the command prompt.
			fmt.Printf("\x1b[%d;%dH", len(e.document)+1, len(e.commandBuffer)+2)
		} else {
			fmt.Printf("\x1b[%d;%dH", e.cy+1, e.cx+1)
		}
	}
}

// saveFile writes the document content to a file.
func (e *Editor) saveFile(filename string) error {
	f, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("os.Create: %w", err)
	}
	defer f.Close()

	for i, line := range e.document {
		_, err := f.WriteString(string(line))
		if err != nil {
			return fmt.Errorf("WriteString: %w", err)
		}
		if i < len(e.document)-1 {
			_, err = f.WriteString("\n")
			if err != nil {
				return fmt.Errorf("WriteString newline: %w", err)
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

	switch b {
	// Command mode
	case ':':
		e.mode = ModeCommand
		e.commandBuffer = ""
		e.normalModeState = "" // Reset state when entering command mode

	// Cursor movement
	case 'h': // left
		e.moveCursorLeft()
		e.normalModeState = "" // Reset state after single char command
	case 'j': // down
		e.moveCursorDown()
		e.normalModeState = "" // Reset state after single char command
	case 'k': // up
		e.moveCursorUp()
		e.normalModeState = "" // Reset state after single char command
	case 'l': // right
		e.moveCursorRight()
		e.normalModeState = "" // Reset state after single char command
	case 'w': // next word
		e.moveCursorToNextWord()
		e.normalModeState = ""
	case 'b': // prev word
		e.moveCursorToPrevWord()
		e.normalModeState = ""
	case 'e': // end of word
		e.moveCursorToEndOfWord()
		e.normalModeState = ""
	case '0': // line start
		e.moveCursorToLineStart()
		e.normalModeState = ""
	case '$': // line end
		e.moveCursorToLineEnd()
		e.normalModeState = ""

	// Mode change
	case 'i': // insert mode
		e.mode = ModeInsert
		e.normalModeState = "" // Reset state when entering insert mode
	case 'A': // append to line end and enter insert mode
		e.cx = len(e.document[e.cy]) // Move cursor to end of line
		e.mode = ModeInsert         // Enter insert mode
		e.normalModeState = ""      // Reset state after 'A' command
	case 'o': // insert line below and enter insert mode
		e.insertLineBelowAndEnterInsertMode()
		e.normalModeState = ""
	case 'O': // insert line above and enter insert mode
		e.insertLineAboveAndEnterInsertMode()
		e.normalModeState = ""

	// Editing operations
	case 'd': // delete line or char
		if e.normalModeState == "d" {
			e.deleteLine()
			e.normalModeState = "" // Reset state after 'dd'
		} else {
			e.normalModeState = "d" // Set state to wait for second 'd'
		}
	case 'x': // delete char at cursor
		e.deleteChar()
		e.normalModeState = ""
	case 'J': // join lines
		e.joinLines()
		e.normalModeState = ""

	// Document navigation
	case 'g': // goto top of document
		if e.normalModeState == "g" {
			e.cy = 0                      // Move to top of document
			e.cx = 0
			e.normalModeState = ""      // Reset state after 'gg'
		} else {
			e.normalModeState = "g"      // Set state to wait for second 'g'
		}
	case 'G': // goto bottom of document
		e.cy = len(e.document) - 1     // Move to bottom of document
		e.cx = len(e.document[e.cy]) // Move to end of line
		e.normalModeState = ""      // Reset state after 'G' command

	// Undo/Redo
	case 'u': // undo
		e.undo()
		e.normalModeState = ""
	case 'r': // redo
		e.redo()
		e.normalModeState = ""

	default:
		e.normalModeState = "" // Reset state for other keys
	}
}

// processInsertInput handles input in Insert mode.
func (e *Editor) processInsertInput(b byte) {
	// Exit Insert mode with Esc.
	if b == 27 { // ESC
		e.mode = ModeNormal
		return
	}

	switch b {
	case 127, 8: // Backspace (DEL or Ctrl+H)
		e.backspace()
	case '\r', '\n': // Newline (Enter)
		e.insertNewline()
	default: // Insertable characters
		if b >= 32 && b < 127 {
			e.insertChar(rune(b))
		}
	}
}

// processCommandInput handles input in Command mode.
func (e *Editor) processCommandInput(b byte) {
	switch b {
	case '\r', '\n': // Enter - execute command
		e.processCommand()
	case 127, 8: // Backspace
		if len(e.commandBuffer) > 0 {
			e.commandBuffer = e.commandBuffer[:len(e.commandBuffer)-1]
		}
	default: // Command characters
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
	default:
		// Should not happen, but handle unknown mode just in case.
		fmt.Fprintf(os.Stderr, "Unknown mode: %s\n", e.mode)
	}
}

// Run starts the main input loop and refreshes the screen periodically.
func (e *Editor) Run(inputCh chan byte) {
	// Use a ticker to periodically refresh the screen (simulate cursor blinking).
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	var showCursor bool // Cursor blinking state

	e.draw(showCursor) // Initial draw.

	for {
		select {
		case b := <-inputCh:
			e.processInput(b, inputCh)
			e.draw(false) // Cursor is always shown when typing
		case <-ticker.C:
			showCursor = !showCursor
			e.draw(showCursor) // Toggle cursor visibility on ticker
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
		fmt.Fprintf(os.Stderr, "Error enabling raw mode: %+v\n", err)
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