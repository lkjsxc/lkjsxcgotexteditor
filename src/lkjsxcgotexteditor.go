package main

import (
	"os"
	"syscall"
	"unicode/utf8"
	"unsafe"
)

// --- Constants ---
const VERSION = "0.0.1"
const TAB_STOP = 8
const QUIT_TIMES = 2 // How many times Ctrl+Q must be pressed if dirty
const MAX_LINES = 4096 // Fixed number of lines (4k * 120 * 4 = ~1.9MB)
const MAX_COLS = 120   // Fixed max column width per line
const STATUS_MSG_BUF_SIZE = 128 // Should be >= MAX_COLS potentially
const FILENAME_BUF_SIZE = 256 // Max filename length

// --- Types ---

type Result int

const (
	result_ok Result = iota
	result_err
)

type EditorMode int

const (
	Mode_Normal EditorMode = iota
	Mode_Insert
)

// Using EditorKey = int32 to align better with potential rune values if needed
// but distinct enough for special keys.
type EditorKey int32

const (
	Key_Null       EditorKey = 0    // Represents no key or error
	Key_Escape     EditorKey = 1000 // Start special keys above rune range
	Key_ArrowLeft  EditorKey = 1001
	Key_ArrowRight EditorKey = 1002
	Key_ArrowUp    EditorKey = 1003
	Key_ArrowDown  EditorKey = 1004
	Key_Delete     EditorKey = 1005
	Key_Home       EditorKey = 1006
	Key_End        EditorKey = 1007
	Key_PageUp     EditorKey = 1008
	Key_PageDown   EditorKey = 1009
	Key_Backspace  EditorKey = 1010 // Represent BS/DEL consistently
	Key_Enter      EditorKey = 1011 // Represent CR/LF consistently

	// Map control characters directly using their ASCII value (0-31)
	Ctrl_A EditorKey = 1
	Ctrl_B EditorKey = 2
	Ctrl_C EditorKey = 3
	Ctrl_D EditorKey = 4
	Ctrl_E EditorKey = 5
	Ctrl_F EditorKey = 6
	Ctrl_G EditorKey = 7
	Ctrl_H EditorKey = 8 // Often Backspace
	Ctrl_I EditorKey = 9 // Tab
	Ctrl_J EditorKey = 10 // Often Newline
	Ctrl_K EditorKey = 11
	Ctrl_L EditorKey = 12 // Often Refresh screen?
	Ctrl_M EditorKey = 13 // Often Enter (Carriage Return)
	Ctrl_N EditorKey = 14
	Ctrl_O EditorKey = 15
	Ctrl_P EditorKey = 16
	Ctrl_Q EditorKey = 17 // Quit
	Ctrl_R EditorKey = 18
	Ctrl_S EditorKey = 19 // Save
	Ctrl_T EditorKey = 20
	Ctrl_U EditorKey = 21
	Ctrl_V EditorKey = 22
	Ctrl_W EditorKey = 23
	Ctrl_X EditorKey = 24
	Ctrl_Y EditorKey = 25
	Ctrl_Z EditorKey = 26
)

// Buffer representation
type EditorBuffer struct {
	lines       [MAX_LINES][MAX_COLS]rune
	lineLengths [MAX_LINES]int
	numLines    int
	filename    [FILENAME_BUF_SIZE]byte // Fixed buffer for filename
	filenameLen int
	dirty       bool
}

// Editor State
type EditorState struct {
	termFd      int             // Terminal file descriptor
	origTermios syscall.Termios // Original terminal settings
	screenRows  int
	screenCols  int
	cursorX     int // Logical column (rune index within the line text)
	cursorY     int // Line number (0-based index into buffer.lines)
	renderX     int // Actual screen column (considering tabs, 0-based)
	rowOffset   int // First visible row in the buffer (0-based index)
	colOffset   int // First visible column on screen (render column index, 0-based)
	buffer      EditorBuffer
	mode        EditorMode
	statusMsg   [STATUS_MSG_BUF_SIZE]byte // Fixed-size status message buffer
	statusMsgLen int
	// statusMsgTime time.Time // Requires time pkg, skip for now
	shouldQuit  bool
	quitConfirm int // Counter for confirming quit on dirty file
	isTTY       bool // Flag indicating if stdin is a terminal
}

var E EditorState // Global editor state

// --- Utility Functions ---

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Simple integer to ASCII byte slice conversion for appending
// Appends result to dst and returns the new slice. Assumes dst has capacity.
func itoaAppend(dst []byte, i int) []byte {
	if i == 0 {
		return append(dst, '0')
	}
	var buf [20]byte // Max length for 64-bit int
	pos := len(buf)
	neg := false
	if i < 0 {
		neg = true
		i = -i // Make positive
		// Handle MIN_INT? Edge case, ignore for now.
	}
	for i > 0 {
		pos--
		buf[pos] = byte('0' + (i % 10))
		i /= 10
	}
	if neg {
		pos--
		buf[pos] = '-'
	}
	return append(dst, buf[pos:]...)
}

// Set status message from string literal or simple string.
// Avoids fmt allocations. Truncates safely.
func setStatusMessage(msg string) {
	// Calculate max bytes to copy from msg, leaving room for "..." if truncated
	maxCopyLen := len(E.statusMsg)
	ellipsisLen := 0
	if len(msg) > maxCopyLen {
		ellipsisLen = 3
		if maxCopyLen > ellipsisLen {
			maxCopyLen -= ellipsisLen
		} else {
			maxCopyLen = 0 // Not enough space even for ellipsis
		}
	}

	// Copy message bytes
	// Using []byte(msg) might allocate temporarily, but msg is often literal.
	copiedLen := copy(E.statusMsg[:maxCopyLen], msg)

	// Add ellipsis if truncated
	if ellipsisLen > 0 && copiedLen == maxCopyLen {
		copy(E.statusMsg[copiedLen:], "...")
		copiedLen += ellipsisLen
	}

	// Clear remaining buffer
	if copiedLen < len(E.statusMsg) {
		clear(E.statusMsg[copiedLen:])
	}
	E.statusMsgLen = copiedLen
}

// Set filename in the fixed buffer. Truncates if necessary.
func setFilename(fname string) {
	n := copy(E.buffer.filename[:], fname)
	E.buffer.filenameLen = n
	// Clear remaining buffer
	if n < len(E.buffer.filename) {
		clear(E.buffer.filename[n:])
	}
}

// --- Terminal Handling ---

// isTerminal checks if the given file descriptor refers to a terminal.
// Uses only standard library functions.
func isTerminal(fd int) bool {
	fileInfo, err := os.Stdin.Stat() // Check specifically Stdin as that's what we use for input/control
	if err != nil {
		return false // Error stating, assume not a terminal
	}
	// Check if the file mode includes os.ModeDevice and os.ModeCharDevice
	return (fileInfo.Mode() & (os.ModeDevice | os.ModeCharDevice)) == (os.ModeDevice | os.ModeCharDevice)
}

func enableRawMode() Result {
	if !E.isTTY {
		return result_err // Cannot enable raw mode on non-TTY
	}

	var termios syscall.Termios
	// Use TCGETS to retrieve current terminal settings
	if _, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(E.termFd), syscall.TCGETS, uintptr(unsafe.Pointer(&termios))); errno != 0 {
		return result_err
	}
	E.origTermios = termios

	raw := termios
	// Input flags: no break, no CR->NL, no parity check, no strip bit, no start/stop output control.
	raw.Iflag &^= (syscall.IGNBRK | syscall.BRKINT | syscall.PARMRK | syscall.ISTRIP | syscall.INLCR | syscall.IGNCR | syscall.ICRNL | syscall.IXON)
	// Output flags: disable post processing.
	raw.Oflag &^= syscall.OPOST
	// Control flags: set 8 bit chars.
	raw.Cflag &^= (syscall.CSIZE | syscall.PARENB)
	raw.Cflag |= syscall.CS8
	// Local flags: no echoing, no canonical processing, no signal chars (^C, ^Z), no extended input processing.
	raw.Lflag &^= (syscall.ECHO | syscall.ICANON | syscall.IEXTEN | syscall.ISIG)
	// Control characters: read returns immediately if no data. Timeout 0.1s
	raw.Cc[syscall.VMIN] = 0
	raw.Cc[syscall.VTIME] = 1 // 1 * 100ms = 100ms timeout

	// Use TCSETS to apply new settings immediately. TCSETSW (wait) might be preferred but less portable.
	if _, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(E.termFd), syscall.TCSETS, uintptr(unsafe.Pointer(&raw))); errno != 0 {
		return result_err
	}
	return result_ok
}

func disableRawMode() {
	// Only disable if FD is valid and it was a TTY
	if E.isTTY && E.termFd >= 0 {
		// Use TCSETS to restore original settings immediately.
		_, _, _ = syscall.Syscall(syscall.SYS_IOCTL, uintptr(E.termFd), syscall.TCSETS, uintptr(unsafe.Pointer(&E.origTermios)))
	}
}

func getWindowSize() Result {
	if !E.isTTY {
		// If not a TTY, window size is irrelevant / cannot be obtained via ioctl.
		// Use defaults, but don't indicate an error unless specifically asked.
		E.screenRows = 24
		E.screenCols = 80
		return result_ok // Not an error in this context, just using defaults.
	}

	var sz struct {
		rows uint16
		cols uint16
		x    uint16 // pixel width (unused)
		y    uint16 // pixel height (unused)
	}
	// Use syscall constant if available, otherwise guess common value (0x5413 on Linux)
	const TIOCGWINSZ = syscall.TIOCGWINSZ // Should work on Linux/macOS

	if _, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(E.termFd), TIOCGWINSZ, uintptr(unsafe.Pointer(&sz))); errno != 0 || sz.cols == 0 || sz.rows == 0 {
		// Use a reasonable default if ioctl fails on a TTY
		E.screenRows = 24
		E.screenCols = 80
		return result_err // Indicate failure to get actual size
	} else {
		E.screenRows = int(sz.rows)
		E.screenCols = int(sz.cols)
		return result_ok
	}
}

// Reads a keypress, handling escape sequences.
// Returns the rune if it's a printable char, or the special EditorKey constant.
// Limitations: Does not handle multi-byte UTF-8 characters robustly. Assumes terminal sends one byte at a time.
func readKey() (rune, EditorKey, Result) {
	var buf [1]byte
	n, err := syscall.Read(E.termFd, buf[:])

	if err != nil && err != syscall.EAGAIN && err != syscall.EINTR {
		// Real read error
		return 0, Key_Null, result_err
	}

	if n == 0 {
		return 0, Key_Null, result_ok // Timeout or signal, no key press
	}

	c := buf[0]

	if c == '\x1b' { // Escape sequence
		var seq [5]byte // Buffer for sequence: [, Type, Params..., Term
		// Try reading the rest of the sequence with short timeout (VTIME)
		n2, _ := syscall.Read(E.termFd, seq[0:1])
		if n2 == 0 {
			return 0, Key_Escape, result_ok
		} // Just Escape

		if seq[0] == '[' { // CSI: Control Sequence Introducer "\x1b["
			n3, _ := syscall.Read(E.termFd, seq[1:2])
			if n3 == 0 {
				return 0, Key_Escape, result_ok
			} // Incomplete sequence

			if seq[1] >= '0' && seq[1] <= '9' { // Sequence with parameters
				// Read parameters until terminating character
				paramBuf := [3]byte{seq[1]} // Store first digit
				paramLen := 1
				for paramLen < len(paramBuf) {
					nParam, _ := syscall.Read(E.termFd, seq[paramLen:paramLen+1])
					if nParam == 0 {
						return 0, Key_Escape, result_ok
					} // Incomplete sequence
					if seq[paramLen] >= '0' && seq[paramLen] <= '9' || seq[paramLen] == ';' {
						paramBuf[paramLen] = seq[paramLen]
						paramLen++
					} else {
						// End of parameters, seq[paramLen] is the command character
						break
					}
				}
				// Read the final command character if not already read
				finalChar := byte(0) // Initialize finalChar
				if paramLen < len(seq) { // If break occurred, finalChar is at seq[paramLen]
					finalChar = seq[paramLen]
				} else { // Loop finished without breaking (buffer full), need to read final char
                    nFinal, _ := syscall.Read(E.termFd, seq[paramLen:paramLen+1]) // Read into seq[paramLen]
                    if nFinal == 0 { return 0, Key_Escape, result_ok }
                    finalChar = seq[paramLen]
                }


				if finalChar == '~' {
					// Parse paramBuf (simple check for now)
					switch paramBuf[0] { // Check first digit only for simplicity
					case '1': return 0, Key_Home, result_ok   // Usually Home
					case '3': return 0, Key_Delete, result_ok
					case '4': return 0, Key_End, result_ok    // Usually End
					case '5': return 0, Key_PageUp, result_ok
					case '6': return 0, Key_PageDown, result_ok
					case '7': return 0, Key_Home, result_ok   // Sometimes Home
					case '8': return 0, Key_End, result_ok    // Sometimes End
					}
				}
				// Ignore other parameterized sequences (like mouse, function keys with modifiers)
			} else { // Sequence without parameters (or parameter was not a digit)
				switch seq[1] {
				case 'A': return 0, Key_ArrowUp, result_ok
				case 'B': return 0, Key_ArrowDown, result_ok
				case 'C': return 0, Key_ArrowRight, result_ok
				case 'D': return 0, Key_ArrowLeft, result_ok
				case 'H': return 0, Key_Home, result_ok // xterm Home
				case 'F': return 0, Key_End, result_ok  // xterm End
				}
			}
		} else if seq[0] == 'O' { // SS3: Single Shift Three "\x1bO" (VT100 function keys/keypad)
			n3, _ := syscall.Read(E.termFd, seq[1:2])
			if n3 == 0 {
				return 0, Key_Escape, result_ok
			} // Incomplete sequence
			switch seq[1] {
			case 'H': return 0, Key_Home, result_ok // Num lock off Home?
			case 'F': return 0, Key_End, result_ok // Num lock off End?
				// case 'P','Q','R','S': // F1-F4? Ignore for now
			}
		}
		// If sequence wasn't recognized, return plain Escape
		return 0, Key_Escape, result_ok

	} else if c == 127 || c == 8 { // ASCII DEL (127) or Backspace (8)
		return 0, Key_Backspace, result_ok
	} else if c == 13 { // Carriage Return (Enter)
		return 0, Key_Enter, result_ok
	} else if c == 10 { // Line Feed (often behaves like Enter in raw mode)
		return 0, Key_Enter, result_ok
	} else if c == 9 { // Tab
		return rune(c), Key_Null, result_ok // Return Tab as a rune
	} else if c < 32 { // Other Control character (0-31, excluding BS, TAB, LF, CR)
		return 0, EditorKey(c), result_ok // Map to Ctrl_* enum value
	} else { // Printable character (ASCII or start of UTF-8)
		// This assumes the terminal sends valid UTF-8, but we only read one byte.
		// Multi-byte characters will arrive as separate calls to readKey.
		// This editor currently doesn't handle combining them.
		return rune(c), Key_Null, result_ok
	}
}

// --- Buffer Operations ---

// Helper: Shift runes in a row right, making space at startIdx. length is current length.
func shiftRunesRight(row *[MAX_COLS]rune, length int, startIdx int) {
	if length < MAX_COLS && startIdx <= length {
		// Manual loop to avoid potential issues with copy on stack arrays
		for i := length; i > startIdx; i-- {
			row[i] = row[i-1]
		}
		// Clear the opened space (optional, good practice)
		row[startIdx] = 0
	}
}

// Helper: Shift runes in a row left, overwriting startIdx. length is current length.
func shiftRunesLeft(row *[MAX_COLS]rune, length int, startIdx int) {
	if startIdx < length && length > 0 {
		// Manual loop
		for i := startIdx; i < length-1; i++ {
			row[i] = row[i+1]
		}
		// Clear the last element (optional)
		row[length-1] = 0
	}
}

// Helper: Shift lines[startY:] and lineLengths[startY:] down by one. numLines is current count.
func shiftLinesDown(buf *EditorBuffer, startY int) {
	if buf.numLines < MAX_LINES && startY <= buf.numLines {
		// Manual loop from end towards startY
		for i := buf.numLines; i > startY; i-- {
			buf.lines[i] = buf.lines[i-1]         // Array copy
			buf.lineLengths[i] = buf.lineLengths[i-1]
		}
		// Clear the opened line (optional, good practice)
		buf.lineLengths[startY] = 0
		clear(buf.lines[startY][:])
	}
}

// Helper: Shift lines[startY+1:] and lineLengths[startY+1:] up by one, overwriting startY. numLines is current count.
func shiftLinesUp(buf *EditorBuffer, startY int) {
	if startY < buf.numLines-1 {
		// Manual loop from startY to end-1
		for i := startY; i < buf.numLines-1; i++ {
			buf.lines[i] = buf.lines[i+1]         // Array copy
			buf.lineLengths[i] = buf.lineLengths[i+1]
		}
		// Clear the last line that is now unused (optional)
		lastIdx := buf.numLines - 1
		buf.lineLengths[lastIdx] = 0
		clear(buf.lines[lastIdx][:])
	} else if startY == buf.numLines - 1 { // Deleting the only line left after the shift target
         // Just clear the line at startY if needed (shift target was the last line)
         buf.lineLengths[startY] = 0
         clear(buf.lines[startY][:])
    }
}

// Inserts rune r at position (y, x). x is the rune index.
func bufferInsertRune(buf *EditorBuffer, y, x int, r rune) Result {
	if y < 0 || y >= buf.numLines || x < 0 || x > buf.lineLengths[y] {
		return result_err // Invalid position
	}
	if buf.lineLengths[y] >= MAX_COLS {
		setStatusMessage("Line length limit reached")
		return result_err // Line full
	}

	line := &buf.lines[y]
	length := buf.lineLengths[y]

	shiftRunesRight(line, length, x)
	line[x] = r
	buf.lineLengths[y]++
	buf.dirty = true
	return result_ok
}

// Deletes rune AT index x in line y.
func bufferDeleteRune(buf *EditorBuffer, y, x int) Result {
	if y < 0 || y >= buf.numLines || x < 0 || x >= buf.lineLengths[y] {
		return result_err // Invalid position or nothing to delete
	}

	line := &buf.lines[y]
	length := buf.lineLengths[y]

	shiftRunesLeft(line, length, x)
	buf.lineLengths[y]--
	buf.dirty = true
	return result_ok
}

// Inserts a newline at (y, x), splitting the line. Content from x onwards moves to the new line (y+1).
func bufferInsertNewline(buf *EditorBuffer, y, x int) Result {
	// Allow inserting newline at y == buf.numLines (effectively appending a new line)
	// AND allow inserting at y < buf.numLines (splitting an existing line).
	if y < 0 || y > buf.numLines || x < 0 {
		return result_err
	}

	// Special case: Insert newline past the end of the buffer content (appending a new line)
	if y == buf.numLines {
		if buf.numLines >= MAX_LINES {
			setStatusMessage("Buffer limit reached")
			return result_err
		}
		// Ensure x is 0 when appending a new line conceptually
		// We could allow splitting the "virtual" line past the last line, but let's simplify.
		if x != 0 {
			// Or should this add spaces? Let's restrict to x=0 for appending.
			return result_err // Append requires x=0
		}
		// Add a new empty line at the end
		buf.lineLengths[buf.numLines] = 0
		clear(buf.lines[buf.numLines][:]) // Ensure it's clear
		buf.numLines++
		buf.dirty = true
		return result_ok
	}

	// Normal case: Splitting line y
	if x > buf.lineLengths[y] { // Can't split past end of existing line content
		return result_err
	}
	if buf.numLines >= MAX_LINES {
		setStatusMessage("Buffer limit reached")
		return result_err
	}

	// Make space for the new line at y+1
	shiftLinesDown(buf, y+1)

	// Get pointers/references to the affected lines
	oldLine := &buf.lines[y]
	newLine := &buf.lines[y+1] // This is the newly created space
	oldLength := buf.lineLengths[y]
	numToMove := oldLength - x

	// Copy content from x onwards in oldLine to the beginning of newLine
	if numToMove > 0 {
		copy(newLine[:numToMove], oldLine[x:oldLength]) // Copy runes
	}

	// Update lengths
	buf.lineLengths[y+1] = numToMove
	buf.lineLengths[y] = x

	// Clear the moved part in the old line (optional, for tidiness)
	if numToMove > 0 {
		clear(oldLine[x:oldLength])
	}

	buf.numLines++
	buf.dirty = true
	return result_ok
}

// Deletes the entire line y.
func bufferDeleteLine(buf *EditorBuffer, y int) Result {
	if y < 0 || y >= buf.numLines {
		return result_err // Invalid line index
	}

	// Shift lines below y upwards
	shiftLinesUp(buf, y)

	// Decrease line count
	buf.numLines--
	buf.dirty = true
	return result_ok
}

// Merges line y+1 into line y (deletes the newline between them).
func bufferMergeLines(buf *EditorBuffer, y int) Result {
	if y < 0 || y >= buf.numLines-1 { // Need line y and y+1 to exist
		return result_err
	}

	line1Len := buf.lineLengths[y]
	line2Len := buf.lineLengths[y+1]

	if line1Len+line2Len > MAX_COLS {
		setStatusMessage("Cannot merge: Result exceeds max line length")
		return result_err
	}

	// Append content of line y+1 to line y
	line1 := &buf.lines[y]
	line2 := buf.lines[y+1] // Get a copy of line y+1's array
	copy(line1[line1Len:], line2[:line2Len])
	buf.lineLengths[y] = line1Len + line2Len

	// Delete line y+1 by shifting subsequent lines up
	shiftLinesUp(buf, y+1) // Shift starting from y+1
	buf.numLines--
	buf.dirty = true
	return result_ok
}

// --- File I/O ---

// Opens file, reads content into buffer. Handles file not found gracefully.
// Uses fixed buffer for reading, manual UTF-8 decoding.
func editorOpenFile(filepath string) Result {
	file, err := os.Open(filepath)
	if err != nil {
		if os.IsNotExist(err) {
			// File not found is okay, start with empty buffer but set filename
			setFilename(filepath)
			E.buffer.numLines = 0
			E.buffer.dirty = false // New file isn't dirty
			setStatusMessage("New file or file not found")
			return result_ok
		}
		// Other error opening file
		setStatusMessage("Error opening file")
		return result_err
	}
	defer file.Close()

	setFilename(filepath)
	E.buffer.numLines = 0
	var readBuf [4096]byte // ~4KB read buffer
	currentLine := 0
	currentCol := 0
	bufPos := 0 // Current position within readBuf
	bufLen := 0 // Number of bytes currently in readBuf
	eofReached := false

	// Ensure first line exists conceptually before reading into it
	if MAX_LINES > 0 {
		clear(E.buffer.lines[0][:])
		E.buffer.lineLengths[0] = 0
	} else {
		return result_err // Cannot work with MAX_LINES = 0
	}

	for currentLine < MAX_LINES {
		// Refill readBuf if empty and EOF not reached
		if bufPos >= bufLen && !eofReached {
			n, readErr := file.Read(readBuf[:])
			// Handle EOF correctly. io.EOF is the standard way, but we avoid 'io'.
			// A zero read count (n=0) with no error (readErr=nil) indicates EOF.
			// An error might also occur *at* EOF, potentially masking it.
			if readErr != nil {
				// Check if it's a common EOF representation? Difficult without 'io'.
				// Assume any error after potential reads means trouble or EOF.
				eofReached = true // Assume EOF on error to stop reading
				if n == 0 {
					// No bytes read and an error -> Likely EOF or read error
				} else {
					// Bytes read AND an error -> Process bytes, then likely stop.
					// Or maybe log the error? For now, treat as EOF after processing.
				}
			} else if n == 0 { // Read 0 bytes without error definitely means EOF
				eofReached = true
			}

			bufLen = n
			bufPos = 0

			// If we refilled and still have no data (and hit EOF), break.
			if bufLen == 0 && eofReached {
				break
			}
		}

		// Find next newline or end of buffer
		lineEnd := -1
		for i := bufPos; i < bufLen; i++ {
			if readBuf[i] == '\n' {
				lineEnd = i
				break
			}
		}

		segmentEnd := 0
		foundNewline := false
		if lineEnd != -1 { // Found newline
			segmentEnd = lineEnd
			foundNewline = true
		} else { // No newline in current buffer chunk
			segmentEnd = bufLen // Process up to end of buffer
		}

		// Process the segment readBuf[bufPos:segmentEnd]
		segment := readBuf[bufPos:segmentEnd]
		byteIdx := 0
		for byteIdx < len(segment) {
			r, size := utf8.DecodeRune(segment[byteIdx:])
			byteIdx += size // Move past the rune bytes regardless

			if r == utf8.RuneError && size == 1 {
				r = '?' // Replace invalid UTF-8 byte with '?'
			}
			// Skip Carriage Return characters
			if r == '\r' {
				continue
			}

			// Check if line buffer is full before adding rune
			if currentCol >= MAX_COLS {
				// Line is full, stop adding runes to this line.
				// We should ideally still consume the rest of the bytes in the segment
				// corresponding to this logical line in the file until the newline.
				// This simple version will just stop processing the rest of the segment
				// for this line and proceed to the newline handling.
				// Mark line as truncated potentially?
				setStatusMessage("Warning: Line truncated (MAX_COLS reached)")
				// Skip remaining bytes in segment until newline or segment end
				byteIdx = len(segment) // Force exit from inner loop
				continue // Skip adding the rune
			}

			E.buffer.lines[currentLine][currentCol] = r
			currentCol++
		}
		// currentCol now holds the length of the line (or max cols)

		// Advance buffer position past the processed segment
		bufPos = segmentEnd
		if foundNewline {
			bufPos++ // Skip the newline character itself

			// Finalize the current line length
			E.buffer.lineLengths[currentLine] = currentCol

			// Move to the next line
			currentLine++
			currentCol = 0

			// Check if we exceeded max lines buffer capacity
			if currentLine >= MAX_LINES {
				setStatusMessage("File truncated: Max lines reached")
				eofReached = true // Stop reading further
				break
			}

			// Initialize the new current line
			clear(E.buffer.lines[currentLine][:])
			E.buffer.lineLengths[currentLine] = 0
		}
		// If no newline found, loop continues to refill buffer or handle EOF
	}

	// Handle the very last line if file didn't end with newline or was truncated mid-line
	// Ensure the last processed line's length is recorded.
	if currentLine < MAX_LINES {
		E.buffer.lineLengths[currentLine] = currentCol
		// Increment numLines only if the last line had content or was explicitly added.
		// If currentCol > 0, it means the loop added characters to currentLine.
		// If the file was empty, currentLine=0, currentCol=0, numLines should be 0.
		// If the file ended exactly at a newline, currentLine would have incremented,
		// and the next line (empty) length is correctly 0.
		if currentCol > 0 {
			E.buffer.numLines = currentLine + 1
		} else {
			// If the last line is empty (currentCol == 0), the number of lines is just currentLine.
			E.buffer.numLines = currentLine
		}
		// Special case: Empty file? numLines should be 0.
		// Let's refine: numLines should be the index of the *next* available line slot.
		// If we processed line 0 and found a newline, currentLine becomes 1. numLines should be 1.
		// If we processed line 0 and hit EOF without newline, currentLine is 0. numLines depends on currentCol.
		if eofReached {
			if currentLine < MAX_LINES { // Ensure we don't overrun buffer if MAX_LINES was reached
				if currentCol > 0 {
					E.buffer.numLines = currentLine + 1
				} else {
					// If last line had 0 length, numLines is currentLine index
					// unless currentLine is 0 (empty file case)
					if currentLine == 0 && E.buffer.lineLengths[0] == 0 {
						E.buffer.numLines = 0 // Empty file
					} else {
						E.buffer.numLines = currentLine
					}
				}
			} else {
				// If MAX_LINES was reached, numLines is MAX_LINES
				E.buffer.numLines = MAX_LINES
			}
		}

	} else { // MAX_LINES reached exactly
		E.buffer.numLines = MAX_LINES
	}

	// Correct numLines for completely empty file
	if E.buffer.numLines == 1 && E.buffer.lineLengths[0] == 0 {
		E.buffer.numLines = 0
	}


	E.buffer.dirty = false // File just loaded isn't dirty
	return result_ok
}

// Saves buffer content to the current filename.
func editorSaveFile() Result {
	if E.buffer.filenameLen == 0 {
		// TODO: Implement prompting for a filename if none exists
		setStatusMessage("No filename specified for saving")
		return result_err
	}

	filenameBytes := E.buffer.filename[:E.buffer.filenameLen]
	// Convert fixed byte array slice to string for OpenFile. This might allocate.
	// Alternative: Use syscall.Open with byte slice? More complex. Accept allocation here.
	filenameStr := string(filenameBytes)

	// Use O_WRONLY instead of O_RDWR if read access isn't needed.
	file, err := os.OpenFile(filenameStr, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		setStatusMessage("Error opening file for writing")
		return result_err
	}
	defer file.Close()

	var writeBuf [MAX_COLS * utf8.UTFMax]byte // Generous buffer for UTF-8 encoding

	totalBytesWritten := 0 // Track total bytes for status message

	for y := 0; y < E.buffer.numLines; y++ {
		lineRunes := E.buffer.lines[y][:E.buffer.lineLengths[y]]
		bytePos := 0
		// Encode line runes to UTF-8 bytes in writeBuf
		for _, r := range lineRunes {
			// Check buffer space before encoding (should be ample)
			if bytePos >= len(writeBuf)-utf8.UTFMax {
				setStatusMessage("Internal error: Line encoding buffer overflow")
				return result_err
			}
			bytePos += utf8.EncodeRune(writeBuf[bytePos:], r)
		}

		// Write encoded line bytes to file using a loop for robustness
		bytesToWrite := writeBuf[:bytePos]
		lineBytesWritten := 0
		for len(bytesToWrite) > 0 {
			n, writeErr := file.Write(bytesToWrite)
			if n > 0 {
				lineBytesWritten += n
				bytesToWrite = bytesToWrite[n:]
			}
			if writeErr != nil {
				// Handle EINTR? Less common for file writes.
				setStatusMessage("Error writing line to file")
				return result_err
			}
			if n == 0 && len(bytesToWrite) > 0 { // Should not happen?
				setStatusMessage("Error: Wrote 0 bytes writing line")
				return result_err
			}
		}
		totalBytesWritten += lineBytesWritten

		// Write newline character after each line, also with robustness loop
		nlBytes := []byte{'\n'}
		nlBytesWritten := 0
		for len(nlBytes) > 0 {
			n, nlErr := file.Write(nlBytes)
			if n > 0 {
				nlBytesWritten += n
				nlBytes = nlBytes[n:]
			}
			if nlErr != nil {
				setStatusMessage("Error writing newline to file")
				return result_err
			}
			if n == 0 && len(nlBytes) > 0 {
				setStatusMessage("Error: Wrote 0 bytes writing newline")
				return result_err
			}
		}
		totalBytesWritten += nlBytesWritten
	}

	E.buffer.dirty = false
	// Set status message with bytes written count
	var statusBuf [STATUS_MSG_BUF_SIZE]byte
	msgSlice := statusBuf[:0]
	msgSlice = appendString(msgSlice, "Wrote ")
	msgSlice = itoaAppend(msgSlice, totalBytesWritten)
	msgSlice = appendString(msgSlice, " bytes to ")
	// Append filename safely (truncate if needed)
	fnameLen := E.buffer.filenameLen
	availableSpace := cap(msgSlice) - len(msgSlice)
	if fnameLen > availableSpace {
		fnameLen = availableSpace - 3 // Make space for "..."
		if fnameLen < 0 { fnameLen = 0 } // Handle edge case
		msgSlice = appendBytes(msgSlice, E.buffer.filename[:fnameLen])
		msgSlice = appendString(msgSlice, "...")
	} else {
		msgSlice = appendBytes(msgSlice, E.buffer.filename[:fnameLen])
	}

	// Copy final message to E.statusMsg
	copiedLen := copy(E.statusMsg[:], msgSlice)
	clear(E.statusMsg[copiedLen:])
	E.statusMsgLen = copiedLen

	return result_ok
}

// --- Input Processing ---

// Calculates the render column (0-based) for a given rune index (0-based) in a line.
func getRenderPosFromRuneIndex(line []rune, length, runeIndex int) int {
	renderPos := 0
	clampedIndex := min(runeIndex, length) // Don't calculate past actual length
	for i := 0; i < clampedIndex; i++ {
		if line[i] == '\t' {
			renderPos += TAB_STOP - (renderPos % TAB_STOP)
		} else {
			// Assume printable chars have width 1. Needs refinement for CJK etc.
			// Basic check for printable range (could be improved)
            if line[i] >= 32 && line[i] != 127 {
                renderPos++
            } else {
				// Treat non-printable as width 1 (e.g., shown as '?') or 0? Let's use 1 for now.
                renderPos++
            }
		}
	}
	return renderPos
}

// Updates E.renderX based on E.cursorX and tabs in the current line.
func updateRenderX() {
	if E.cursorY >= 0 && E.cursorY < E.buffer.numLines {
		line := E.buffer.lines[E.cursorY]
		length := E.buffer.lineLengths[E.cursorY]
		// Ensure cursorX doesn't exceed line length when calculating render pos
		E.renderX = getRenderPosFromRuneIndex(line[:length], length, E.cursorX)
	} else {
		E.renderX = 0 // No line, render pos is 0
	}
}

// Move cursor based on key command, handling wrapping and line boundaries.
func editorMoveCursor(key EditorKey) {
	currentY := E.cursorY
	currentX := E.cursorX
	lineLength := 0
	if currentY >= 0 && currentY < E.buffer.numLines {
		lineLength = E.buffer.lineLengths[currentY]
	}

	switch key {
	case Key_ArrowLeft, Ctrl_B: // Ctrl+B often behaves like Left
		if currentX > 0 {
			E.cursorX--
		} else if currentY > 0 { // Wrap to end of previous line
			E.cursorY--
			E.cursorX = E.buffer.lineLengths[E.cursorY]
		}
	case Key_ArrowRight, Ctrl_F: // Ctrl+F often behaves like Right
		if currentY < E.buffer.numLines && currentX < lineLength {
			E.cursorX++
		} else if currentY < E.buffer.numLines-1 { // Wrap to start of next line
			E.cursorY++
			E.cursorX = 0
		}
	case Key_ArrowUp, Ctrl_P: // Ctrl+P often behaves like Up
		if currentY > 0 {
			E.cursorY--
			// Adjust X to be within the new line's bounds ("sticky" column)
			newLength := E.buffer.lineLengths[E.cursorY]
			E.cursorX = min(E.cursorX, newLength) // Use min for sticky behavior
		}
	case Key_ArrowDown, Ctrl_N: // Ctrl+N often behaves like Down
		if currentY < E.buffer.numLines-1 {
			E.cursorY++
			// Adjust X to be within the new line's bounds
			newLength := E.buffer.lineLengths[E.cursorY]
			E.cursorX = min(E.cursorX, newLength) // Use min for sticky behavior
		}
	case Key_Home, Ctrl_A: // Ctrl+A often behaves like Home
		E.cursorX = 0
	case Key_End, Ctrl_E: // Ctrl+E often behaves like End
		if currentY < E.buffer.numLines {
			E.cursorX = E.buffer.lineLengths[currentY]
		} else {
			E.cursorX = 0 // End of non-existent line is 0
		}
	case Key_PageUp:
		// Move cursor Y first
		E.cursorY -= E.screenRows
		if E.cursorY < 0 { E.cursorY = 0 }
		// Adjust rowOffset based on new cursor Y
		if E.cursorY < E.rowOffset {
			E.rowOffset = E.cursorY
		}
		// Adjust X
		if E.cursorY < E.buffer.numLines {
			newLength := E.buffer.lineLengths[E.cursorY]
			E.cursorX = min(E.cursorX, newLength)
		} else { E.cursorX = 0 }

	case Key_PageDown:
		// Move cursor Y first
		maxY := max(0, E.buffer.numLines-1)
		E.cursorY += E.screenRows
		if E.cursorY > maxY { E.cursorY = maxY }
		// Adjust rowOffset based on new cursor Y
		if E.cursorY >= E.rowOffset + E.screenRows {
			E.rowOffset = E.cursorY - E.screenRows + 1
			// Ensure rowOffset doesn't go too far if numLines is small
			maxRowOffset := max(0, E.buffer.numLines-E.screenRows)
			E.rowOffset = min(E.rowOffset, maxRowOffset)
		}
		// Adjust X
		if E.cursorY < E.buffer.numLines {
            newLength := E.buffer.lineLengths[E.cursorY]
            E.cursorX = min(E.cursorX, newLength)
        } else { E.cursorX = 0 }

	}

	// Final check: Ensure cursor X is valid for the potentially new line Y after any move.
	// This handles cases where PgUp/PgDn land on shorter lines or buffer is empty.
	if E.cursorY >= 0 && E.cursorY < E.buffer.numLines {
		finalLength := E.buffer.lineLengths[E.cursorY]
		if E.cursorX > finalLength {
			E.cursorX = finalLength
		}
	} else if E.buffer.numLines == 0 { // If buffer is empty, cursor must be 0,0
		E.cursorX = 0
		E.cursorY = 0
	} else {
         // If cursorY somehow went out of bounds (e.g. negative?), clamp it.
         if E.cursorY < 0 { E.cursorY = 0 }
		 // Clamp Y to last valid line index
		 maxY := max(0, E.buffer.numLines-1)
         if E.cursorY > maxY { E.cursorY = maxY }
         // Recalculate length for clamped Y and clamp X
         finalLength := E.buffer.lineLengths[E.cursorY]
         if E.cursorX > finalLength { E.cursorX = finalLength }
    }
}

// Process keypress in Normal mode
func editorProcessNormalMode(r rune, key EditorKey) Result {
	// Reset quit confirmation unless Ctrl+Q is pressed again
	if key != Ctrl_Q {
		E.quitConfirm = 0
	}

	switch key {
	case Key_Null: // Check rune for commands
		switch r {
		case 'i': // Enter Insert mode at cursor
			E.mode = Mode_Insert
			setStatusMessage("-- INSERT --")
		case 'a': // Enter Insert mode after cursor
			if E.cursorY < E.buffer.numLines && E.cursorX < E.buffer.lineLengths[E.cursorY] {
				E.cursorX++ // Move right only if not at end of line
			}
			E.mode = Mode_Insert
			setStatusMessage("-- INSERT --")
		case 'o': // Open line below
			// Insert newline *after* current line (y+1), at column 0
			res := result_ok
			if E.buffer.numLines == 0 { // Special case: buffer empty
				res = bufferInsertNewline(&E.buffer, 0, 0) // Insert first line
				if res == result_ok {
					// Still need to insert the second line
					res = bufferInsertNewline(&E.buffer, 1, 0)
					if res == result_ok {
						E.cursorY = 1 // Move to the newly opened second line
						E.cursorX = 0
					}
				}
			} else {
				res = bufferInsertNewline(&E.buffer, E.cursorY+1, 0)
				if res == result_ok {
					E.cursorY++ // Move cursor to the new line
					E.cursorX = 0
				}
			}

			if res == result_ok {
				E.mode = Mode_Insert
				setStatusMessage("-- INSERT --")
			}
		case 'O': // Open line above
			// Insert newline *at* current line index (y), at column 0
			res := bufferInsertNewline(&E.buffer, E.cursorY, 0)
			if res == result_ok {
				// Cursor stays at the original line index (which is now the new line)
				// No need to move cursorY.
				E.cursorX = 0
				E.mode = Mode_Insert
				setStatusMessage("-- INSERT --")
			}
		case 'x': // Delete character under cursor
			if E.cursorY < E.buffer.numLines && E.cursorX < E.buffer.lineLengths[E.cursorY] {
				bufferDeleteRune(&E.buffer, E.cursorY, E.cursorX)
				// Cursor stays at same position unless it was the last char
				// No adjustment needed here, subsequent moves handle boundary.
			}
		case 'd': // Potential 'dd' command
			// Wait for the next key, expecting another 'd'
			setStatusMessage("d_") // Indicate pending command
			editorRefreshScreen()  // Show the status message immediately
			nr, nkey, nres := readKey()
			if nres == result_ok && nkey == Key_Null && nr == 'd' {
				// Got 'dd' - Delete current line
				if E.buffer.numLines > 0 && E.cursorY < E.buffer.numLines {
					savedX := E.cursorX // Try to maintain column
					if bufferDeleteLine(&E.buffer, E.cursorY) == result_ok {
						// Clamp cursor Y if it's now past the end
						if E.cursorY >= E.buffer.numLines && E.buffer.numLines > 0 {
							E.cursorY = E.buffer.numLines - 1
						} else if E.buffer.numLines == 0 { // If buffer becomes empty
                            E.cursorY = 0
                        }

						// Try to restore X, clamped to new line length
						if E.cursorY >= 0 && E.cursorY < E.buffer.numLines {
							newLength := E.buffer.lineLengths[E.cursorY]
							E.cursorX = min(savedX, newLength)
						} else { // Buffer became empty
							E.cursorX = 0
						}
						setStatusMessage("Line deleted")
					}
				} else {
					setStatusMessage("") // Nothing to delete
				}
			} else {
				// Not 'dd', cancel operation
				setStatusMessage("")
				// Process the second key (nr, nkey) normally? Vim ignores. Let's ignore.
			}
			return result_ok // Handled the 'd' sequence

		// Movement runes
		case 'j': editorMoveCursor(Key_ArrowDown)
		case 'k': editorMoveCursor(Key_ArrowUp)
		case 'h': editorMoveCursor(Key_ArrowLeft)
		case 'l': editorMoveCursor(Key_ArrowRight)
		case '0': E.cursorX = 0 // Move to beginning of line (column 0)
		case '$': // Move to end of line
			 if E.cursorY < E.buffer.numLines {
				 E.cursorX = E.buffer.lineLengths[E.cursorY]
			 } else {
				 E.cursorX = 0
			 }

		case ':':
            // Command mode not implemented
            setStatusMessage("Command mode ':' not implemented")

		}

	// Movement Keys
	case Key_ArrowDown, Key_ArrowUp, Key_ArrowLeft, Key_ArrowRight,
		Key_Home, Key_End, Key_PageUp, Key_PageDown,
		Ctrl_B, Ctrl_F, Ctrl_P, Ctrl_N, Ctrl_A, Ctrl_E: // Include Ctrl movement keys
		editorMoveCursor(key)

	// Control Keys
	case Ctrl_Q: // Quit
		if E.buffer.dirty && E.quitConfirm < QUIT_TIMES-1 {
			setStatusMessage("Unsaved changes! Press Ctrl+Q again to quit.")
			E.quitConfirm++
			return result_ok // Don't quit yet
		}
		E.shouldQuit = true // Signal main loop to exit
		return result_ok     // Let main loop handle exit

	case Ctrl_S: // Save
		editorSaveFile() // Status message set by save function
		return result_ok

	case Key_Escape:
		// Already in normal mode, do nothing (or beep?)
		// Could potentially clear status message or pending commands.
		setStatusMessage("")
		break

	case Key_Backspace: // Typical normal mode BS moves left
		editorMoveCursor(Key_ArrowLeft)
		break

	case Key_Enter: // Typical normal mode Enter moves down
		editorMoveCursor(Key_ArrowDown)
		// Vim also moves to first non-whitespace char, simplify to col 0
		E.cursorX = 0
		break

	case Ctrl_L: // Often redraw screen command
        // The main loop redraws anyway, but could force clear/redraw if needed
        // No action needed here for basic version
        break

	// Ignore other keys/runes in normal mode for now
	}

	return result_ok
}

// Process keypress in Insert mode
func editorProcessInsertMode(r rune, key EditorKey) Result {
	switch key {
	case Key_Escape:
		E.mode = Mode_Normal
		setStatusMessage("") // Clear "-- INSERT --" message
		// Vim moves cursor left when exiting insert mode if not at col 0 and possible
		if E.cursorX > 0 && E.cursorY < E.buffer.numLines {
			// Check if cursorX is valid for the line length before decrementing
			// This handles exiting insert mode immediately after 'a' at end of line.
			lineLen := E.buffer.lineLengths[E.cursorY]
			if E.cursorX <= lineLen { // Allow cursor at len (after last char)
				E.cursorX--
			}
			// Clamp just in case
			if E.cursorX < 0 { E.cursorX = 0 }
		}
		return result_ok

	case Key_Enter, Ctrl_M, Ctrl_J: // Handle common Enter keys
		// Handle empty buffer case for enter
		if E.buffer.numLines == 0 {
			if bufferInsertNewline(&E.buffer, 0, 0) == result_ok { // Insert first line
				E.cursorY = 0 // Stay on line 0
				E.cursorX = 0
				// Now insert the second line by splitting the (now existing) first line
				if bufferInsertNewline(&E.buffer, E.cursorY, E.cursorX) == result_ok {
					E.cursorY++ // Move to start of new line (line 1)
					E.cursorX = 0
				}
			}
		} else {
			if bufferInsertNewline(&E.buffer, E.cursorY, E.cursorX) == result_ok {
				E.cursorY++ // Move to start of new line
				E.cursorX = 0
			} // Error message set by buffer func if fail
		}
		return result_ok

	case Key_Backspace, Ctrl_H: // Handle common Backspace keys
		if E.cursorX > 0 {
			// Ensure cursor is within valid line bounds before deleting
			if E.cursorY < E.buffer.numLines {
				E.cursorX-- // Move cursor first
				bufferDeleteRune(&E.buffer, E.cursorY, E.cursorX)
			}
		} else if E.cursorY > 0 {
			// Backspace at start of line: merge with previous line
			prevLineLen := E.buffer.lineLengths[E.cursorY-1]
			if bufferMergeLines(&E.buffer, E.cursorY-1) == result_ok {
				E.cursorY--                // Move cursor up
				E.cursorX = prevLineLen // Move cursor to end of merged part
			}
		}
		return result_ok

	case Key_Delete:
		// Delete character AT cursor position (doesn't move cursor)
		if E.cursorY < E.buffer.numLines && E.cursorX < E.buffer.lineLengths[E.cursorY] {
			bufferDeleteRune(&E.buffer, E.cursorY, E.cursorX)
			// Cursor position stays same relative to content before it
		} else if E.cursorY < E.buffer.numLines-1 && E.cursorX == E.buffer.lineLengths[E.cursorY] {
			// If at end of line (and not last line), merge with next line
			bufferMergeLines(&E.buffer, E.cursorY)
            // Cursor stays at same X, Y position (now end of merged line)
		}
		return result_ok

	// Movement Keys (allowed in insert mode, but might be Vim-incompatible depending on settings)
	case Key_ArrowDown, Key_ArrowUp, Key_ArrowLeft, Key_ArrowRight,
		Key_Home, Key_End, Key_PageUp, Key_PageDown:
		editorMoveCursor(key)
		// Ensure cursor X is valid for the line after moving
		if E.cursorY >= 0 && E.cursorY < E.buffer.numLines {
			lineLen := E.buffer.lineLengths[E.cursorY]
			if E.cursorX > lineLen {
				E.cursorX = lineLen
			}
		} else if E.buffer.numLines == 0 { // Handle empty buffer
            E.cursorX = 0
            E.cursorY = 0
        }
		return result_ok

	case Key_Null: // Regular character typed
		if r != 0 { // Ensure it's not just a null timeout
			// Handle multi-byte UTF-8? Current readKey is limited. Assume 'r' is a valid rune.
			// Handle Tab specifically? Expand to spaces? For now, insert literal Tab rune.

			// Handle inserting into an empty buffer
			if E.buffer.numLines == 0 {
				if bufferInsertNewline(&E.buffer, 0, 0) == result_ok { // Insert the first line
					// Now we can insert the rune
					if bufferInsertRune(&E.buffer, 0, 0, r) == result_ok {
						E.cursorY = 0
						E.cursorX = 1 // Move cursor right after inserting
					}
				}
			} else {
				// Insert into existing line
				if bufferInsertRune(&E.buffer, E.cursorY, E.cursorX, r) == result_ok {
					E.cursorX++ // Move cursor right after inserting
				} // Error msg set by buffer func if fail
			}
		}
		return result_ok

		// Ignore other control keys in insert mode for now (Ctrl+Q, Ctrl+S etc.)
	}

	return result_ok
}

// Main key processing dispatcher
func editorProcessKeypress() Result {
	r, key, res := readKey()
	if res == result_err {
		// Let main loop decide if this is fatal
		return result_err
	}
	if r == 0 && key == Key_Null {
		// Timeout, no input, just continue
		return result_ok
	}

	// If any key other than Ctrl+Q (in normal mode) was pressed, reset the quit confirmation counter
	// Check mode BEFORE resetting, as Ctrl-Q might be pressed in Insert mode where it shouldn't trigger quit confirm logic.
	if !(key == Ctrl_Q && E.mode == Mode_Normal) {
		E.quitConfirm = 0
	}

	switch E.mode {
	case Mode_Normal:
		return editorProcessNormalMode(r, key)
	case Mode_Insert:
		return editorProcessInsertMode(r, key)
	}
	return result_ok // Should not be reached
}

// --- Output / Rendering ---

// Scroll the view if cursor moved out of screen bounds. Also updates renderX.
func editorScroll() {
	// Update renderX based on cursorX before scrolling checks
	updateRenderX()

	// Vertical scroll
	if E.cursorY < E.rowOffset {
		E.rowOffset = E.cursorY
	}
	// Ensure cursorY is visible on screen. If cursorY is at rowOffset + screenRows, it's off screen.
	if E.screenRows > 0 && E.cursorY >= E.rowOffset+E.screenRows {
		E.rowOffset = E.cursorY - E.screenRows + 1
	}

	// Horizontal scroll based on renderX
	if E.renderX < E.colOffset {
		E.colOffset = E.renderX
	}
	// Ensure renderX is visible. If renderX is at colOffset + screenCols, it's off screen.
	if E.screenCols > 0 && E.renderX >= E.colOffset+E.screenCols {
		E.colOffset = E.renderX - E.screenCols + 1
	}
}

// Append bytes without allocation if possible (relies on dst having capacity)
func appendBytes(dst []byte, src []byte) []byte {
	// Basic append, caller must ensure capacity
	return append(dst, src...)
}
func appendString(dst []byte, src string) []byte {
	// Basic append, caller must ensure capacity
	return append(dst, src...)
}
func appendRune(dst []byte, r rune) []byte {
	// Ensure capacity before encoding and appending
	if cap(dst)-len(dst) < utf8.UTFMax {
		// Not enough capacity, maybe return error or original slice?
		// For now, assume caller managed capacity.
		// If we wanted safety, we'd return (slice, bool) or similar.
		return dst
	}
	var encoded [utf8.UTFMax]byte
	n := utf8.EncodeRune(encoded[:], r)
	return append(dst, encoded[:n]...)
}

// Draws the rows of the text buffer onto the screen buffer `buf`.
func editorDrawRows(buf *[]byte) {
	screenBuf := *buf // Get current slice state
	for y := 0; y < E.screenRows; y++ {
		fileRow := y + E.rowOffset
		if fileRow >= E.buffer.numLines {
			// Draw empty line marker (tilde) or welcome message
			if E.buffer.numLines == 0 && y == E.screenRows/3 && E.screenCols > 10 {
				// Show welcome message on first third of screen if buffer empty
				welcome := "LKJSXCG Editor -- Version " + VERSION
				padding := (E.screenCols - len(welcome)) / 2
				if padding < 0 { padding = 0 }

				// Ensure buffer has capacity before appending potentially large strings/padding
				needed := 1 + padding + len(welcome) // Tilde + padding + message
				if cap(screenBuf)-len(screenBuf) >= needed {
					screenBuf = append(screenBuf, '~')
					// Add padding spaces
					for i := 0; i < padding-1 ; i++ { screenBuf = append(screenBuf, ' ') }
					// Append welcome string
					screenBuf = appendString(screenBuf, welcome)
				} else {
					// Not enough space, just draw tilde if possible
					if cap(screenBuf)-len(screenBuf) >= 1 {
						screenBuf = append(screenBuf, '~')
					}
				}
			} else {
				// Ensure capacity for tilde
				if cap(screenBuf)-len(screenBuf) >= 1 {
					screenBuf = append(screenBuf, '~')
				}
			}
		} else {
			// Draw line content, handling horizontal scroll and tabs
			line := E.buffer.lines[fileRow]
			length := E.buffer.lineLengths[fileRow]
			currentRenderCol := 0 // Tracks the logical render column position from start of line
			screenCol := 0      // Tracks the current column being drawn on the screen (0 to screenCols-1)

			for runeIdx := 0; runeIdx < length; runeIdx++ {
				r := line[runeIdx]
				runeToDraw := r
				runeDisplayWidth := 0

				if r == '\t' {
					runeDisplayWidth = TAB_STOP - (currentRenderCol % TAB_STOP)
				} else {
					// Assume printable chars width 1. Needs wcwidth for CJK.
					// Handle non-printable chars? Replace with '?'
					if r < 32 || r == 127 { // Basic check for control chars + DEL
                         runeToDraw = '?' // Replace non-printable with '?'
                         runeDisplayWidth = 1
                    } else {
                         runeDisplayWidth = 1 // Assume others are width 1
                    }
				}

                // Calculate the rune's render position range
                startRenderCol := currentRenderCol
                endRenderCol := currentRenderCol + runeDisplayWidth

                // Iterate through each column this rune/tab occupies
                for col := startRenderCol; col < endRenderCol; col++ {
                    // Is this column visible on the screen?
                    if col >= E.colOffset && screenCol < E.screenCols {
                        // Yes, determine what character to draw
						if r == '\t' {
							// Ensure buffer has capacity for space
							if cap(screenBuf)-len(screenBuf) < 1 { break }
							screenBuf = append(screenBuf, ' ')
						} else if col == startRenderCol { // Draw the actual (or replacement) rune only at its starting column
							// Ensure buffer has capacity for the rune
							needed := utf8.RuneLen(runeToDraw)
							if needed < 0 { needed = 1 } // Safety for bad runes
							if cap(screenBuf)-len(screenBuf) < needed { break }
							screenBuf = appendRune(screenBuf, runeToDraw)
						} else {
							// This case handles multi-column characters (if width > 1).
							// Since we assume width 1, this shouldn't be reached unless tabs span columns.
							// If tabs, we already drew spaces. If we supported wide chars, we'd draw space padding here.
							// Ensure capacity for space (safety)
							if cap(screenBuf)-len(screenBuf) < 1 { break }
							screenBuf = append(screenBuf, ' ')
						}
                        screenCol++ // Advance screen column index
                    }
                     // Exit inner loop early if screen is full
                    if screenCol >= E.screenCols { break }
                }

				currentRenderCol = endRenderCol // Advance the logical render column

                // Early exit from rune loop if we've filled the screen width
                if screenCol >= E.screenCols { break }
			}
		}

		// Clear rest of the line on screen (important for scrolling/deleting)
		// Ensure buffer has capacity
		if cap(screenBuf)-len(screenBuf) >= 3 {
			screenBuf = appendString(screenBuf, "\x1b[K")
		}
		// Move to next line, unless it's the last screen row
		if y < E.screenRows-1 {
			// Ensure buffer has capacity
			if cap(screenBuf)-len(screenBuf) >= 2 {
				screenBuf = appendString(screenBuf, "\r\n")
			}
		}
	}
	*buf = screenBuf // Update the slice header in the caller
}

// Draws the status bar at the bottom of the screen.
func editorDrawStatusBar(buf *[]byte) {
	screenBuf := *buf
	// Ensure capacity for style codes
	needed := 3 + 3 // \x1b[7m and \x1b[m
	if cap(screenBuf)-len(screenBuf) < needed { /* Handle full */ return }

	// Set style: inverted colors (negative)
	screenBuf = appendString(screenBuf, "\x1b[7m")

	// Left side: Mode, Filename, Dirty status
	modeStr := "[Normal]"
	if E.mode == Mode_Insert {
		modeStr = "[Insert]"
	}

	// Filename display (from fixed buffer)
	fnameBytes := E.buffer.filename[:E.buffer.filenameLen]
	fnameDisplayStr := "[No Name]"
    maxFnameLen := 20 // Max length for filename in status bar
	if E.buffer.filenameLen > 0 {
		fnameStr := string(fnameBytes) // Allocation here
        if E.buffer.filenameLen > maxFnameLen {
			// Use rune count for better UTF-8 truncation? More complex. Stick to bytes.
			fnameDisplayStr = fnameStr[:maxFnameLen-3] + "..."
        } else {
            fnameDisplayStr = fnameStr
        }
	}

	// Dirty status indicator
	dirtyMark := "     " // Pad to align right side info
	if E.buffer.dirty {
		dirtyMark = " [+] "
	}

	// Right side: Line/Total, Column (using renderX for visual column)
	lineInfoStr := "/"
	colInfoStr := ", "
	// Pre-render numbers to estimate length accurately
	var lineNumBuf, totalLineBuf, colNumBuf [20]byte
	lineNumSlice := itoaAppend(lineNumBuf[:0], E.cursorY+1)          // 1-based line number
	totalLineSlice := itoaAppend(totalLineBuf[:0], E.buffer.numLines) // Total lines
	colNumSlice := itoaAppend(colNumBuf[:0], E.renderX+1)           // 1-based render column

	// Calculate lengths
	leftInfoLen := len(modeStr) + 1 + len(fnameDisplayStr) + len(dirtyMark)
	rightInfoLen := 1 + len(lineNumSlice) + len(lineInfoStr) + len(totalLineSlice) + len(colInfoStr) + len(colNumSlice) + 1

	// Calculate padding space
	space := E.screenCols - leftInfoLen - rightInfoLen
	if space < 0 {
		space = 0
	}

	// Ensure capacity for the whole status bar string
	needed = leftInfoLen + space + rightInfoLen
	if cap(screenBuf)-len(screenBuf) >= needed {
		// Append left info
		screenBuf = appendString(screenBuf, modeStr)
		screenBuf = append(screenBuf, ' ')
		screenBuf = appendString(screenBuf, fnameDisplayStr)
		screenBuf = appendString(screenBuf, dirtyMark)

		// Fill middle with spaces
		for i := 0; i < space; i++ {
			screenBuf = append(screenBuf, ' ')
		}

		// Append right info
		screenBuf = append(screenBuf, ' ') // Space before numbers
		screenBuf = appendBytes(screenBuf, lineNumSlice)
		screenBuf = appendString(screenBuf, lineInfoStr)
		screenBuf = appendBytes(screenBuf, totalLineSlice)
		screenBuf = appendString(screenBuf, colInfoStr)
		screenBuf = appendBytes(screenBuf, colNumSlice)
		screenBuf = append(screenBuf, ' ') // Space after numbers

	} else {
		// Not enough space, draw truncated left side only if possible
		truncLen := E.screenCols - 3 // Space for reset code
		if truncLen < 0 { truncLen = 0 }
		if cap(screenBuf) - len(screenBuf) >= truncLen {
			currentLen := 0
			if currentLen + len(modeStr) <= truncLen {
				screenBuf = appendString(screenBuf, modeStr)
				currentLen += len(modeStr)
			}
			// Add more components if space allows... (simplified: just show mode)
			// Fill rest with spaces
			for i := currentLen; i < truncLen; i++ {
				screenBuf = append(screenBuf, ' ')
			}
		}
	}


	// Reset colors/style to default
	screenBuf = appendString(screenBuf, "\x1b[m")
	// Add newline for message bar below status bar
	// Ensure capacity
	if cap(screenBuf)-len(screenBuf) >= 2 {
		screenBuf = appendString(screenBuf, "\r\n")
	}
	*buf = screenBuf
}

// Draws the message bar (last screen line).
func editorDrawMessageBar(buf *[]byte) {
	screenBuf := *buf
	// Ensure capacity for clear code + message
	needed := 3 + E.statusMsgLen
	if cap(screenBuf)-len(screenBuf) >= needed {
		// Clear the line first
		screenBuf = appendString(screenBuf, "\x1b[K")

		// Draw status message if present
		if E.statusMsgLen > 0 {
			// Ensure message fits screen width, truncate if necessary
			msgLen := E.statusMsgLen
			if msgLen > E.screenCols {
				msgLen = E.screenCols
			}
			screenBuf = appendBytes(screenBuf, E.statusMsg[:msgLen])
		}
	} else {
		// Not enough space, clear if possible
		if cap(screenBuf)-len(screenBuf) >= 3 {
			screenBuf = appendString(screenBuf, "\x1b[K")
		}
	}
	// TODO: Add timer to clear message? Requires 'time' package.
	*buf = screenBuf
}

// Refreshes the screen: clears, draws rows, status bar, message bar, positions cursor.
func editorRefreshScreen() {
	// Only perform screen operations if it's a TTY
	if !E.isTTY {
		// Maybe print a message indicating non-interactive mode?
		// For now, do nothing if not a TTY.
		return
	}

	editorScroll() // Adjust viewport (rowOffset, colOffset) based on cursor

	// Use stack buffer ~64KB. Ensure large enough!
	var screenBuf [64 * 1024]byte
	buf := screenBuf[:0]          // Create slice backed by the array

	// ANSI Escape Codes:
	// Hide cursor: \x1b[?25l
	// Move cursor to top-left (1,1): \x1b[H
	// Show cursor: \x1b[?25h
	// Clear screen: \x1b[2J (Use if needed, but drawing+K should suffice)
	// Clear line from cursor to end: \x1b[K

	// Ensure capacity for initial escape codes
	if cap(buf)-len(buf) < 6 { /* Handle full */ return }
	buf = appendString(buf, "\x1b[?25l") // Hide cursor during redraw
	buf = appendString(buf, "\x1b[H")   // Move to home position

	editorDrawRows(&buf)       // Draw text buffer content
	editorDrawStatusBar(&buf)  // Draw status bar
	editorDrawMessageBar(&buf) // Draw message bar

	// Position cursor: Calculate screen row/col (1-based) relative to viewport
	cursorScreenY := 1
	if E.screenRows > 0 { // Avoid division by zero or negative rows
		cursorScreenY = (E.cursorY - E.rowOffset) + 1
	}
	cursorScreenX := 1
	if E.screenCols > 0 {
		cursorScreenX = (E.renderX - E.colOffset) + 1
	}


	// Clamp cursor position to screen boundaries
	cursorScreenY = max(1, min(cursorScreenY, E.screenRows)) // Clamp Y to text area
	cursorScreenX = max(1, min(cursorScreenX, E.screenCols))

	// Move cursor to calculated position: \x1b[<row>;<col>H
	// Estimate buffer needed: ~ len("\x1b[") + 5 + 1 + 5 + 1 = ~13 bytes
	if cap(buf)-len(buf) < 15 { /* Handle full */ } else {
		buf = appendString(buf, "\x1b[")
		buf = itoaAppend(buf, cursorScreenY)
		buf = append(buf, ';')
		buf = itoaAppend(buf, cursorScreenX)
		buf = append(buf, 'H')
	}


	// Ensure capacity for show cursor code
	if cap(buf)-len(buf) < 6 { /* Handle full */ }
	buf = appendString(buf, "\x1b[?25h") // Show cursor

	// Write entire buffer to terminal output at once
	// Loop write in case of partial writes (EINTR)
	bytesToWrite := buf
	for len(bytesToWrite) > 0 {
		// Write to Stdout FD
		n, err := syscall.Write(int(os.Stdout.Fd()), bytesToWrite)
		if n > 0 {
			bytesToWrite = bytesToWrite[n:]
		}
		// Handle errors, EINTR means retry, others might be fatal.
		if err != nil && err != syscall.EINTR && err != syscall.EAGAIN {
			// Cannot easily call die() here without risking terminal state.
			// Perhaps set a flag for main loop to detect write error?
			E.shouldQuit = true // Signal quit on persistent write error
			setStatusMessage("Terminal write error")
			break // Exit write loop
		}
		if n == 0 && err == nil { // Should not happen with blocking write?
			// Avoid potential infinite loop
			setStatusMessage("Terminal write error (0 bytes)")
			E.shouldQuit = true
			break
		}
	}
}

// --- Initialization ---

// Initializes the editor state.
func initEditor() {
	// Set FD and check if it's a TTY
	E.termFd = int(os.Stdin.Fd())
	E.isTTY = isTerminal(E.termFd)

	E.origTermios = syscall.Termios{} // Initialize empty
	E.cursorX = 0
	E.cursorY = 0
	E.renderX = 0
	E.rowOffset = 0
	E.colOffset = 0
	E.mode = Mode_Normal
	E.buffer.numLines = 0
	E.buffer.dirty = false
	E.buffer.filenameLen = 0
	clear(E.buffer.filename[:]) // Clear filename buffer
	E.statusMsgLen = 0
	clear(E.statusMsg[:]) // Clear status message buffer
	E.shouldQuit = false
	E.quitConfirm = 0

	// Get window size, using defaults if it fails or not a TTY
	if getWindowSize() == result_err && E.isTTY {
		// Only show warning if it failed on an actual TTY
		setStatusMessage("Warning: Cannot get window size, using defaults")
	}

	// Reserve rows for status bar and message bar if we have a TTY
	if E.isTTY {
		if E.screenRows >= 2 {
			E.screenRows -= 2
		} else {
			// Handle extremely small terminals
			if E.screenRows < 0 { E.screenRows = 0 }
			setStatusMessage("Warning: Terminal too small!")
		}
		if E.screenCols <= 0 { // Also check columns
			 E.screenCols = 80 // Default if invalid
			 setStatusMessage("Warning: Invalid terminal width!")
		}
	} else {
		// Not a TTY, no screen interaction, keep default sizes but don't subtract status rows.
		E.screenRows = 24 // Default
		E.screenCols = 80 // Default
	}
}

// --- Main ---

// Cleanly exits the editor after restoring terminal settings and printing error.
func die(s string) {
	// Attempt to clear screen and move cursor home only if TTY
	if E.isTTY {
		_, _ = syscall.Write(int(os.Stdout.Fd()), []byte("\x1b[2J"))
		_, _ = syscall.Write(int(os.Stdout.Fd()), []byte("\x1b[H"))
	}
	// Ensure raw mode is disabled (safe even if never enabled or not TTY)
	disableRawMode()
	// Print error message to Stderr
	_, _ = syscall.Write(int(os.Stderr.Fd()), []byte(s))
	_, _ = syscall.Write(int(os.Stderr.Fd()), []byte("\n"))
	os.Exit(1)
}

func main() {
	// Initialize basic state including TTY check
	initEditor()

	// Check if running interactively
	if !E.isTTY {
		// Optional: Print message? Or just exit? Let's exit cleanly.
		// We could potentially process the file non-interactively here if needed.
		_, _ = syscall.Write(int(os.Stderr.Fd()), []byte("Error: lkjsxcgoeditor must be run in a terminal.\n"))
		os.Exit(1)
		return // Unreachable after Exit, but good practice
	}

	// Enable raw mode; die if it fails.
	if enableRawMode() == result_err {
		// Use die for consistent exit handling
		die("Error: Failed to enable raw terminal mode.")
	}
	// Ensure raw mode is disabled on normal exit or panic
	defer disableRawMode()

	// Load file if specified as command line argument
	if len(os.Args) >= 2 {
		// editorOpenFile handles errors and sets status message
		_ = editorOpenFile(os.Args[1]) // Ignore result, status message indicates outcome
	} else {
		// Initialize with an empty buffer and welcome message
		E.buffer.numLines = 0 // Ensure explicitly 0
		setStatusMessage("LKJSXCG Editor | Ctrl-S: Save | Ctrl-Q: Quit")
	}

	// Main event loop: Refresh screen, process keypress, repeat until quit signal.
	for !E.shouldQuit {
		editorRefreshScreen()
		if editorProcessKeypress() == result_err {
			// If key processing returns error (e.g., read error), signal quit.
			// Don't set status message here, let the loop terminate.
			E.shouldQuit = true
			break
		}
	}

	// Final cleanup: Clear screen and move cursor home before exiting.
	// Use a loop for Write like in editorRefreshScreen for robustness.
	finalClear := []byte("\x1b[2J\x1b[H")
	bytesToWrite := finalClear
	for len(bytesToWrite) > 0 {
		n, err := syscall.Write(int(os.Stdout.Fd()), bytesToWrite)
		if n > 0 {
			bytesToWrite = bytesToWrite[n:]
		}
		if err != nil && err != syscall.EINTR && err != syscall.EAGAIN {
			break // Give up on error during final clear
		}
		if n == 0 && err == nil {
			break
		}
	}
}