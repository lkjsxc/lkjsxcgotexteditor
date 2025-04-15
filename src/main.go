package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
	"unicode"

	"golang.org/x/term" // Use the standard extended terminal package
)

const kiloVersion = "0.0.1"
const tabStop = 8
const kiloQuitTimes = 3
const kiloQueryLen = 256

// Syntax highlight types
const (
	hlNormal byte = iota
	hlNonPrint
	hlComment    // Single line comment.
	hlMlComment  // Multi-line comment.
	hlKeyword1
	hlKeyword2
	hlString
	hlNumber
	hlMatch // Search match.
)

// Syntax highlight flags
const (
	hlHighlightStrings = 1 << 0
	hlHighlightNumbers = 1 << 1
)

// KeyAction represents special key presses.
type KeyAction int

const (
	keyNull KeyAction = iota // Special value for non-mapped keys or errors

	// Mapped directly from ASCII, but given names for clarity
	ctrlC     KeyAction = 3
	ctrlD     KeyAction = 4
	ctrlF     KeyAction = 6
	ctrlH     KeyAction = 8 // Historically Backspace
	tab       KeyAction = 9
	ctrlL     KeyAction = 12
	enter     KeyAction = 13
	ctrlQ     KeyAction = 17
	ctrlS     KeyAction = 19
	ctrlU     KeyAction = 21
	esc       KeyAction = 27
	backspace KeyAction = 127

	// Values >= 1000 are synthetic key codes
	arrowLeft KeyAction = iota + 1000
	arrowRight
	arrowUp
	arrowDown
	delKey
	homeKey
	endKey
	pageUp
	pageDown
)

// editorSyntax defines syntax highlighting rules for a file type.
type editorSyntax struct {
	Filematch              []string // File patterns (e.g., ".c", "Makefile")
	Keywords               []string // Keywords to highlight
	SinglelineCommentStart string   // e.g., "//"
	MultilineCommentStart  string   // e.g., "/*"
	MultilineCommentEnd    string   // e.g., "*/"
	Flags                  int      // Bitmask for HL_HIGHLIGHT_STRINGS, HL_HIGHLIGHT_NUMBERS
}

// erow represents a single line of the file being edited.
type erow struct {
	Idx    int    // Row index in the file, zero-based.
	Size   int    // Size of the row (Chars), excluding the null term equivalent.
	Rsize  int    // Size of the rendered row (Render).
	Chars  []byte // Row content.
	Render []byte // Row content "rendered" for screen (for TABs).
	Hl     []byte // Syntax highlight type for each character in Render.
	HlOc   bool   // Row had open comment at end in last syntax highlight check.
}

// editorConfig holds the global state of the editor.
type editorConfig struct {
	Cx            int           // Cursor x position within the rendered line (Render)
	Cy            int           // Cursor y position on the screen (0-based)
	Rx            int           // Cursor x position within the actual characters (Chars) - needed for tab mapping
	Rowoff        int           // Offset of row displayed at the top.
	Coloff        int           // Offset of column displayed at the left.
	ScreenRows    int           // Number of rows that we can show (excluding status bar)
	ScreenCols    int           // Number of cols that we can show
	NumRows       int           // Number of rows in the file
	TermState     *term.State   // Original terminal state to restore on exit
	Row           []erow        // Slice holding all rows (the file buffer)
	Dirty         int           // File modified but not saved count
	Filename      string        // Currently open filename
	StatusMsg     string        // Message to display in the status bar
	StatusMsgTime time.Time     // Timestamp for status message expiry
	Syntax        *editorSyntax // Current syntax highlight rules, or nil.
	QuitTimes     int           // How many times Ctrl-Q must be pressed if dirty
}

var E editorConfig // Global editor state

// =========================== Syntax highlights DB =========================

var hldb []editorSyntax

func initSyntaxDB() {
	// C / C++
	cKeywords := []string{
		// C Keywords
		"auto", "break", "case", "continue", "default", "do", "else", "enum",
		"extern", "for", "goto", "if", "register", "return", "sizeof", "static",
		"struct", "switch", "typedef", "union", "volatile", "while", "NULL",

		// C++ Keywords
		"alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor", "class",
		"compl", "constexpr", "const_cast", "decltype", "delete", "dynamic_cast", // Corrected typo deltype -> decltype
		"explicit", "export", "false", "friend", "inline", "mutable", "namespace",
		"new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
		"private", "protected", "public", "reinterpret_cast", "static_assert",
		"static_cast", "template", "this", "thread_local", "throw", "true", "try",
		"typeid", "typename", "virtual", "xor", "xor_eq",

		// C types (ending with | for HL_KEYWORD2)
		"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
		"void|", "short|", "auto|", "const|", "bool|",
	}
	cFilematch := []string{".c", ".h", ".cpp", ".hpp", ".cc"}

	hldb = append(hldb, editorSyntax{
		Filematch:              cFilematch,
		Keywords:               cKeywords,
		SinglelineCommentStart: "//",
		MultilineCommentStart:  "/*",
		MultilineCommentEnd:    "*/",
		Flags:                  hlHighlightStrings | hlHighlightNumbers,
	})

	// Go
	goKeywords := []string{
		// Go keywords
		"break", "case", "chan", "const", "continue", "default", "defer", "else", "fallthrough",
		"for", "func", "go", "goto", "if", "import", "interface", "map", "package", "range",
		"return", "select", "struct", "switch", "type", "var",

		// Go types (ending with | for HL_KEYWORD2)
		"bool|", "byte|", "complex64|", "complex128|", "error|", "float32|", "float64|",
		"int|", "int8|", "int16|", "int32|", "int64|", "rune|", "string|", "uint|",
		"uint8|", "uint16|", "uint32|", "uint64|", "uintptr|",
	}
	goFilematch := []string{".go"}

	hldb = append(hldb, editorSyntax{
		Filematch:              goFilematch,
		Keywords:               goKeywords,
		SinglelineCommentStart: "//",
		MultilineCommentStart:  "/*",
		MultilineCommentEnd:    "*/",
		Flags:                  hlHighlightStrings | hlHighlightNumbers,
	})
}

// ======================= Low level terminal handling ======================

// die prints an error message and exits.
func die(s string, err error) {
	// Clear screen and move cursor to top-left before exiting
	os.Stdout.WriteString("\x1b[2J")
	os.Stdout.WriteString("\x1b[H")
	if E.TermState != nil {
		term.Restore(int(os.Stdin.Fd()), E.TermState) // Attempt to restore terminal
	}
	log.Fatalf("%s: %v\n", s, err)
}

// disableRawMode restores the original terminal settings.
func disableRawMode() {
	if E.TermState != nil {
		if err := term.Restore(int(os.Stdin.Fd()), E.TermState); err != nil {
			// Log the error but don't die, as we are already exiting.
			log.Printf("Failed to restore terminal: %v", err)
		}
		E.TermState = nil
	}
}

// enableRawMode sets the terminal to raw mode.
func enableRawMode() {
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		die("Failed to enable raw mode", err)
	}
	E.TermState = oldState
	// Set up cleanup function
	// We use os.Exit which doesn't run deferred functions, so atexit is better simulated
	// using signal handling for common exit signals or explicitly calling disableRawMode before os.Exit.
	// The signal handler below takes care of SIGTERM etc.
	// Direct calls to os.Exit() like in C's exit(0) will bypass this.
	// Consider replacing explicit os.Exit(0) calls with a helper function
	// that calls disableRawMode() first.
}

// editorReadKey reads a single keypress from the terminal in raw mode.
// It handles escape sequences for special keys.
func editorReadKey() KeyAction {
	var buf [1]byte // Buffer to read one byte
	n, err := os.Stdin.Read(buf[:])
	if err != nil {
		// Check for EOF (e.g., if stdin is redirected and finishes)
		if errors.Is(err, io.EOF) {
			return keyNull // Or perhaps a dedicated EOF key?
		}
		// EAGAIN might happen with non-blocking reads, but term.MakeRaw uses blocking.
		// Other errors are likely fatal in this context.
		die("Read error", err)
	}
	if n == 0 {
		return keyNull // Should not happen with blocking read unless EOF?
	}

	c := KeyAction(buf[0])

	if c != esc {
		return c // Return regular key directly
	}

	// --- Handle Escape Sequences ---
	// Try reading the next two bytes with a short timeout
	// Go's standard library doesn't have a direct equivalent to VTIME.
	// We simulate it by attempting non-blocking reads or using channels with timeouts.
	// For simplicity here, we'll try reading sequentially, assuming latency isn't extreme.
	// A more robust solution might involve setting a read deadline or using select.

	seq := make([]byte, 3)
	nRead := 0

	// Attempt to read the next character (e.g., '[' or 'O')
	n, err = os.Stdin.Read(seq[nRead : nRead+1])
	if n == 0 || err != nil { // Timeout or error reading seq[0]
		return esc // Just an ESC key press
	}
	nRead++

	// Attempt to read the character after '[' or 'O'
	n, err = os.Stdin.Read(seq[nRead : nRead+1])
	if n == 0 || err != nil { // Timeout or error reading seq[1]
		return esc // Incomplete sequence
	}
	nRead++

	// --- Parse ESC [ sequences ---
	if seq[0] == '[' {
		if seq[1] >= '0' && seq[1] <= '9' {
			// Extended escape sequence like PageUp/Down, Del
			// Attempt to read the final '~'
			n, err = os.Stdin.Read(seq[nRead : nRead+1])
			if n == 0 || err != nil || seq[nRead] != '~' {
				return esc // Incomplete sequence
			}
			switch seq[1] {
			case '1': return homeKey // Typically ESC [ 1 ~ or ESC [ H
			case '3': return delKey  // ESC [ 3 ~
			case '4': return endKey  // Typically ESC [ 4 ~ or ESC [ F
			case '5': return pageUp  // ESC [ 5 ~
			case '6': return pageDown // ESC [ 6 ~
			case '7': return homeKey // Sometimes ESC [ 7 ~
			case '8': return endKey  // Sometimes ESC [ 8 ~
			default: return esc
			}
		} else {
			// Arrow keys, Home, End
			switch seq[1] {
			case 'A': return arrowUp
			case 'B': return arrowDown
			case 'C': return arrowRight
			case 'D': return arrowLeft
			case 'H': return homeKey // Typically ESC [ H
			case 'F': return endKey  // Typically ESC [ F
			default: return esc
			}
		}
	}

	// --- Parse ESC O sequences (legacy, e.g., from VT100 keypad) ---
	if seq[0] == 'O' {
		switch seq[1] {
		case 'H': return homeKey
		case 'F': return endKey
		case 'P': // F1? Ignore for now
		case 'Q': // F2? Ignore for now
		case 'R': // F3? Ignore for now
		case 'S': // F4? Ignore for now
		default: return esc
		}
	}

	return esc // If sequence wasn't recognized
}

// getWindowSize attempts to get the terminal dimensions.
func getWindowSize() error {
	width, height, err := term.GetSize(int(os.Stdout.Fd()))
	if err != nil {
		// Fallback: Try querying cursor position (less reliable)
		// Save cursor, move far right/down, query, restore.
		os.Stdout.WriteString("\x1b[999C\x1b[999B")
		if err := getCursorPosition(); err != nil {
			// If even fallback fails, use default and report error
			E.ScreenCols = 80
			E.ScreenRows = 24
			return fmt.Errorf("failed to get window size: %w; fallback query failed: %v", err, err)
		}
		// Cursor position reports 1-based row/col, adjust later
	} else {
		E.ScreenCols = width
		E.ScreenRows = height
	}

	if E.ScreenRows <= 2 { // Need space for status bar + message bar
		return errors.New("terminal height too small")
	}
	E.ScreenRows -= 2 // Adjust for status and message bars
	return nil
}

// getCursorPosition queries the terminal for the current cursor position.
// Updates E.ScreenRows and E.ScreenCols as a fallback mechanism.
// This is often less reliable than ioctl/GetSize.
func getCursorPosition() error {
	// Send Device Status Report (DSR) CPR - Cursor Position Report
	_, err := os.Stdout.WriteString("\x1b[6n")
	if err != nil {
		return fmt.Errorf("failed to write DSR: %w", err)
	}

	// Read the response: typically ESC[<row>;<col>R
	reader := bufio.NewReader(os.Stdin)
	response := ""
	for {
		r, _, err := reader.ReadRune()
		if err != nil {
			return fmt.Errorf("failed to read DSR response: %w", err)
		}
		response += string(r)
		if r == 'R' {
			break // End of response marker
		}
		if len(response) > 30 { // Prevent infinite loop on unexpected input
			return errors.New("DSR response too long or invalid")
		}
	}

	// Parse the response
	if !strings.HasPrefix(response, "\x1b[") || !strings.HasSuffix(response, "R") {
		return fmt.Errorf("invalid DSR response format: %q", response)
	}

	parts := strings.Split(strings.TrimSuffix(strings.TrimPrefix(response, "\x1b["), "R"), ";")
	if len(parts) != 2 {
		return fmt.Errorf("invalid DSR response parts: %q", response)
	}

	row, err1 := strconv.Atoi(parts[0])
	col, err2 := strconv.Atoi(parts[1])
	if err1 != nil || err2 != nil {
		return fmt.Errorf("failed to parse DSR row/col: %q (%v, %v)", response, err1, err2)
	}

	// DSR is 1-based, Kilo/Go expects 0-based internally usually, but getWindowSize uses raw counts.
	// This function is primarily used as a *fallback* for getWindowSize, so update ScreenCols/Rows directly.
	E.ScreenRows = row
	E.ScreenCols = col
	return nil
}

// ====================== Syntax highlight color scheme  ====================

// isSeparator checks if a character is considered a separator for syntax highlighting.
func isSeparator(c byte) bool {
	return c == 0 || unicode.IsSpace(rune(c)) || bytes.IndexByte([]byte(",.()+-/*=~%[];"), c) != -1
}

// editorRowHasOpenComment checks if the row ends with an unterminated multi-line comment.
func editorRowHasOpenComment(row *erow) bool {
	// Check if highlighting is enabled and the last highlight is ML_COMMENT
	if row.Hl == nil || row.Rsize == 0 || row.Hl[row.Rsize-1] != hlMlComment {
		return false
	}
	// Check if the comment doesn't actually end at the very end of the line
	// Requires looking at the *rendered* characters that correspond to the highlight
	if E.Syntax == nil || E.Syntax.MultilineCommentEnd == "" {
		return true // Assume open if no end defined
	}
	endLen := len(E.Syntax.MultilineCommentEnd)
	if row.Rsize < endLen {
		return true // Too short to contain the end sequence
	}
	// Compare the end of the rendered string with the multi-line comment end sequence
	return !bytes.HasSuffix(row.Render, []byte(E.Syntax.MultilineCommentEnd))
}

// editorUpdateSyntax updates the syntax highlighting for a single row.
func editorUpdateSyntax(row *erow) {
	// Ensure HL slice is allocated and has the correct size
	if cap(row.Hl) < row.Rsize {
		row.Hl = make([]byte, row.Rsize)
	} else {
		row.Hl = row.Hl[:row.Rsize] // Adjust slice length if needed
	}
	// Initialize highlighting to normal
	for i := range row.Hl {
		row.Hl[i] = hlNormal
	}

	if E.Syntax == nil {
		return // No syntax definition, nothing more to do
	}

	keywords := E.Syntax.Keywords
	scs := []byte(E.Syntax.SinglelineCommentStart) // Single-line comment start
	mcs := []byte(E.Syntax.MultilineCommentStart)  // Multi-line comment start
	mce := []byte(E.Syntax.MultilineCommentEnd)   // Multi-line comment end
	scsLen := len(scs)
	mcsLen := len(mcs)
	mceLen := len(mce)
	flags := E.Syntax.Flags

	// State variables for parsing
	prevSep := true // Is the previous character a separator?
	var inString byte = 0 // Are we inside a string? (stores '"' or '\'')
	// Determine if we start inside a multi-line comment based on the previous line
	inComment := row.Idx > 0 && editorRowHasOpenComment(&E.Row[row.Idx-1])

	i := 0 // Index into row.Render and row.Hl
	for i < row.Rsize {
		c := row.Render[i]

		// Handle multi-line comments first
		if inComment {
			row.Hl[i] = hlMlComment
			// Check for end of multi-line comment
			if mceLen > 0 && bytes.HasPrefix(row.Render[i:], mce) {
				for j := 0; j < mceLen && i+j < row.Rsize; j++ {
					row.Hl[i+j] = hlMlComment
				}
				i += mceLen
				inComment = false
				prevSep = true
				continue
			} else {
				// Still inside the comment
				prevSep = false
				i++
				continue
			}
		}

		// Handle single-line comments
		if scsLen > 0 && prevSep && bytes.HasPrefix(row.Render[i:], scs) {
			// Comment extends to the end of the line
			for j := i; j < row.Rsize; j++ {
				row.Hl[j] = hlComment
			}
			i = row.Rsize // Move index to end
			break
		}

		// Handle start of multi-line comments
		if mcsLen > 0 && bytes.HasPrefix(row.Render[i:], mcs) {
			for j := 0; j < mcsLen && i+j < row.Rsize; j++ {
				row.Hl[i+j] = hlMlComment
			}
			i += mcsLen
			inComment = true
			prevSep = false
			continue
		}

		// Handle strings
		if flags&hlHighlightStrings != 0 {
			if inString != 0 {
				row.Hl[i] = hlString
				// Handle escaped quotes within strings
				if c == '\\' && i+1 < row.Rsize {
					row.Hl[i+1] = hlString
					i += 2 // Skip the escaped character
					prevSep = false
					continue
				}
				// Check for end of string
				if c == inString {
					inString = 0 // End of string
					prevSep = true // Character after quote is separator
				} else {
					prevSep = false
				}
				i++
				continue
			} else {
				// Check for start of string
				if c == '"' || c == '\'' {
					inString = c
					row.Hl[i] = hlString
					i++
					prevSep = false
					continue
				}
			}
		}

		// Handle numbers
		if flags&hlHighlightNumbers != 0 {
			if (unicode.IsDigit(rune(c)) && (prevSep || (i > 0 && row.Hl[i-1] == hlNumber))) ||
				(c == '.' && i > 0 && row.Hl[i-1] == hlNumber) {
				row.Hl[i] = hlNumber
				i++
				prevSep = false
				continue
			}
		}

		// Handle keywords
		if prevSep {
			foundKeyword := false
			for _, kw := range keywords {
				kwLen := len(kw)
				kwType := hlKeyword1
				if strings.HasSuffix(kw, "|") {
					kwLen--
					kwType = hlKeyword2
					kw = kw[:kwLen] // Remove the marker
				}

				// Check if keyword matches and is followed by a separator
				if i+kwLen <= row.Rsize &&
					bytes.Equal(row.Render[i:i+kwLen], []byte(kw)) &&
					(i+kwLen == row.Rsize || isSeparator(row.Render[i+kwLen])) {
					// Highlight the keyword
					for j := 0; j < kwLen; j++ {
						row.Hl[i+j] = byte(kwType)
					}
					i += kwLen
					prevSep = false // Keyword itself is not a separator
					foundKeyword = true
					break // Found a keyword match
				}
			}
			if foundKeyword {
				continue // Skip the normal character processing
			}
		}

		// Handle non-printable characters (already done during rendering usually)
		// If needed, check here: if !unicode.IsPrint(rune(c)) { row.Hl[i] = hlNonPrint; ... }
		// Kilo C renders them as '?', so normal highlighting applies. We do the same.

		// Default case: not a special character
		prevSep = isSeparator(c)
		// Ensure HL is set to normal if not otherwise highlighted (redundant due to init, but safe)
		if row.Hl[i] == 0 { // If somehow not set
			row.Hl[i] = hlNormal
		}
		i++
	}

	// Check if the open comment status changed for this row
	oc := editorRowHasOpenComment(row)
	// If it changed and it affects the next row, request a syntax update for the next row.
	// This might cascade down the file. Avoid infinite loops by checking index bounds.
	if row.HlOc != oc && row.Idx+1 < E.NumRows {
		// Mark the next row for update (or update directly if feasible)
		// Direct update can be slow for large files. A marking system might be better.
		// For simplicity, we'll update directly here.
		editorUpdateSyntax(&E.Row[row.Idx+1])
	}
	row.HlOc = oc
}

// editorSyntaxToColor maps syntax highlight types to ANSI color codes.
func editorSyntaxToColor(hl byte) int {
	switch hl {
	case hlComment, hlMlComment:
		return 36 // cyan
	case hlKeyword1:
		return 33 // yellow
	case hlKeyword2:
		return 32 // green
	case hlString:
		return 35 // magenta
	case hlNumber:
		return 31 // red
	case hlMatch:
		return 34 // blue
	case hlNonPrint: // Kilo C doesn't color non-print, but uses reverse video. We can use a color.
		return 31 // red (or choose another)
	default: // hlNormal
		return 37 // white
	}
}

// editorSelectSyntaxHighlight determines the syntax rules based on the filename.
func editorSelectSyntaxHighlight(filename string) {
	E.Syntax = nil // Reset syntax
	if filename == "" {
		return
	}

	fnameLower := strings.ToLower(filepath.Base(filename)) // Use base name for matching

	for i := range hldb {
		s := &hldb[i] // Pointer to the syntax rule
		for _, pattern := range s.Filematch {
			isSuffix := strings.HasPrefix(pattern, ".")
			match := false
			if isSuffix {
				// Match file extension
				if strings.HasSuffix(fnameLower, pattern) {
					match = true
				}
			} else {
				// Match if pattern is contained anywhere in the filename
				if strings.Contains(fnameLower, strings.ToLower(pattern)) {
					match = true
				}
			}

			if match {
				E.Syntax = s
				// Update syntax for all existing rows
				for j := 0; j < E.NumRows; j++ {
					editorUpdateRow(&E.Row[j]) // Recalculate render and syntax
				}
				return // Found a match, stop searching
			}
		}
	}
}

// ======================= Editor rows implementation =======================

// editorUpdateRow calculates the 'Render' string from 'Chars' (handling tabs)
// and updates syntax highlighting for the row.
func editorUpdateRow(row *erow) {
	var renderBuf bytes.Buffer
	col := 0
	for _, char := range row.Chars {
		if char == byte(tab) {
			renderBuf.WriteByte(' ')
			col++
			for col%tabStop != 0 {
				renderBuf.WriteByte(' ')
				col++
			}
		} else if !unicode.IsPrint(rune(char)) {
			// Represent non-printable characters (like Kilo C)
			// Note: Go strings handle UTF-8, but Kilo C treats bytes.
			// We'll represent invalid UTF-8 or control chars simply.
			// This rendering might differ slightly from Kilo C for multi-byte non-printables.
			renderBuf.WriteByte('?') // Simple representation
			col++
		} else {
			renderBuf.WriteByte(char)
			col++
		}
	}
	row.Render = renderBuf.Bytes()
	row.Rsize = col // The *rendered* width
	row.Size = len(row.Chars) // Actual byte length

	// Update syntax highlighting based on the new render
	editorUpdateSyntax(row)
}

// editorInsertRow inserts a new row at the specified index 'at'.
func editorInsertRow(at int, content []byte) {
	if at < 0 || at > E.NumRows {
		return
	}

	// Create the new row
	newRow := erow{
		Idx:   at,
		Chars: make([]byte, len(content)), // Make a copy
		// Render, Rsize, Size, Hl will be set by editorUpdateRow
	}
	copy(newRow.Chars, content)

	// Expand the Rows slice
	// Using append for insertion requires splitting the slice
	E.Row = append(E.Row[:at], append([]erow{newRow}, E.Row[at:]...)...)

	// Update indices for rows below the inserted one
	for i := at + 1; i < E.NumRows+1; i++ {
		E.Row[i].Idx++
	}

	E.NumRows++
	E.Dirty++

	// Calculate render and syntax for the new row
	editorUpdateRow(&E.Row[at])
	// Also update syntax for the row *after* the new one, as its context might change
	if at+1 < E.NumRows {
		editorUpdateSyntax(&E.Row[at+1])
	}
}

// editorDelRow deletes the row at the specified index 'at'.
func editorDelRow(at int) {
	if at < 0 || at >= E.NumRows {
		return
	}

	// Remove the row using slice manipulation
	E.Row = append(E.Row[:at], E.Row[at+1:]...)

	// Update indices for rows below the deleted one
	for i := at; i < E.NumRows-1; i++ {
		E.Row[i].Idx--
	}

	E.NumRows--
	E.Dirty++

	// Update syntax for the row *at* the deleted position (now the next row)
	// and the one before it, as their context might change
	if at > 0 {
		editorUpdateSyntax(&E.Row[at-1])
	}
	if at < E.NumRows { // Check bounds after deletion
		editorUpdateSyntax(&E.Row[at])
	}
}

// editorRowsToString converts all editor rows into a single string with newlines.
func editorRowsToString() string {
	var builder strings.Builder
	for i, row := range E.Row {
		builder.Write(row.Chars)
		if i < E.NumRows-1 { // Add newline except for the last line
			builder.WriteByte('\n')
		}
	}
	return builder.String()
}

// editorRowInsertChar inserts a character 'c' into row 'row' at index 'at'.
// 'at' is the index within the *original* Chars slice.
func editorRowInsertChar(row *erow, at int, c byte) {
	if at < 0 || at > row.Size { // Allow insertion at the very end
		at = row.Size
	}

	// Expand Chars slice
	row.Chars = append(row.Chars[:at], append([]byte{c}, row.Chars[at:]...)...)
	row.Size = len(row.Chars) // Update Size explicitly

	// Recalculate render and syntax
	editorUpdateRow(row)
	E.Dirty++
}

// editorRowAppendString appends string 's' to the end of 'row'.
func editorRowAppendString(row *erow, s []byte) {
	row.Chars = append(row.Chars, s...)
	row.Size = len(row.Chars) // Update Size

	// Recalculate render and syntax
	editorUpdateRow(row)
	E.Dirty++
}

// editorRowDelChar deletes the character at index 'at' in the row's Chars.
func editorRowDelChar(row *erow, at int) {
	if at < 0 || at >= row.Size {
		return
	}

	// Delete character using slice manipulation
	row.Chars = append(row.Chars[:at], row.Chars[at+1:]...)
	row.Size = len(row.Chars) // Update Size

	// Recalculate render and syntax
	editorUpdateRow(row)
	E.Dirty++
}

// editorInsertChar inserts a character at the current cursor position.
func editorInsertChar(c byte) {
	fileRow := E.Rowoff + E.Cy
	//fileCol := E.Coloff + E.Cx // This is render column, need character column (Rx)

	// If cursor is beyond the end of the file, create empty rows
	if fileRow > E.NumRows {
		for E.NumRows < fileRow {
			editorInsertRow(E.NumRows, []byte{}) // Insert empty row
		}
	}
	// If cursor is on the line *after* the last line
	if fileRow == E.NumRows {
		editorInsertRow(E.NumRows, []byte{})
	}

	row := &E.Row[fileRow]
	editorRowInsertChar(row, E.Rx, c) // Use Rx for character index

	// Move cursor forward
	// We simply move right by one character column (Rx) and let editorMoveCursor
	// or the refresh logic handle the render column (Cx) adjustment.
	E.Rx++
	// Adjust Cx based on the new Rx and potential tab expansion
	editorUpdateCxFromRx(row)
}

// editorInsertNewline handles inserting a newline character.
func editorInsertNewline() {
	fileRow := E.Rowoff + E.Cy
	//fileCol := E.Coloff + E.Cx // Render column

	if fileRow > E.NumRows { // Should not happen if editorInsertChar logic is correct
		return
	}
	// If cursor is on the virtual line after the last line
	if fileRow == E.NumRows {
		editorInsertRow(E.NumRows, []byte{}) // Insert empty row first
		// Now fileRow points to the newly inserted row index
	}

	row := &E.Row[fileRow]

	// If cursor is at the beginning of the line (Rx=0)
	if E.Rx == 0 {
		editorInsertRow(fileRow, []byte{}) // Insert new empty row before current
	} else {
		// Split the current line at the cursor position (Rx)
		contentToMove := make([]byte, len(row.Chars[E.Rx:]))
		copy(contentToMove, row.Chars[E.Rx:])
		// Truncate the current row
		row.Chars = row.Chars[:E.Rx]
		row.Size = E.Rx
		editorUpdateRow(row) // Update render/syntax for the truncated row

		// Insert the rest of the line as a new row below
		editorInsertRow(fileRow+1, contentToMove)
	}

	// Move cursor to the beginning of the new line
	E.Cy++
	E.Rx = 0
	E.Cx = 0 // Render column is also 0
	E.Coloff = 0 // Reset column offset

	// Scroll screen if necessary
	if E.Cy >= E.ScreenRows {
		E.Rowoff++
		E.Cy = E.ScreenRows - 1 // Stay at bottom visible row
	}
}

// editorDelChar handles deleting the character *before* the cursor (Backspace).
func editorDelChar() {
	fileRow := E.Rowoff + E.Cy
	//fileCol := E.Coloff + E.Cx // Render column

	// Cannot delete if at the very beginning of the file
	if E.Cy == 0 && E.Rx == 0 {
		return
	}
	// If cursor is beyond the actual data (should not happen with correct Rx/Cx sync)
	if fileRow >= E.NumRows {
		return
	}

	row := &E.Row[fileRow]

	if E.Rx > 0 {
		// Delete character within the current line
		editorRowDelChar(row, E.Rx-1) // Delete char before Rx
		E.Rx--                      // Move character cursor back
		editorUpdateCxFromRx(row)   // Update render cursor position
	} else {
		// Cursor is at the beginning of the line (Rx=0), merge with the previous line
		if fileRow == 0 { return } // Already at the first line

		prevRow := &E.Row[fileRow-1]
		originalPrevRowLen := prevRow.Size // Store length before append

		// Append current row's content to the previous row
		editorRowAppendString(prevRow, row.Chars)

		// Delete the current row
		editorDelRow(fileRow)

		// Move cursor to the end of the merged part on the previous line
		E.Cy--
		E.Rx = originalPrevRowLen
		editorUpdateCxFromRx(&E.Row[fileRow-1]) // Update Cx for the merged row

		// Adjust scroll if needed (though Cy decreased, Coloff might need adjustment)
		editorScroll() // Recalculate scroll based on new cursor position
	}
}

// editorDelKey handles deleting the character *at* the cursor (Delete key).
func editorDelKeyForward() {
    fileRow := E.Rowoff + E.Cy

    // Cannot delete if at the very end of the file content
    if fileRow >= E.NumRows {
        return
    }
    // If cursor is on the last row and at the end of it
    if fileRow == E.NumRows-1 && E.Rx == E.Row[fileRow].Size {
        return
    }

    row := &E.Row[fileRow]

    if E.Rx < row.Size {
        // Delete character within the current line at Rx
        editorRowDelChar(row, E.Rx)
        // Cursor position (Rx and Cx) doesn't change, but the content shifts left
        // We still need to update Cx in case a tab was involved indirectly
        editorUpdateCxFromRx(row)
    } else {
        // Cursor is at the end of the current line (Rx == row.Size)
        // Merge the next line into the current one
        if fileRow + 1 >= E.NumRows { return } // No next line exists

        nextRow := &E.Row[fileRow+1]

        // Append next row's content to the current row
        editorRowAppendString(row, nextRow.Chars)

        // Delete the next row
        editorDelRow(fileRow + 1)

        // Cursor position (Rx and Cx) doesn't change relative to the start of the original line
        // Need to ensure Cx is updated if tabs were involved
        editorUpdateCxFromRx(row)
    }
}


// editorOpen loads a file into the editor.
func editorOpen(filename string) error {
	// Reset existing state if any
	E.Filename = filename
	E.Row = nil // Clear existing rows
	E.NumRows = 0
	E.Dirty = 0
	E.Cx = 0
	E.Cy = 0
	E.Rx = 0
	E.Rowoff = 0
	E.Coloff = 0
	E.QuitTimes = kiloQuitTimes

	file, err := os.Open(filename)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			// File doesn't exist, treat as a new file
			E.Dirty = 0 // Not dirty yet
			editorSelectSyntaxHighlight(filename) // Select syntax based on name
			return nil
		}
		return fmt.Errorf("opening file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNum := 0
	for scanner.Scan() {
		lineBytes := scanner.Bytes() // Get bytes directly
		// Need to make a copy because scanner reuses the buffer
		lineCopy := make([]byte, len(lineBytes))
		copy(lineCopy, lineBytes)
		editorInsertRow(lineNum, lineCopy)
		lineNum++
	}
	if err := scanner.Err(); err != nil {
		// Don't die, just report the error
		editorSetStatusMessage("Error reading file: %v", err)
	}

	E.Dirty = 0 // File loaded successfully, not dirty
	editorSelectSyntaxHighlight(filename) // Select syntax after loading

	return nil
}

// editorSave writes the current buffer content to the file.
func editorSave() error {
	if E.Filename == "" {
		// Prompt for filename if not set (Advanced feature, not in original Kilo)
		// For now, just report error.
		editorSetStatusMessage("No filename specified. Use Save As (not implemented).")
		// Or potentially implement a prompt here using editorPrompt function
		newFilename := editorPrompt("Save as: %s (ESC to cancel)", nil)
		if newFilename == "" {
			editorSetStatusMessage("Save aborted.")
			return errors.New("save aborted by user")
		}
		E.Filename = newFilename
		editorSelectSyntaxHighlight(E.Filename) // Update syntax if name changed
	}


	content := editorRowsToString()
	contentBytes := []byte(content)
	fileLen := len(contentBytes)

	// Write atomically: write to temp file, then rename.
	// Or simpler: O_TRUNC | O_CREATE
	err := os.WriteFile(E.Filename, contentBytes, 0644) // 0644 permissions
	if err != nil {
		editorSetStatusMessage("Can't save! I/O error: %v", err)
		return fmt.Errorf("writing file: %w", err)
	}

	E.Dirty = 0
	editorSetStatusMessage("%d bytes written to %s", fileLen, E.Filename)
	return nil
}

// ============================= Terminal update ============================

// abuf provides an append buffer similar to the C version's struct abuf.
// Go's strings.Builder or bytes.Buffer are generally preferred.
// We'll use strings.Builder for efficiency.

// editorRefreshScreen redraws the entire editor UI.
func editorRefreshScreen() {
	editorScroll() // Ensure cursor is within viewport first

	var sb strings.Builder

	sb.WriteString("\x1b[?25l") // Hide cursor
	sb.WriteString("\x1b[H")    // Go home (top-left corner)

	for y := 0; y < E.ScreenRows; y++ {
		fileRow := E.Rowoff + y

		if fileRow >= E.NumRows {
			// Draw tildes for lines beyond the file content
			if E.NumRows == 0 && y == E.ScreenRows/3 {
				// Display welcome message on empty file
				welcome := fmt.Sprintf("LKJ's XC Go Editor -- version %s", kiloVersion)
				if len(welcome) > E.ScreenCols {
					welcome = welcome[:E.ScreenCols]
				}
				padding := (E.ScreenCols - len(welcome)) / 2
				if padding > 0 {
					sb.WriteString("~")
					sb.WriteString(strings.Repeat(" ", padding-1))
				}
				sb.WriteString(welcome)
			} else {
				sb.WriteString("~")
			}
		} else {
			// Draw actual file content
			row := &E.Row[fileRow]
			renderLen := row.Rsize
			line := row.Render
			hl := row.Hl

			// Adjust length and start position based on column offset
			start := E.Coloff
			length := 0
			if start < renderLen {
				length = renderLen - start
				if length > E.ScreenCols {
					length = E.ScreenCols // Truncate to screen width
				}
				line = line[start : start+length]
				if hl != nil && len(hl) > start { // Ensure hl slice is valid and long enough
					// Adjust hl slice, handle case where start+length exceeds hl bounds (shouldn't happen if updated correctly)
					hlEnd := start + length
					if hlEnd > len(hl) { hlEnd = len(hl)}
					hl = hl[start:hlEnd]
				} else {
					hl = nil // No highlighting available for this part
				}

			} else {
				line = nil // Nothing to display if coloff is beyond line end
				hl = nil
			}

			currentColor := -1 // Track current color to minimize escape codes
			if line != nil {
				for i, char := range line {
					hlType := hlNormal
					if hl != nil && i < len(hl) {
						hlType = byte(hl[i])
					}

					if hlType == hlNonPrint { // Kilo C uses reverse video
						sb.WriteString("\x1b[7m") // Reverse video on
						sb.WriteByte('?')         // Already rendered as '?' by editorUpdateRow
						sb.WriteString("\x1b[m")  // Reverse video off (reset all)
						currentColor = -1         // Reset color tracking
					} else {
						newColor := editorSyntaxToColor(byte(hlType))
						if newColor != currentColor {
							sb.WriteString(fmt.Sprintf("\x1b[%dm", newColor))
							currentColor = newColor
						}
						sb.WriteByte(char)
					}
				}
			}
			// Reset color at the end of the line if a color was active
			if currentColor != -1 {
				sb.WriteString("\x1b[39m") // Default foreground color
			}
		}

		sb.WriteString("\x1b[K") // Clear rest of the line
		if y < E.ScreenRows-1 {
			sb.WriteString("\r\n") // Move to next line
		}
	}

	// Draw Status Bar
	sb.WriteString("\x1b[K")   // Clear status line
	sb.WriteString("\x1b[7m") // Inverse video on
	dirtyMarker := ""
	if E.Dirty > 0 {
		dirtyMarker = "(modified)"
	}
	filename := E.Filename
	if filename == "" {
		filename = "[No Name]"
	}
	// Truncate filename if necessary
	maxFilenameLen := E.ScreenCols - 20 // Reserve space for line count and status
	if maxFilenameLen < 1 { maxFilenameLen = 1 }
	displayFilename := filename
	if len(displayFilename) > maxFilenameLen {
		displayFilename = "..." + displayFilename[len(displayFilename)-maxFilenameLen+3:] // Show end part
	}

	statusLeft := fmt.Sprintf("%.*s - %d lines %s", maxFilenameLen, displayFilename, E.NumRows, dirtyMarker)
	statusRight := ""
	if E.Syntax != nil {
		// Maybe show syntax name? Not in original Kilo. Keep it simple.
		// statusRight = E.Syntax.Filetype + " | "
	}
	statusRight += fmt.Sprintf("%d/%d", E.Cy+1, E.NumRows) // Show 1-based line number

	lineLen := len(statusLeft)
	rightLen := len(statusRight)
	sb.WriteString(statusLeft)
	for lineLen < E.ScreenCols {
		if E.ScreenCols-lineLen == rightLen {
			sb.WriteString(statusRight)
			break
		}
		sb.WriteString(" ")
		lineLen++
	}
	sb.WriteString("\x1b[m") // Inverse video off (reset all)
	sb.WriteString("\r\n")   // Move to message bar line

	// Draw Message Bar
	sb.WriteString("\x1b[K") // Clear message line
	msgLen := len(E.StatusMsg)
	if msgLen > 0 && time.Since(E.StatusMsgTime) < 5*time.Second {
		if msgLen > E.ScreenCols {
			msgLen = E.ScreenCols
		}
		sb.WriteString(E.StatusMsg[:msgLen])
	}

	// Position Cursor
	// Cx, Cy are 0-based screen coordinates. VT100 is 1-based.
	// Cx already reflects the *rendered* position including tabs.
	cursorY := E.Cy + 1
	cursorX := E.Cx - E.Coloff + 1 // Adjust Cx by Coloff for final screen position
	sb.WriteString(fmt.Sprintf("\x1b[%d;%dH", cursorY, cursorX))

	sb.WriteString("\x1b[?25h") // Show cursor

	// Write the entire buffer to stdout at once
	_, err := os.Stdout.WriteString(sb.String())
	if err != nil {
		die("Failed to write to terminal", err)
	}
}

// editorSetStatusMessage sets the message shown in the status bar.
func editorSetStatusMessage(format string, args ...interface{}) {
	E.StatusMsg = fmt.Sprintf(format, args...)
	E.StatusMsgTime = time.Now()
}

// editorScroll adjusts Rowoff and Coloff to keep the cursor visible.
func editorScroll() {
	// Update Rx (character index) based on Cx (render index) if needed.
	// This relationship is complex with tabs. We primarily drive changes
	// via Rx and update Cx accordingly, so scrolling should use Rx.
	// However, the *visual* scrolling depends on Cx and Coloff.

	// Vertical scroll
	if E.Cy < E.Rowoff {
		E.Rowoff = E.Cy
	}
	if E.Cy >= E.Rowoff+E.ScreenRows {
		E.Rowoff = E.Cy - E.ScreenRows + 1
	}

	// Horizontal scroll (based on Cx, the render position)
	if E.Cx < E.Coloff {
		E.Coloff = E.Cx
	}
	if E.Cx >= E.Coloff+E.ScreenCols {
		E.Coloff = E.Cx - E.ScreenCols + 1
	}
}

// editorUpdateCxFromRx recalculates E.Cx (render column) based on E.Rx (character column).
func editorUpdateCxFromRx(row *erow) {
	cx := 0
	for i := 0; i < E.Rx; i++ {
		if i >= len(row.Chars) { break } // Safety check
		if row.Chars[i] == byte(tab) {
			cx += (tabStop - 1) - (cx % tabStop)
		}
		cx++
	}
	E.Cx = cx
}

// editorUpdateRxFromCx recalculates E.Rx (character column) based on E.Cx (render column).
// This is used when moving the cursor vertically to maintain horizontal position.
func editorUpdateRxFromCx(row *erow) {
	currentCx := 0
	E.Rx = 0 // Reset Rx
	for rx, char := range row.Chars {
		if char == byte(tab) {
			currentCx += (tabStop - 1) - (currentCx % tabStop)
		}
		currentCx++

		if currentCx > E.Cx { // Found the character position corresponding to Cx
			E.Rx = rx
			return
		}
		E.Rx = rx + 1 // Point Rx to the *next* character index
	}
	// If Cx was beyond the rendered line length, Rx should be at the end.
	// The loop already sets Rx = len(row.Chars) in this case.
}


// =============================== Find mode ================================

// editorFind implements the search functionality.
func editorFind() {
	// Save cursor position to restore on cancel
	savedCx := E.Cx
	savedCy := E.Cy
	savedRx := E.Rx
	savedColoff := E.Coloff
	savedRowoff := E.Rowoff

	lastMatch := -1 // Index of the last matched row (-1 = none)
	direction := 1  // 1 = forward, -1 = backward
	var savedHl []byte
	savedHlLine := -1 // Row index whose HL is saved

	query := ""

	// Restore Highlight function
	restoreHl := func() {
		if savedHlLine != -1 && savedHlLine < E.NumRows {
			// Check if row still exists and Hl needs restoring
			row := &E.Row[savedHlLine]
			if len(savedHl) == len(row.Hl) { // Basic check
				copy(row.Hl, savedHl)
				savedHl = nil
				savedHlLine = -1
			} else {
				// Mismatch, likely row changed significantly. Just recalculate.
				editorUpdateSyntax(row)
				savedHl = nil
				savedHlLine = -1
			}
		}
	}
	defer restoreHl() // Ensure highlight is restored on exit

	for {
		editorSetStatusMessage("Search: %s (ESC=Cancel, Enter=Confirm, Arrows=Next/Prev)", query)
		editorRefreshScreen()

		key := editorReadKey()

		switch key {
		case backspace, ctrlH, delKey: // Handle Backspace/Delete
			if len(query) > 0 {
				query = query[:len(query)-1]
				lastMatch = -1 // Reset search state
				restoreHl()    // Remove previous highlight
			}
		case esc, ctrlC: // Cancel search
			// Restore original cursor position
			E.Cx = savedCx
			E.Cy = savedCy
			E.Rx = savedRx
			E.Coloff = savedColoff
			E.Rowoff = savedRowoff
			editorSetStatusMessage("")
			return // Exit find mode
		case enter: // Confirm search (stay at current match)
			// Keep the current position, just clear the status message
			editorSetStatusMessage("")
			// Don't restore highlight here, keep it until next action
			return // Exit find mode
		case arrowRight, arrowDown:
			direction = 1 // Search forward
		case arrowLeft, arrowUp:
			direction = -1 // Search backward
		default:
			// Append printable characters to query
			if key >= 32 && key <= 126 { // Basic printable ASCII
				if len(query) < kiloQueryLen {
					query += string(byte(key))
					lastMatch = -1 // Reset search state on query change
					restoreHl()    // Remove previous highlight
				}
			} else {
				// Ignore other special keys in find mode for now
				direction = 0 // No search trigger on unknown keys
			}
		}

		// Perform search if direction is set
		if direction != 0 && query != "" {
			restoreHl() // Restore previous highlight before searching again

			current := lastMatch
			if current == -1 { // If starting a new search
				current = E.Rowoff + E.Cy // Start from current cursor line
				if direction == -1 {
					current-- // Adjust starting point for backward search
				}
			}

			for i := 0; i < E.NumRows; i++ {
				current += direction
				// Wrap around search
				if current < 0 {
					current = E.NumRows - 1
				} else if current >= E.NumRows {
					current = 0
				}

				row := &E.Row[current]
				// Search in the *rendered* text for matches
				matchIndex := bytes.Index(row.Render, []byte(query))

				if matchIndex != -1 {
					// Match found!
					lastMatch = current

					// Save current row's highlight state before overwriting
					savedHlLine = current
					savedHl = make([]byte, len(row.Hl))
					copy(savedHl, row.Hl)

					// Highlight the match in the row's Hl slice
					matchEnd := matchIndex + len(query)
					if matchEnd > len(row.Hl) { matchEnd = len(row.Hl) } // Bounds check
					for j := matchIndex; j < matchEnd; j++ {
						row.Hl[j] = hlMatch
					}

					// Move cursor to the match
					E.Cy = current - E.Rowoff // Calculate screen Y relative to row offset
					// Need to find Rx corresponding to the matchIndex (render column)
					E.Rx = 0
					cx := 0
					for rx, char := range row.Chars {
						if cx >= matchIndex {
							E.Rx = rx
							break
						}
						if char == byte(tab) {
							cx += (tabStop - 1) - (cx % tabStop)
						}
						cx++
						E.Rx = rx + 1 // Keep track of character index
					}

					E.Cx = matchIndex // Render position
					E.Rowoff = current // Ensure the matched line is visible (scrolling will adjust)

					// Adjust Cy and Rowoff based on screen height
                    if E.Cy < 0 || E.Cy >= E.ScreenRows {
						E.Rowoff = current
						E.Cy = 0 // Place cursor at top if scrolled significantly
						if current > E.ScreenRows / 2 { // Center roughly if possible
							E.Rowoff = current - E.ScreenRows / 2
							E.Cy = E.ScreenRows / 2
						}
                    }

					editorScroll() // Adjust Coloff if necessary
					break          // Stop searching for this iteration
				}
			}
			// Reset direction after search attempt
			direction = 0
		}
	}
}

// ========================= Editor events handling  ========================

// editorMoveCursor handles cursor movement based on arrow key presses.
func editorMoveCursor(key KeyAction) {
	fileRow := E.Rowoff + E.Cy // Current line index in the file buffer
	var row *erow
	if fileRow >= 0 && fileRow < E.NumRows {
		row = &E.Row[fileRow]
	} else {
		row = nil // Cursor is potentially outside the file rows (e.g., on tilde lines)
	}

	switch key {
	case arrowLeft:
		if E.Rx > 0 {
			E.Rx--
			// Update Cx based on the new Rx
			if row != nil {
				editorUpdateCxFromRx(row)
			} else {
				E.Cx-- // Simple decrement if no row data
			}
		} else if fileRow > 0 {
			// Move to the end of the previous line
			E.Cy--
			prevRow := &E.Row[fileRow-1]
			E.Rx = prevRow.Size // Move to end char index
			editorUpdateCxFromRx(prevRow) // Update render position
		}
		// If Cx moved left off the screen, editorScroll will handle Coloff
	case arrowRight:
		if row != nil && E.Rx < row.Size {
			E.Rx++
			// Update Cx based on the new Rx
			editorUpdateCxFromRx(row)
		} else if row != nil && E.Rx == row.Size && fileRow < E.NumRows-1 {
			// Move to the beginning of the next line
			E.Cy++
			E.Rx = 0
			E.Cx = 0 // Render position is also 0
		}
		// If Cx moved right off the screen, editorScroll will handle Coloff
	case arrowUp:
		if E.Cy > 0 {
			E.Cy--
			// When moving up/down, try to maintain the horizontal *render* position (Cx).
			// We need to find the Rx on the new line that corresponds best to the old Cx.
			newFileRow := E.Rowoff + E.Cy
			if newFileRow >= 0 && newFileRow < E.NumRows {
				newRow := &E.Row[newFileRow]
				editorUpdateRxFromCx(newRow) // Find Rx for current Cx on new row
				// Ensure Rx doesn't go beyond the actual characters
				if E.Rx > newRow.Size {
					E.Rx = newRow.Size
				}
				// Recalculate Cx based on the possibly adjusted Rx
				editorUpdateCxFromRx(newRow)
			} else {
				// Moving onto a tilde line or above file start
				E.Rx = 0
				E.Cx = 0
			}
		} else if E.Rowoff > 0 {
			// Scroll up if at the top of the screen but not the file
			E.Rowoff--
			// Try to maintain position as above
			newFileRow := E.Rowoff + E.Cy // Should be E.Rowoff + 0
			if newFileRow >= 0 && newFileRow < E.NumRows {
				newRow := &E.Row[newFileRow]
				editorUpdateRxFromCx(newRow)
				if E.Rx > newRow.Size { E.Rx = newRow.Size }
				editorUpdateCxFromRx(newRow)
			} else {
				E.Rx = 0; E.Cx = 0;
			}
		}
	case arrowDown:
		if E.Cy < E.ScreenRows-1 {
			fileRowBelow := E.Rowoff + E.Cy + 1
			if fileRowBelow < E.NumRows { // Only move down if there's a line below
				E.Cy++
				newRow := &E.Row[fileRowBelow]
				editorUpdateRxFromCx(newRow) // Find Rx for current Cx on new row
				if E.Rx > newRow.Size {
					E.Rx = newRow.Size
				}
				editorUpdateCxFromRx(newRow) // Recalculate Cx
			}
		} else if E.Rowoff+E.ScreenRows < E.NumRows {
			// Scroll down if at the bottom of the screen but not the file
			E.Rowoff++
			// Try to maintain position as above
			newFileRow := E.Rowoff + E.Cy // Cy is ScreenRows-1
			if newFileRow < E.NumRows {
				newRow := &E.Row[newFileRow]
				editorUpdateRxFromCx(newRow)
				if E.Rx > newRow.Size { E.Rx = newRow.Size }
				editorUpdateCxFromRx(newRow)
			} else {
				// This case should technically not be reachable if check above is correct
				E.Rx = 0; E.Cx = 0;
			}
		}
	}

	// Final check: Ensure Rx/Cx are not beyond the end of the *current* line after movement
	finalFileRow := E.Rowoff + E.Cy
	var finalRow *erow
	if finalFileRow >= 0 && finalFileRow < E.NumRows {
		finalRow = &E.Row[finalFileRow]
		if E.Rx > finalRow.Size {
			E.Rx = finalRow.Size
			editorUpdateCxFromRx(finalRow) // Correct Cx if Rx was adjusted
		}
	} else {
		// If cursor ended up outside file rows (e.g., moved down from last line)
		E.Rx = 0
		E.Cx = 0
	}

}

// editorProcessKeypress handles a single keypress event.
func editorProcessKeypress() {
	key := editorReadKey()

	switch key {
	case enter:
		editorInsertNewline()
	case ctrlC:
		// Ignore Ctrl-C in this version, like Kilo C
	case ctrlQ:
		if E.Dirty > 0 && E.QuitTimes > 0 {
			editorSetStatusMessage("WARNING! File has unsaved changes. Press Ctrl-Q %d more times to quit.", E.QuitTimes)
			E.QuitTimes--
			return // Don't exit yet
		}
		// Clean exit
		disableRawMode() // Explicitly restore terminal
		os.Stdout.WriteString("\x1b[2J") // Clear screen
		os.Stdout.WriteString("\x1b[H")  // Go home
		os.Exit(0)
	case ctrlS:
		editorSave()
	case ctrlF:
		editorFind()
	case backspace, ctrlH: // Backspace
		editorDelChar()
	case delKey: // Delete key (forward delete)
		// Kilo C moves cursor right then calls delete char (backspace)
		// We can implement forward delete directly
		editorDelKeyForward()
	case pageUp, pageDown:
		// Move cursor to top or bottom of screen
		if key == pageUp {
			E.Cy = 0
		} else { // pageDown
			E.Cy = E.ScreenRows - 1
		}
		// Move cursor up/down by a full screen height
		times := E.ScreenRows
		moveDir := arrowUp
		if key == pageDown {
			moveDir = arrowDown
		}
		for times > 0 {
			editorMoveCursor(moveDir)
			times--
		}
	case homeKey: // Move cursor to beginning of line
		E.Rx = 0
		E.Cx = 0
	case endKey: // Move cursor to end of line
		fileRow := E.Rowoff + E.Cy
		if fileRow < E.NumRows {
			row := &E.Row[fileRow]
			E.Rx = row.Size
			editorUpdateCxFromRx(row) // Update Cx based on end Rx
		} else {
			E.Rx = 0
			E.Cx = 0
		}

	case arrowUp, arrowDown, arrowLeft, arrowRight:
		editorMoveCursor(key)
	case ctrlL:
		// Kilo C documentation says Ctrl+L refreshes the screen,
		// but the C code doesn't have explicit handling. Refresh happens
		// in the main loop anyway. We can leave this empty or force refresh.
		// No explicit action needed here as refresh happens on loop.
	case esc:
		// No action for ESC in normal mode (used in Find mode)
	default:
		// Insert character if it's printable (or Tab)
		// Need a better check than just >= 32 for Unicode potentially
		// For simplicity matching Kilo C: handle Tab and basic printables
		if key == tab || (key >= 32 && key <= 126) || key >= 128 { // Allow bytes >= 128 for UTF-8 etc.
			editorInsertChar(byte(key))
		}
	}

	// Reset quit counter if the key was not Ctrl-Q trying to quit dirty file
	if key != ctrlQ || E.Dirty == 0 {
		E.QuitTimes = kiloQuitTimes
	}
}

// editorPrompt gets a line of input from the user in the status bar.
// Returns the input string, or empty string if canceled (ESC).
// callback, if not nil, is called for syntax highlighting/validation during input.
func editorPrompt(promptFormat string, callback func(query string, key KeyAction)) string {
	query := ""
	for {
		prompt := fmt.Sprintf(promptFormat, query)
		editorSetStatusMessage(prompt)
		editorRefreshScreen()

		key := editorReadKey()

		switch key {
		case backspace, ctrlH, delKey:
			if len(query) > 0 {
				// Handle UTF-8 correctly if necessary, for now byte-based:
				query = query[:len(query)-1]
			}
		case esc, ctrlC:
			editorSetStatusMessage("") // Clear prompt message
			if callback != nil {
				callback(query, key) // Notify callback about cancellation
			}
			return "" // Cancelled
		case enter:
			if len(query) > 0 {
				editorSetStatusMessage("") // Clear prompt message
				if callback != nil {
					callback(query, key) // Notify callback about confirmation
				}
				return query // Confirmed
			}
		default:
			// Append printable characters
			if key >= 32 && key <= 126 { // Basic printable ASCII
				if len(query) < kiloQueryLen-1 { // Leave space for null term equiv? Not really needed
					query += string(byte(key))
				}
			}
		}
		// Call callback on each keypress (except Enter/Esc)
		if callback != nil && key != enter && key != esc && key != ctrlC {
			callback(query, key)
		}
	}
}


// =============================== Initialization ============================

func updateWindowSize() {
	if err := getWindowSize(); err != nil {
		die("Unable to query screen size", err)
	}
	// Adjust cursor if it's now outside the bounds
	if E.Cy >= E.ScreenRows {
		E.Cy = E.ScreenRows - 1
	}
	if E.Cx >= E.ScreenCols {
		E.Cx = E.ScreenCols - 1
	}
    // Need to update Rx based on adjusted Cx
	fileRow := E.Rowoff + E.Cy
	if fileRow >= 0 && fileRow < E.NumRows {
		row := &E.Row[fileRow]
		editorUpdateRxFromCx(row)
		if E.Rx > row.Size { E.Rx = row.Size }
		// Re-update Cx just in case Rx was clamped
		editorUpdateCxFromRx(row)
		// Re-clamp Cx after potential change
		if E.Cx >= E.ScreenCols { E.Cx = E.ScreenCols - 1 }
	} else {
		E.Rx = 0
		E.Cx = 0
	}

    editorScroll() // Re-apply scroll logic with new dimensions
}

func handleSigWinCh(c chan os.Signal) {
	for range c { // Loop forever waiting for signals
		updateWindowSize()
		editorRefreshScreen() // Redraw the screen immediately
	}
}

func initEditor() {
	E.Cx = 0
	E.Cy = 0
	E.Rx = 0
	E.Rowoff = 0
	E.Coloff = 0
	E.NumRows = 0
	E.Row = nil // Initialize as empty slice
	E.Dirty = 0
	E.Filename = ""
	E.Syntax = nil
	E.StatusMsg = ""
	E.QuitTimes = kiloQuitTimes

	// Get initial window size
	if err := getWindowSize(); err != nil {
		// Use defaults if error getting size initially
		log.Printf("Warning: could not get window size: %v. Using defaults 80x24.", err)
		E.ScreenCols = 80
		E.ScreenRows = 24 // This already accounts for status bars subtracted in getWindowSize
	}

	// Initialize Syntax Highlighting Database
	initSyntaxDB()

	// Set up SIGWINCH handler
	sigWinCh := make(chan os.Signal, 1)
	signal.Notify(sigWinCh, syscall.SIGWINCH)
	go handleSigWinCh(sigWinCh) // Run handler in a separate goroutine

	// Set up handler for common termination signals to restore terminal state
	sigTerm := make(chan os.Signal, 1)
	signal.Notify(sigTerm, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigTerm // Wait for signal
		disableRawMode()
		os.Stdout.WriteString("\x1b[2J") // Clear screen
		os.Stdout.WriteString("\x1b[H")  // Go home
		os.Exit(1) // Exit after receiving signal
	}()
}

func main() {
	// Check for filename argument
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: lkjsxcgoeditor <filename>\n")
		os.Exit(1)
	}
	filename := os.Args[1]

	// Prepare terminal
	enableRawMode()
	defer disableRawMode() // Ensure terminal state is restored on exit

	// Initialize editor state
	initEditor()

	// Open the specified file
	if err := editorOpen(filename); err != nil {
		// If opening failed fatally (not just file not found), die.
		// File not found is handled in editorOpen by starting empty.
		if !errors.Is(err, os.ErrNotExist) {
			die("Failed to open file", err)
		}
	}

	// Set initial status message
	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find")

	// Main event loop
	for {
		editorRefreshScreen()
		editorProcessKeypress()
	}
}