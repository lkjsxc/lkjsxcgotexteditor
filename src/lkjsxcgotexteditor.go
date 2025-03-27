package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

// EditorState holds the editor's data and state.
type EditorState struct {
	lines    []string
	filename string
	cursorX  int
	cursorY  int
	modified bool
}

// NewEditorState initializes a new editor state.
func NewEditorState() *EditorState {
	return &EditorState{
		lines:    []string{""}, // Start with an empty line
		filename: "untitled.txt",
		cursorX:  0,
		cursorY:  0,
		modified: false,
	}
}

// loadFile reads a file into the editor buffer.
func (e *EditorState) loadFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("error opening file '%s': %w", filename, err) // More informative error
	}
	defer file.Close()

	e.lines = []string{} // Clear existing buffer
	reader := bufio.NewReader(file)
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				if line != "" { // Handle last line if not empty
					e.lines = append(e.lines, strings.TrimRight(line, "\n"))
				}
				break // End of file
			}
			return fmt.Errorf("error reading file '%s': %w", filename, err) // More informative error
		}
		e.lines = append(e.lines, strings.TrimRight(line, "\n")) // Remove newline
	}
	e.filename = filename
	e.cursorX = 0
	e.cursorY = 0
	e.modified = false // Loading a file is not considered modification yet.
	return nil
}

// saveFile writes the buffer content to the current filename.
func (e *EditorState) saveFile() error {
	return e.saveFileAs(e.filename)
}

// saveFileAs writes the buffer content to a specified filename.
func (e *EditorState) saveFileAs(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("error creating/opening file '%s' for saving: %w", filename, err) // More informative error
	}
	defer file.Close()

	writer := bufio.NewWriter(file)
	for _, line := range e.lines {
		_, err := writer.WriteString(line + "\n")
		if err != nil {
			return fmt.Errorf("error writing to file '%s': %w", filename, err) // More informative error
		}
	}
	if err := writer.Flush(); err != nil {
		return fmt.Errorf("error flushing buffer to file '%s': %w", filename, err) // More informative error
	}
	e.filename = filename
	e.modified = false // Saving resets modified status
	return nil
}

// insertChar inserts a character at the cursor position.
func (e *EditorState) insertChar(char rune) {
	line := []rune(e.lines[e.cursorY])
	if e.cursorX > len(line) {
		e.cursorX = len(line) // Correct cursor if it's out of bounds (shouldn't happen normally)
	}
	newLine := append(line[:e.cursorX], append([]rune{char}, line[e.cursorX:]...)...)
	e.lines[e.cursorY] = string(newLine)
	e.cursorX++
	e.modified = true
}

// insertLine inserts a new line at the cursor position.
func (e *EditorState) insertLine() {
	line := []rune(e.lines[e.cursorY])
	currentLinePart1 := string(line[:e.cursorX]) // Part of the current line before the cursor
	currentLinePart2 := string(line[e.cursorX:]) // Part of the current line after the cursor

	e.lines[e.cursorY] = currentLinePart1 // Update the current line with the first part

	// Insert a new line below the current line and put the second part there
	e.lines = append(e.lines[:e.cursorY+1], append([]string{currentLinePart2}, e.lines[e.cursorY+1:]...)...)

	e.cursorY++   // Move cursor to the new line
	e.cursorX = 0 // Cursor starts at the beginning of the new line
	e.modified = true
}

// deleteChar deletes the character before the cursor.
func (e *EditorState) deleteChar() {
	if e.cursorX > 0 {
		line := []rune(e.lines[e.cursorY])
		newLine := append(line[:e.cursorX-1], line[e.cursorX:]...)
		e.lines[e.cursorY] = string(newLine)
		e.cursorX--
		e.modified = true
	} else if e.cursorY > 0 { // Delete line break - join current line with previous line
		prevLine := e.lines[e.cursorY-1]
		currentLine := e.lines[e.cursorY]
		e.lines[e.cursorY-1] = prevLine + currentLine
		e.lines = append(e.lines[:e.cursorY], e.lines[e.cursorY+1:]...) // Remove current line
		e.cursorY--
		e.cursorX = len(prevLine) // Cursor to end of previous line
		e.modified = true
	}
}

// moveCursorLeft moves the cursor one position to the left.
func (e *EditorState) moveCursorLeft() {
	if e.cursorX > 0 {
		e.cursorX--
	} else if e.cursorY > 0 {
		e.cursorY--
		e.cursorX = len(e.lines[e.cursorY]) // Move to end of previous line
	}
}

// moveCursorRight moves the cursor one position to the right.
func (e *EditorState) moveCursorRight() {
	if e.cursorX < len(e.lines[e.cursorY]) {
		e.cursorX++
	} else if e.cursorY < len(e.lines)-1 {
		e.cursorY++
		e.cursorX = 0 // Move to beginning of next line
	}
}

// moveCursorUp moves the cursor one line up.
func (e *EditorState) moveCursorUp() {
	if e.cursorY > 0 {
		e.cursorY--
		if e.cursorX > len(e.lines[e.cursorY]) {
			e.cursorX = len(e.lines[e.cursorY]) // Keep cursor within line bounds
		}
	}
}

// moveCursorDown moves the cursor one line down.
func (e *EditorState) moveCursorDown() {
	if e.cursorY < len(e.lines)-1 {
		e.cursorY++
		if e.cursorX > len(e.lines[e.cursorY]) {
			e.cursorX = len(e.lines[e.cursorY]) // Keep cursor within line bounds
		}
	}
}

// display draws the editor content to the terminal.
func (e *EditorState) display() {
	fmt.Print("\033[H\033[2J") // Clear screen (ANSI escape codes)
	fmt.Printf("File: %s", e.filename)
	if e.modified {
		fmt.Print(" (modified)")
	}
	fmt.Println()

	for i, line := range e.lines {
		if i == e.cursorY {
			fmt.Printf("%s\033[7m%s\033[0m%s\n", line[:e.cursorX], string('_'), line[e.cursorX:]) // Show cursor
		} else {
			fmt.Println(line)
		}
	}
	fmt.Printf("\nCursor: %d:%d\n", e.cursorY+1, e.cursorX+1) // 1-based indexing for user display
}

// handleInput processes user input and updates editor state.
func handleInput(e *EditorState, input string) bool {
	if len(input) == 0 {
		return true // Continue
	}

	switch input {
	case "\x03": // Ctrl+C
		return false // Quit
	case "\r": // Enter
		e.insertLine()
	case "\x7f", "\b": // Backspace (both DEL and BS might send these)
		e.deleteChar()
	case "\x1b[A": // Up Arrow
		e.moveCursorUp()
	case "\x1b[B": // Down Arrow
		e.moveCursorDown()
	case "\x1b[C": // Right Arrow
		e.moveCursorRight()
	case "\x1b[D": // Left Arrow
		e.moveCursorLeft()
	case ":q":
		if e.modified {
			fmt.Println("File modified. Use :q! to quit without saving or :w to save.")
			return true
		}
		return false // Quit
	case ":q!":
		return false // Force quit
	case ":w":
		if err := e.saveFile(); err != nil {
			fmt.Println("Error saving file:", err)
		} else {
			fmt.Println("File saved.")
		}
	case ":wq", ":xw", ":x": // Save and quit
		if err := e.saveFile(); err != nil {
			fmt.Println("Error saving file:", err)
			return true // Stay in editor if save fails
		} else {
			fmt.Println("File saved.")
			return false // Quit after saving
		}
	case ":e": // Reload current file (discard changes)
		if e.modified {
			fmt.Println("File modified. Use :e! to reload without saving or :w to save.")
			return true
		}
		if err := e.loadFile(e.filename); err != nil {
			fmt.Println("Error reloading file:", err)
		} else {
			fmt.Println("File reloaded.")
		}
	case ":e!": // Force reload
		if err := e.loadFile(e.filename); err != nil {
			fmt.Println("Error reloading file:", err)
		} else {
			fmt.Println("File reloaded.")
		}
	case ":o": // Open another file
		fmt.Print("Open file: ")
		reader := bufio.NewReader(os.Stdin)
		filename, _ := reader.ReadString('\n')
		filename = strings.TrimSpace(filename)
		if filename != "" {
			if err := e.loadFile(filename); err != nil {
				fmt.Println("Error opening file:", err)
			}
		}
	case ":n", ":new": // New file
		e.lines = []string{""}
		e.filename = "untitled.txt"
		e.cursorX = 0
		e.cursorY = 0
		e.modified = false
		fmt.Println("New file created.")

	default:
		if len(input) == 1 { // Normal character input
			e.insertChar(rune(input[0]))
		} else {
			// Handle potential multi-byte characters or other escape sequences not handled above.
			// For simplicity, treat as string input. Could be improved for proper UTF-8 handling.
			for _, r := range input {
				e.insertChar(r)
			}
		}
	}
	return true // Continue editing
}

func main() {
	editor := NewEditorState()

	if len(os.Args) > 1 {
		filename := os.Args[1]
		if err := editor.loadFile(filename); err != nil {
			fmt.Println("Error opening file:", err)
			fmt.Println("Starting with a new file.")
		}
	}

	reader := bufio.NewReader(os.Stdin)

	fmt.Println("Simple Go Text Editor - mattn style (kinda)")
	fmt.Println("Commands: :q (quit), :w (save), :wq (save and quit), :e (reload), :o (open), :n (new)")
	fmt.Println("         :q! (force quit), :e! (force reload)")
	fmt.Println("Ctrl+C to quit.")

	editing := true
	for editing {
		fmt.Print("> ") // Prompt - moved before display
		editor.display()

		input, _ := reader.ReadString('\n')
		input = strings.TrimRight(input, "\n") // Remove newline from input

		editing = handleInput(editor, input)
	}

	fmt.Println("Exiting.")
}
