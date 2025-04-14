package terminal

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

type Terminal struct {
	originalTermios syscall.Termios
}

type Result int

const (
	ResultOk Result = iota
	ResultErr
)

func NewTerminal() (*Terminal, Result) {
	t := &Terminal{}
	err := t.initTerminal()
	if err != nil {
		return nil, ResultErr
	}
	return t, ResultOk
}

func (t *Terminal) initTerminal() error {
	fd := uintptr(syscall.Stdin)
	if _, _, err := syscall.Syscall6(syscall.SYS_IOCTL, fd, uintptr(syscall.TCGETS), uintptr(unsafe.Pointer(&t.originalTermios)), 0, 0, 0); err != 0 {
		return err
	}

	termios := t.originalTermios
	termios.Lflag &^= syscall.ICANON // Disable canonical mode
	termios.Lflag &^= syscall.ECHO   // Disable echo
	termios.Cc[syscall.VMIN] = 1     // Minimum number of characters to read
	termios.Cc[syscall.VTIME] = 0    // No timeout

	if _, _, err := syscall.Syscall6(syscall.SYS_IOCTL, fd, uintptr(syscall.TCSETS), uintptr(unsafe.Pointer(&termios)), 0, 0, 0); err != 0 {
		return err
	}
	return nil
}

func (t *Terminal) RestoreTerminal() error {
	fd := uintptr(syscall.Stdin)
	if _, _, err := syscall.Syscall6(syscall.SYS_IOCTL, fd, uintptr(syscall.TCSETS), uintptr(unsafe.Pointer(&t.originalTermios)), 0, 0, 0); err != 0 {
		return err
	}
	return nil
}

func (t *Terminal) ReadKey() (byte, Result) {
	var buf [1]byte
	_, err := os.Stdin.Read(buf[:])
	if err != nil {
		return 0, ResultErr
	}
	if buf[0] == '\x1b' { // Escape sequence
		// Read next two bytes
		var escapeBuf [2]byte
		_, err = os.Stdin.Read(escapeBuf[:])
		if err != nil {
			return '\x1b', ResultOk // Just return ESC if error reading escape sequence
		}
		if escapeBuf[0] == '[' {
			switch escapeBuf[1] {
			case 'A':
				return KeyUp, ResultOk
			case 'B':
				return KeyDown, ResultOk
			case 'C':
				return KeyRight, ResultOk
			case 'D':
				return KeyLeft, ResultOk
			}
		}
		return '\x1b', ResultOk // Unknown escape sequence, return ESC
	}
	return buf[0], ResultOk
}

const (
	KeyUp    byte = 27 + 'A' // Assuming these values, might need to adjust
	KeyDown  byte = 27 + 'B'
	KeyRight byte = 27 + 'C'
	KeyLeft  byte = 27 + 'D'
)

func (t *Terminal) Print(s string) {
	fmt.Print(s)
}

func (t *Terminal) Println(s string) {
	fmt.Println(s)
}