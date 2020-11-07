package util

func IsNumber(b byte) bool {
	return b >= '0' && b <= '9'
}

func IsNumberAndLargerThanZero(b byte) bool {
	if b == '0' {
		return false
	}
	return IsNumber(b)
}

func IsUnderScore(b byte) bool {
	return b == '_'
}

func IsLetter(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
}

func IsLetterOrUnderscore(b byte) bool {
	return IsLetter(b) || IsUnderScore(b)
}

func IsLetterOrUnderscoreOrNumber(b byte) bool {
	return IsLetter(b) || IsUnderScore(b) || IsNumber(b)
}
