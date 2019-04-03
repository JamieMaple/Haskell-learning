package main

func isPalindromic(s string, i int, j int) bool {
	if i == j {
		return true
	}

	if s[i] == s[j] {
		return isPalindromic(s, i+1, j-1)
	}

	return false
}

func longestPalindromeOn3(s string) string {
	longestPalindrome := ""

	for i := 0; i < len(s); i++ {
		for j := i; j < len(s); j++ {
			if length := len(s[i : j+1]); isPalindromic(s, i, j) && len(longestPalindrome) < length {
				longestPalindrome = s[i : j+1]
			}
		}
	}

	return longestPalindrome
}

func longestPalindrome(s string) string {
	longest := ""
	matrix := make([][]bool, len(s))
	for i := range matrix {
		matrix[i] = make([]bool, len(s))
	}

	for i := range s {
		matrix[i][i] = true
		for j := 0; j <= i; j++ {
			if s[i] == s[j] {
				if i-j < 2 || matrix[i+1][j-1] {
					matrix[i][j] = true
					if offset := i - j + 1; offset > len(longest) {
						longest = s[j : i+1]
					}
				}
			}
		}
	}

	return longest
}
