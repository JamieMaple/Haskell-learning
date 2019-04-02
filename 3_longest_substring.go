package main

func allCharUnique(s string) bool {
	m := make(map[rune]int)
	for i, v := range s {
		if j, ok := m[v]; ok {
			if i != j {
				return false
			}
		}
		m[v] = i
	}
	return true
}

func lengthOfLongestSubstring(s string) int {
	max := 1
	for i := 0; i < len(s); i++ {
		for j := i + 2; j < len(s); j++ {
			offset := j - i
			if allCharUnique(s[i:j]) && offset > max {
				max = offset
			}
		}
	}

	return max
}
