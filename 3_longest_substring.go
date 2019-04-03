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

func lengthOfLongestSubstringOn3(s string) int {
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

// slide window
func lengthOfLongestSubstringO2n(s string) int {
	max := 0
	i := 0
	j := 0
	var set map[byte]bool

	for i < len(s) && j < len(s) {
		v := s[j]
		if ok := set[v]; !ok {
			set[v] = true
			j++
			offset := j - i
			if max < offset {
				max = offset
			}
		} else {
			v = s[i]
			set[v] = false
			i++
		}
	}

	return max
}

// slide window optimized
func lengthOfLongestSubstringOnOptimized(s string) int {
	max := 0
	j := 0
	m := make(map[byte]int)

	for i := -1; j < len(s); j++ {
		v := s[j]

		if ii, ok := m[v]; ok && ii > i {
			i = ii
		}

		if offset := j - i; offset > max {
			max = offset
		}

		m[v] = j
	}

	return max
}

func lengthOfLongestSubstring(s string) int {
	max := 0
	i := -1
	m := [128]int{}

	for j := range m {
		m[j] = -10
	}

	for j := 0; j < len(s); j++ {
		index := int(s[j])

		if i < m[index] {
			i = m[index]
		}

		offset := j - i
		if max < offset {
			max = offset
		}

		m[index] = j
	}

	return max
}
