package main

func mergeN(nums1 []int, nums2 []int, n int) *[]int {
	i, j := 0, 0
	nums := []int{}

	for i+j < n {
		smallOne := 0
		if i < len(nums1) && j < len(nums2) {
			if nums1[i] <= nums2[j] {
				smallOne = nums1[i]
				i++
			} else {
				smallOne = nums2[j]
				j++
			}
		} else {
			if i < len(nums1) {
				smallOne = nums1[i]
				i++
			}
			if j < len(nums2) {
				smallOne = nums2[j]
				j++
			}
		}

		nums = append(nums, smallOne)
	}

	return &nums
}

func findMedianSortedArrays(nums1 []int, nums2 []int) float64 {
	len1, len2 := len(nums1), len(nums2)
	totalLen := len1 + len2
	isOddLen := true
	var median float64

	if totalLen%2 == 0 {
		isOddLen = false
	}

	middle := totalLen / 2
	temp := *mergeN(nums1, nums2, middle+1)

	if isOddLen {
		median = float64(temp[middle])
	} else {
		median = float64(temp[middle]+temp[middle-1]) / 2
	}

	return median
}
