package main

func indexOf(nums []int, val int) int {
	for i := 0; i < len(nums); i++ {
		if val == nums[i] {
			return i
		}
	}
	return -1
}

func twoSum(nums []int, target int) []int {
	i := 0
	j := 0

	for ; i < len(nums); i++ {
		cur := nums[i]
		other := target - cur
		j = indexOf(nums, other)
		if j > -1 && i != j {
			break
		}
	}

	return []int{i, j}
}

func twoSum2(nums []int, target int) []int {
	numMap := make(map[int]int)

	for i := 0; i < len(nums); i++ {
		cur := nums[i]
		if j, ok := numMap[target-cur]; ok && i != j {
			return []int{i, j}
		}
		numMap[cur] = i
	}

	return nil
}
