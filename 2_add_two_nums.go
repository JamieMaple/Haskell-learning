package main

type ListNode struct {
	Val  int
	Next *ListNode
}

func addTwoNumbers(l1 *ListNode, l2 *ListNode) *ListNode {
	l3 := new(ListNode)
	newNode := l3
	largerThanTen := false

	for l1 != nil || l2 != nil {
		if l1 != nil {
			newNode.Val += l1.Val
			l1 = l1.Next
		}
		if l2 != nil {
			newNode.Val += l2.Val
			l2 = l2.Next
		}
		if largerThanTen {
			newNode.Val++
		}

		if newNode.Val >= 10 {
			newNode.Val -= 10
			largerThanTen = true
		} else {
			largerThanTen = false
		}

		if l1 != nil || l2 != nil {
			var nextNode ListNode
			newNode.Next = &nextNode
			newNode = &nextNode
		}
	}

	if largerThanTen {
		nextNode := ListNode{1, nil}
		newNode.Next = &nextNode
		newNode = &nextNode
	}

	return l3
}
