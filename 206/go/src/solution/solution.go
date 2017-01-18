package main

import "fmt"
import "math"

func main() {
	min := uint64(math.Sqrt(float64(1020304050607080900)))
	max := uint64(math.Sqrt(float64(1929394959697989990)))

	pattern := map[uint64]uint64 {
		10000000000000000000: 1,
		100000000000000000: 2,
		1000000000000000: 3,
		10000000000000: 4,
		100000000000: 5,
		1000000000: 6,
		10000000: 7,
		100000: 8,
		1000: 9,
		10: 0,
	}
	var valid bool = false

	for i := min; i < max; i++ {
		sq := i * i
		// fmt.Printf(fmt.Sprintf("check %d\n", sq))

		valid = true
		for pos, expect := range pattern {
			found :=  (sq % pos) / (pos / 10)
			if (expect != found) {
				valid = false
				break
			}
		}

		if valid {
			fmt.Printf("valid: %d\n", i)
			break
		}
	}
}
