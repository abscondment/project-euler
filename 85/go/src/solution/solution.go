package main

import "fmt"
import "math"

func main() {
	error := math.MaxInt64
	closest := 0
	target := 2000000
	x := 2000
	y := 1

	for x >= y {
		rects := x * (x + 1) * y * (y + 1) / 4
		if error > int(math.Abs(float64(rects - target))) {
			closest = x * y
			error = int(math.Abs(float64(rects - target)))
		}

		if rects > target {
			x--
		} else {
			y++
		}
	}

	fmt.Printf(fmt.Sprintf("grid area: %d\n", closest))
}
