package main

import "fmt"
import "math"

func main() {
	const primeLimit = 100000000
	primeCache := make([]bool, primeLimit + 1)


	for i := 2; i < primeLimit; i += 1 {
		primeCache[i] = true
	}
	primeCache[0] = false
	primeCache[1] = false

	for i := 1; i < primeLimit; i += 1 {
		if primeCache[i] == true {
			for n := 2; n * i < primeLimit; n++ {
				primeCache[i * n] = false
			}
		}
	}

	tot := 1
	candidate := 0
	for n := 0; candidate < primeLimit; n++ {
		candidate = 4 * n + 2
		if candidate < primeLimit && (primeCache[candidate + 1] == true) {
			if (primeCache[candidate / 2 + 2] == true) {
				if IsPrimeGenerating(candidate, primeCache) {
					tot += candidate
				}
			}
		}
	}

	fmt.Printf(fmt.Sprintf("IsPrimeGenerating sum: %d\n", tot))
}

func IsPrimeGenerating(n int, primeCache []bool) bool {
	for x := 1; x <= int(math.Sqrt(float64(n))); x++ {
		if n % x == 0 && primeCache[x+n/x] != true {
			return false
		}
	}
	return true
}
