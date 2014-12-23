mod3Or5 xs = [x | x <- xs, mod x 3 == 0 || mod x 5 == 0]

solve = sum(mod3Or5 [1..999])
