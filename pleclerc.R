
nums.in.list <- c(6,10,14,15,21,22,26,33,34,35,38,39,46,51,55,57,58,62,65,69,75,77,82,85,86,87,91,93,94,95)
max.of.list = max(nums.in.list)

# tried my own sieve of eratosthenes
max.to.check <- floor(max.of.list/2) # since 2 is the smallest prime, if a number is the product of two unique primes, 
# the largest possible candidate prime is at most half the list max rounded down
primes <- 2:max.to.check
for(i in 2:max.to.check){ # not maximally efficient, but rather not use primes here since it gets dynamically modified
  primes <- primes[primes == i | primes %% i != 0] # first part keeps the original candidate, 
  # second part removes multiples of it
}

good.nums <- rep(0,times = max.to.check) # initializing a numeric vector to hold all numbers in range 
# that are products of two unique primes
for(j in 1:(length(primes)-1)){ # ending at index length(primes)-1 avoids out of bounds issues
  for(k in (j+1):length(primes)){ # starting at index j+1 excludes all squares of primes
    n <- primes[j]*primes[k] # product of two unique primes, no duplicates since
    # j<k is an invariant + fundamental theorem of arithmetic
    if(n <= max.of.list){ # not interested in numbers outside our given list range, also avoids index issues
      good.nums[n] <- n; # avoids having to count instances of "good nums"
    }
  }
}
good.nums <- good.nums[which(good.nums != 0)] # filter out the zeros at indices of "bad nums"

(incorrect.num <- setdiff(nums.in.list, good.nums)) # s-e
(replacement.num <- setdiff(good.nums, nums.in.list)) 
