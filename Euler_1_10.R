# 1: 233168
n <- seq(1:999)
sum(n[(n %% 3 == 0) | (n %% 5 == 0)])

# 2a: 4613732
fc <- function(fib) {
  newnum <- sum(tail(fib, 2))
  if (newnum <= 4000000) fib <- fc(c(fib, newnum))
  return(fib)
}
fib_seq <- fc(c(1, 2))
sum(fib_seq[fib_seq %% 2 == 0])

# 2b
fc <- function(fib) {
  newnum <- sum(tail(fib, 2))
  if (newnum > 4000000) {
    return(fib)
  }
  return(fc(c(fib, newnum)))
}
fib_seq <- fc(c(1, 2))
sum(fib_seq[fib_seq %% 2 == 0])

# 3: 6857
factorize <- function(x) {
  maxfac <- floor(sqrt(x))
  for (i in seq(2, maxfac)) {
    if (x %% i == 0) {
      return(c(i, factorize(x / i)))
    }
  }
  return(x)
}
max(factorize(600851475143))

# 4: 906609
is_palindrom <- function(n) {
  digits <- strsplit(as.character(n), "")
  digits_rev <- lapply(digits, rev)
  n_reversed <- sapply(digits_rev, paste, collapse = "")
  return(n_reversed == as.character(n))
}

# 4a (modern computer)
prod_all <- c((100:999) %*% t(100:999))
max(prod_all[is_palindrom(prod_all)])

# 4b (faster)
maxpal <- 0
for (n1 in seq(999, 100)) {
  if ((n1 * 999) < maxpal) break
  for (n2 in seq(999, n1)) {
    if (is_palindrom(n1 * n2)) {
      maxpal <- n1 * n2
      break
    }
  }
}
print(maxpal)

# 5: 232792560
2 * 3 * 2 * 5 * 7 * 2 * 3 * 11 * 13 * 2 * 17 * 19

# 6: 25164150
sum(seq(100))^2 - sum(seq(1, 100)^2)

# 7: 104743
is_prime <- function(x) {
  if (x %in% c(2, 3)) {
    return(TRUE)
  }
  maxfac <- floor(sqrt(x))
  for (i in seq(2, maxfac)) {
    if (x %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

primecnt <- 0
cur <- 1
repeat{
  cur <- cur + 1
  if (is_prime(cur)) primecnt <- primecnt + 1
  if (primecnt == 10001) break
}
print(cur)

# 8: 23514624000; "5" "5" "7" "6" "6" "8" "9" "6" "6" "4" "8" "9" "5"
bignum <- "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
splitnum <- strsplit(bignum, "")[[1]]
nprod <- 13
prod_max <- 0
for (i in seq(nprod, 1000)) {
  prod_cur <- prod(as.numeric(splitnum[seq(i - nprod + 1, i)]))
  if (prod_cur > prod_max) {
    prod_max <- prod_cur
    numbers_max <- splitnum[seq(i - nprod + 1, i)]
  }
}
print(prod_max)
print(numbers_max)

# 9: 200, 375, 425
searchsum <- 1000
triplet <- numeric()
for (c in seq(ceiling(searchsum / 3), floor(searchsum / 2))) {
  for (b in seq(c - 1, ceiling(c / 2))) {
    a <- searchsum - b - c
    if (a^2 + b^2 == c^2) {
      triplet <- c(a, b, c)
      break
    }
  }
  if (length(triplet) > 0) break
}
print(triplet)

# 10: 142913828922

# 10a (slow)
topnum <- 2000000
sum(seq(2, topnum)[sapply(seq(2, topnum), is_prime)])

# 10b (nice!)
topnum <- 2000000
allnum <- seq(topnum)
number_type <- rep("unknown", topnum)
for (n in seq(2, topnum)) {
  if (number_type[n] == "non-prime") next
  number_type[n] <- "prime"
  maxmul <- topnum %/% n
  if (maxmul > 1) number_type[seq(2, maxmul) * n] <- "non-prime"
}
sum(allnum[number_type == "prime"])
