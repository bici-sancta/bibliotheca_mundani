# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...															patrick mcdevitt
# ...															06-mar-2017
# ...	converting values to/from base10 to base n and the reverse
# ...	
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


p7 <- function (n)
{
	string_digits <- character()
	
# ... loop thru all values from 0 to n-1
# ... convert each value to base 7
# ... add to accumulator string
	
	for (indx in 0 : (n-1))		
	{
		next_digit <- base10to7 (indx)	
		string_digits <- paste(string_digits, next_digit)	
	}
# ... return as string with all values (space separated)
	return(string_digits)	
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	test p7 function
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

print("")
print(" p7() function test cases ......")
test_value <- c(5, 15, 52)
for (n in 1 : length(test_value))
{
	digit_string <- p7(test_value[n])
	print(sprintf("p7(%d) = %s", test_value[n], digit_string))
}


# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	function that converts a decimal value to base 7
# ...		e.g., base10to7(100) => 202
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

base10to7 <- function(x)
{
	i = 0
	sum = 0
	while(x %/% 7 != 0)
	{
		sum <- sum + ((x %% 7) * (10^i))
		i = i + 1
		x <- x %/% 7
	}
	sum <- sum + ((x %% 7) * (10^i))
	return(sum)
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	test base10to7 function
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

print("")
print(" base10to7() function test cases ......")

xb10 <- c(7, 48, 49, 100, 343, 344)
for (ii in 1 : length(xb10))
{
	xb7 <- base10to7(xb10[ii])
	print(sprintf("base10 : %6d ==> base 7 : %6d", xb10[ii], xb7))
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	function that converts a base 7 value to decimal
# ...		e.g., base7to10(202) => 100
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

base7to10 <- function (xb7)
{
	
# ...	convert base7 value to vector of integers
# ...	multiply each digit by appropriate power of 7
# ...	add accumulation of each digit to overall base10 sum
	
	xb7_char <- as.character(xb7)
	xb7_digits <- as.integer(unlist((str_splt_revrse(xb7_char))))

	sum <- 0
	for (indx in 0 : (length(xb7_digits)-1))
	{
		sum <- sum + xb7_digits[indx+1] * (7^indx)
	}
	return(sum)
}

# ...	splits character string into a reversed list of single chararacters
str_splt_revrse <- function(x)
{
	lapply(strsplit(x, NULL), rev)
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	test base10to7 function
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

print("")
print(" base7to10() function test cases ......")

xb7 <- c(1001, 1000, 202, 100, 66, 10)
for (jj in 1 : length(xb7))
{
	xb10 <- base7to10(xb7[jj])
	print(sprintf("base7 : %6d ==> base 10 : %6d", xb7[jj], xb10))
}


# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	generalize to any base ... base 10 to base_y
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...	this is same logic as base10to7, except provide 2 input arguments
# .			x ... base 10 value to be converted to other base
# .			y ... base to which the value is to be converted
# .
# .		n.b. : this only accomodates up to base 10 ...
# .				to convert to higher order bases (e.g., 16)
# .				would require to add in capability to placehold digits with characters
# .				other than 0 - 9, e.g. A,B,C...

base_10_to_base_y <- function(x, y)
{
	i = 0
	sum = 0
	while(x %/% y != 0)
	{
		sum <- sum + ((x %% y) * (10^i))
		i = i + 1
		x <- x %/% y
	}
	sum <- sum + ((x %% y) * (10^i))
	return(sum)
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	test base10toany function
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

print("")
print(" base_10_to_base_y() function test cases ......")

base <- c(2, 2, 5, 5, 7, 7, 10, 10)
value <- c(8, 9, 20, 99, 100, 101, 100, 199)

for (jj in 1 : length(value))
{
	xby <- base_10_to_base_y(value[jj], base[jj])
	print(sprintf("base 10 : %6d ==> base[%2d] : %6d", value[jj], base[jj], xby))
}


# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	generalize to any base ... base_y to base_10
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...	this is same logic as base7to10, except provide 2 input arguments
# .			xby ... value to be converted to base 10
# .			y ... base from which the value is to be converted
# .
# .		n.b. : this only accomodates up to base 10 ...
# .				to convert to higher order bases (e.g., 16)
# .				would require to add in capability to placehold digits with characters
# .				other than 0 - 9, e.g. A,B,C...

base_y_to_base_10 <- function(xby, y)
{
	xby_char <- as.character(xby)
	xby_digits <- as.integer(unlist((str_splt_revrse(xby_char))))
	
	sum <- 0
	for (indx in 0 : (length(xby_digits)-1))
	{
		sum <- sum + xby_digits[indx+1] * (y^indx)
	}
	return(sum)
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	test base_y_to_base_10 function
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

print("")
print(" base_y_to_base_10() function test cases ......")


base <- c(2, 2, 5, 5, 7, 7, 10, 10)
value <- c(1000, 1001, 40, 344, 202, 203, 100, 199)

for (jj in 1 : length(value))
{
	xby <- base_y_to_base_10(value[jj], base[jj])
	
	print(sprintf("base[%2d] : %6d ==> base10 : %6d", base[jj], value[jj], xby))
}

# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	This is console output of above code
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# [1] ""
# [1] " p7() function test cases ......"
# [1] "p7(5) =  0 1 2 3 4"
# [1] "p7(15) =  0 1 2 3 4 5 6 10 11 12 13 14 15 16 20"
# [1] "p7(52) =  0 1 2 3 4 5 6 10 11 12 13 14 15 16 20 21 22 23 24 25 26 30 31 32 33 34 35 36 40 41 42 43 44 45 46 50 51 52 53 54 55 56 60 61 62 63 64 65 66 100 101 102"
# [1] ""
# [1] " base10to7() function test cases ......"
# [1] "base10 :      7 ==> base 7 :     10"
# [1] "base10 :     48 ==> base 7 :     66"
# [1] "base10 :     49 ==> base 7 :    100"
# [1] "base10 :    100 ==> base 7 :    202"
# [1] "base10 :    343 ==> base 7 :   1000"
# [1] "base10 :    344 ==> base 7 :   1001"
# [1] ""
# [1] " base7to10() function test cases ......"
# [1] "base7 :   1001 ==> base 10 :    344"
# [1] "base7 :   1000 ==> base 10 :    343"
# [1] "base7 :    202 ==> base 10 :    100"
# [1] "base7 :    100 ==> base 10 :     49"
# [1] "base7 :     66 ==> base 10 :     48"
# [1] "base7 :     10 ==> base 10 :      7"
# [1] ""
# [1] " base_10_to_base_y() function test cases ......"
# [1] "base 10 :      8 ==> base[ 2] :   1000"
# [1] "base 10 :      9 ==> base[ 2] :   1001"
# [1] "base 10 :     20 ==> base[ 5] :     40"
# [1] "base 10 :     99 ==> base[ 5] :    344"
# [1] "base 10 :    100 ==> base[ 7] :    202"
# [1] "base 10 :    101 ==> base[ 7] :    203"
# [1] "base 10 :    100 ==> base[10] :    100"
# [1] "base 10 :    199 ==> base[10] :    199"
# [1] ""
# [1] " base_y_to_base_10() function test cases ......"
# [1] "base[ 2] :   1000 ==> base10 :      8"
# [1] "base[ 2] :   1001 ==> base10 :      9"
# [1] "base[ 5] :     40 ==> base10 :     20"
# [1] "base[ 5] :    344 ==> base10 :     99"
# [1] "base[ 7] :    202 ==> base10 :    100"
# [1] "base[ 7] :    203 ==> base10 :    101"
# [1] "base[10] :    100 ==> base10 :    100"
# [1] "base[10] :    199 ==> base10 :    199"


# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...	end_of_file
# ...	-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

