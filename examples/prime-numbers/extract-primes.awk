#!/usr/bin/awk -f

BEGIN {
	is_prime = 0
}

/^A2	= 0x1/ {
	is_prime = 1
}

/^Next/ {
	is_prime = 0
}

/^A3	=/ {
	if (is_prime)
		print($3)
}
