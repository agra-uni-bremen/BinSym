int first_divisor(unsigned int a) {
	unsigned int i;

	for (i = 2; i < a; i++) {
		if (a % i == 0) {
			return i;
		}
	}

	return a;
}

int main(void) {
	register int a asm("a0");
	if (a <= 10) {
		if (a > 1 && first_divisor(a) == a) {
			return 1;
		} else {
			return 0;
		}
	}
}
