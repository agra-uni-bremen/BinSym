.globl _start
_start:

/* The following  code represents this program:
 *
 *   if (a0 < a1) {
 *     t0 = 5
 *     if (a1 < t0) {
 *       a3 = 3
 *     } else {
 *       a3 = 2
 *     }
 *   } else {
 *     a3 = 1
 *   }
 *
 * As a0 and a1 are unconstrained symbolic three paths should be found:
 *
 *   (1) (a0 < a1 && a1 < 5)  → a3 = 3
 *   (2) (a0 < a1 && a1 >= 5) → a3 = 2
 *   (3) (a0 >= a1)           → a3 = 1
 */

blt a0, a1, true1       /* a0 < a1 */

/* else: a0 >= a1 */
addi a3, zero, 1        /* a3 = 1 */
jal zero, end

/* then: a0 < a1 */
true1:
    addi t0, zero, 5    /* t0 = 5 */
    blt a1, t0, true2   /* a1 < 5 */

    /* else: a1 >= 5 */
    addi a3, zero, 2    /* a3 = 2 */
    jal zero, end

/* then: a1 < 5 */
true2:
    addi a3, zero, 3    /* a3 = 3 */

end:
.word 0xffff
