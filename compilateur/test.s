	.text
main:
	move	$s0, $ra
	sub	$sp, $sp, 4
	add	$fp, $sp, 0
	lw	$a0, _f
	la	$t1, _1clos
	sw	$a0, 4($t1)
	la	$a0, _1clos
	sw	$a0, _f
	sw	$a0, 4($a0)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _f
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 8
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sw	$a0, _d
	la	$a0, Lasciiz0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _d
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _fib
	la	$t1, _2clos
	sw	$a0, 4($t1)
	la	$a0, _2clos
	sw	$a0, _fib
	sw	$a0, 4($a0)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _fib
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 8
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sw	$a0, _d
	la	$a0, Lasciiz1
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _d
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	la	$a0, Lasciiz2
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	read_line
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	sw	$a0, _str
	lw	$a0, _str
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	la	$a0, _0closprint_int
	sw	$a0, _ga
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _ga
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 999999999
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	la	$a0, _0closprint_newline
	sw	$a0, _ixce
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _ixce
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	li	$a0, 5
	la	$t1, Lword1
	sw	$a0, 0($t1)
	li	$a0, 8
	la	$t1, Lword2
	sw	$a0, 0($t1)
	li	$a0, 6
	la	$t1, Lword3
	sw	$a0, 0($t1)
	li	$a0, 5
	la	$t1, Lword4
	sw	$a0, 0($t1)
	li	$a0, 0
	la	$t1, Lword4
	sw	$a0, 4($t1)
	la	$a0, Lword4
	la	$t1, Lword3
	sw	$a0, 4($t1)
	la	$a0, Lword3
	la	$t1, Lword2
	sw	$a0, 4($t1)
	la	$a0, Lword2
	la	$t1, Lword1
	sw	$a0, 4($t1)
	la	$a0, Lword1
	sw	$a0, _f
	li	$a0, 42
	la	$t1, Lword5
	sw	$a0, 0($t1)
	lw	$a0, _f
	la	$t1, Lword5
	sw	$a0, 4($t1)
	la	$a0, Lword5
	sw	$a0, _g
	lw	$a0, _bob
	la	$t1, _3clos
	sw	$a0, 4($t1)
	la	$a0, _3clos
	sw	$a0, _bob
	sw	$a0, 4($a0)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _bob
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, _g
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	la	$a0, _6clos
	sw	$a0, _test2
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _test2
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 5
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 8
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _test2
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 7
	neg	$a0, $a0
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	add	$a0, $a1, $a0
	sw	$a0, _test3
	lw	$a0, _test3
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 4
	la	$t1, Lword6
	sw	$a0, 0($t1)
	la	$a0, Lasciiz4
	la	$t1, Lword7
	sw	$a0, 0($t1)
	li	$a0, 5
	la	$t1, Lword7
	sw	$a0, 4($t1)
	li	$a0, 8
	la	$t1, Lword8
	sw	$a0, 0($t1)
	li	$a0, 9
	la	$t1, Lword8
	sw	$a0, 4($t1)
	li	$a0, 7
	la	$t1, Lword9
	sw	$a0, 0($t1)
	li	$a0, 0
	la	$t1, Lword9
	sw	$a0, 4($t1)
	la	$a0, Lword9
	la	$t1, Lword8
	sw	$a0, 8($t1)
	li	$a0, 7
	la	$t1, Lword8
	sw	$a0, 12($t1)
	la	$a0, Lword8
	la	$t1, Lword7
	sw	$a0, 8($t1)
	la	$a0, Lasciiz3
	la	$t1, Lword7
	sw	$a0, 12($t1)
	li	$a0, 0
	la	$t1, Lword7
	sw	$a0, 16($t1)
	la	$a0, Lword7
	la	$t1, Lword6
	sw	$a0, 4($t1)
	li	$a0, 0
	la	$t1, Lword6
	sw	$a0, 8($t1)
	la	$a0, Lword6
	sw	$a0, _test4
	lw	$a0, _test4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	sw	$a0, _a1
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	sw	$a0, _a2
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, _a3
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($a0)
	sw	$a0, _a4
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 12($a0)
	sw	$a0, _a5
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 16($a0)
	sw	$a0, _a6
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($a0)
	sw	$a0, _a7
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _a4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	sw	$a0, _b1
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, _b2
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($a0)
	sw	$a0, _b3
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 12($a0)
	sw	$a0, _b4
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _b4
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, _a2
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 4
	sw	$a0, 0($fp)
	lw	$a0, 0($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 5
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	add	$a0, $a1, $a0
	sw	$a0, 0($fp)
	lw	$a0, 0($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 2
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	mul	$a0, $a1, $a0
	sw	$a0, _f
	lw	$a0, _f
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	la	$a0, _7clos
	sw	$a0, _x
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, _x
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 5
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	la	$a0, Lasciiz5
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_string
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	read_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	sw	$a0, _d
	lw	$a0, _d
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_int
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	li	$a0, 0
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	jal	print_newline
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	add	$sp, $sp, 4
	move	$ra, $s0
	jr	$ra
_1fun:
	sub	$sp, $sp, 12
	sw	$fp, 8($sp)
	sw	$ra, 4($sp)
	add	$fp, $sp, 4
	lw	$a0, 12($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, -4($fp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 0
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	seq	$a0, $a1, $a0
	beq	$a0, 0, Lif2
	li	$a0, 26
	j	Lif1
Lif2:
	li	$a0, 2
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, -4($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 1
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	sub	$a0, $a1, $a0
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	add	$a0, $a1, $a0
Lif1:
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 12
	jr	$ra
_2fun:
	sub	$sp, $sp, 12
	sw	$fp, 8($sp)
	sw	$ra, 4($sp)
	add	$fp, $sp, 4
	lw	$a0, 12($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, -4($fp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 0
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	seq	$a0, $a1, $a0
	beq	$a0, 0, Lif6
	li	$a0, 0
	j	Lif5
Lif6:
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 1
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	seq	$a0, $a1, $a0
	beq	$a0, 0, Lif4
	li	$a0, 1
	j	Lif3
Lif4:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, -4($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 2
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	sub	$a0, $a1, $a0
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, -4($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 1
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	sub	$a0, $a1, $a0
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	add	$a0, $a1, $a0
Lif3:
Lif5:
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 12
	jr	$ra
_3fun:
	sub	$sp, $sp, 20
	sw	$fp, 16($sp)
	sw	$ra, 12($sp)
	add	$fp, $sp, 12
	lw	$a0, 12($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, -4($fp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, 8($fp)
	beq	$a0, 0, Lif8
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 0($a0)
	sw	$a0, -12($fp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, 4($a0)
	sw	$a0, -8($fp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 4
	lw	$a0, -12($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	lw	$a0, -4($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$a0, -8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	lw	$t1, 4($sp)
	lw	$t1, 0($t1)
	jalr	$t1
	lw	$ra, 0($sp)
	add	$sp, $sp, 12
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	add	$a0, $a1, $a0
	j	Lif7
Lif8:
	li	$a0, 0
Lif7:
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 20
	jr	$ra
_4fun:
	sub	$sp, $sp, 8
	sw	$fp, 4($sp)
	sw	$ra, 0($sp)
	add	$fp, $sp, 0
	lw	$a0, 12($fp)
	lw	$a0, 8($fp)
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 8
	jr	$ra
_5fun:
	sub	$sp, $sp, 8
	sw	$fp, 4($sp)
	sw	$ra, 0($sp)
	add	$fp, $sp, 0
	lw	$a0, 12($fp)
	lw	$a0, 8($fp)
	neg	$a0, $a0
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 8
	jr	$ra
_6fun:
	sub	$sp, $sp, 8
	sw	$fp, 4($sp)
	sw	$ra, 0($sp)
	add	$fp, $sp, 0
	lw	$a0, 12($fp)
	lw	$a0, 8($fp)
	sub	$sp, $sp, 4
	sw	$a0, 0($sp)
	li	$a0, 0
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	slt	$a0, $a1, $a0
	beq	$a0, 0, Lif10
	la	$a0, _5clos
	j	Lif9
Lif10:
	la	$a0, _4clos
Lif9:
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 8
	jr	$ra
_7fun:
	sub	$sp, $sp, 8
	sw	$fp, 4($sp)
	sw	$ra, 0($sp)
	add	$fp, $sp, 0
	lw	$a0, 12($fp)
	lw	$a0, 8($fp)
	lw	$ra, 0($fp)
	lw	$fp, 4($fp)
	add	$sp, $sp, 8
	jr	$ra
print_int:
	li	$v0, 1
	syscall
	jr	$ra
print_string:
	li	$v0, 4
	syscall
	jr	$ra
print_newline:
	sub	$sp, $sp, 8
	sw	$ra, 4($sp)
	sw	$a0, 0($sp)
	la	$a0, newline
	jal	print_string
	lw	$ra, 4($sp)
	lw	$a0, 0($sp)
	add	$sp, $sp, 8
	jr	$ra
read_int:
	li	$v0, 5
	syscall
	move	$a0, $v0
	jr	$ra
read_line:
	sub	$sp, $sp, 4
	sw	$a1, 0($sp)
	la	$a0, buffer
	li	$a1, 1000
	li	$v0, 8
	syscall
	lw	$a1, 0($sp)
	add	$sp, $sp, 4
	jr	$ra
	.data
buffer:	.space 1000
newline:	.asciiz "\n"
Lasciiz0:	.asciiz "f(8) = "
Lasciiz1:	.asciiz "fib(8) = "
Lasciiz2:	.asciiz "écrire qqch:\n"
Lasciiz3:	.asciiz "p"
Lasciiz4:	.asciiz "k"
Lasciiz5:	.asciiz "donner un nombre:\n"
_d:	.word 1
_bob:	.word 1
_ga:	.word 1
_a7:	.word 1
_a6:	.word 1
_b4:	.word 1
_a5:	.word 1
_b3:	.word 1
_test4:	.word 1
_a4:	.word 1
_b2:	.word 1
_str:	.word 1
_test3:	.word 1
_a3:	.word 1
_b1:	.word 1
_test2:	.word 1
_a2:	.word 1
_a1:	.word 1
_ixce:	.word 1
_fib:	.word 1
_g:	.word 1
_x:	.word 1
_f:	.word 1
Lword9:	.word 1 1
Lword8:	.word 1 1 1 1
Lword7:	.word 1 1 1 1 1
Lword6:	.word 1 1 1
Lword5:	.word 1 1
Lword4:	.word 1 1
Lword3:	.word 1 1
Lword2:	.word 1 1
Lword1:	.word 1 1
_7clos:	.word _7fun
_6clos:	.word _6fun
_3clos:	.word _3fun 1
_2clos:	.word _2fun 1
_1clos:	.word _1fun 1
_4clos:	.word _4fun
_5clos:	.word _5fun
_0closprint_int:	.word print_int
_0closprint_string:	.word print_string
_0closprint_newline:	.word print_newline
_0closread_int:	.word read_int
_0closread_line:	.word read_line
