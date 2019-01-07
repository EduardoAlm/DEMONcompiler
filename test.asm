.text
main:
	li $v0, 1
	li $t0, 0
	move $t1, $t0
	li $t0, 1
	move $t2, $t0
	and $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 1
	li $t0, 1
	move $t1, $t0
	li $t0, 1
	move $t2, $t0
	and $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 1
	li $t0, 0
	move $t1, $t0
	li $t0, 0
	move $t2, $t0
	and $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 1
	li $t0, 0
	move $t1, $t0
	li $t0, 1
	move $t2, $t0
	or $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 1
	li $t0, 1
	move $t1, $t0
	li $t0, 1
	move $t2, $t0
	or $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
	li $v0, 1
	li $t0, 0
	move $t1, $t0
	li $t0, 0
	move $t2, $t0
	or $t0, $t1, $t2
	move $a0, $t0
	syscall
	li $v0, 4
	la $a0, newline
	syscall
.data
newline:
	.asciiz "\n"
