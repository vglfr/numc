	.text
	.file	"test1.ll"
	.globl	div1                            # -- Begin function div1
	.p2align	4, 0x90
	.type	div1,@function
div1:                                   # @div1
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	mul1@PLT
                                        # kill: def $eax killed $eax def $rax
	leal	3(%rax), %ecx
	testl	%eax, %eax
	cmovsl	%ecx, %eax
	sarl	$2, %eax
                                        # kill: def $eax killed $eax killed $rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	div1, .Lfunc_end0-div1
	.cfi_endproc
                                        # -- End function
	.globl	mul1                            # -- Begin function mul1
	.p2align	4, 0x90
	.type	mul1,@function
mul1:                                   # @mul1
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	add1@PLT
	addl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	mul1, .Lfunc_end1-mul1
	.cfi_endproc
                                        # -- End function
	.globl	add1                            # -- Begin function add1
	.p2align	4, 0x90
	.type	add1,@function
add1:                                   # @add1
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	sub1@PLT
	addl	$6, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	add1, .Lfunc_end2-add1
	.cfi_endproc
                                        # -- End function
	.globl	sub1                            # -- Begin function sub1
	.p2align	4, 0x90
	.type	sub1,@function
sub1:                                   # @sub1
	.cfi_startproc
# %bb.0:
	movl	$-2, %eax
	retq
.Lfunc_end3:
	.size	sub1, .Lfunc_end3-sub1
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
