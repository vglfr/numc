	.text
	.file	"test2.ll"
	.globl	expr                            # -- Begin function expr
	.p2align	4, 0x90
	.type	expr,@function
expr:                                   # @expr
	.cfi_startproc
# %bb.0:
	movl	$2, %eax
	retq
.Lfunc_end0:
	.size	expr, .Lfunc_end0-expr
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
