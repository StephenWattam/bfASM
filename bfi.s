
# ==================================================================
# BSS Section.  Defines macros and reusable memory 
# ==================================================================
.bss
# ==================================================================
#  Macros 
# ==================================================================
# ------------------------------------------------------------------
# Output n characters from the current data pointer to stdout
# ------------------------------------------------------------------
	.macro stdout_nchar n 
	movl $\n,	%edx				# Load n into edx
	movl %edi,  %ecx				# Load the address to print into ecx
	movl $1,    %ebx				# file handle (stdout)
	movl $4,    %eax				# syscall number (sys_write)
	int $0x80						# call the OS
	.endm


# ==================================================================
# Reusable working memory areas
# ==================================================================



# ==================================================================
# Text Section.  Defines entry point and running code
# ==================================================================
.text					
	.global _start							# Set ELF Entry point (conventional, use ld -e to set)

# Print usage and quit
usage_exit:
	movl $msg_usage, %edi
	stdout_nchar msg_usage_len 
	jmp exit								# and quit. TODO: return code 1?

failopen_exit:
	# Print an error message if we could not open the file.
	movl $msg_failopen, %edi
	stdout_nchar msg_failopen_len
	jmp exit								# and quit. TODO: return code 1?

# Cannot read file (read returns -1)
failread_exit:
	# Show the user an error message and quit
	movl $msg_failread, %edi				# Move the message into the data pointer
	stdout_nchar msg_failread_len			# Output using a macro
	jmp close_and_exit						# and quit

# Could not read instructions due to buffer length
failread_instructions:
	movl $msg_failread_instr, %edi
	stdout_nchar msg_failread_instr_len
	jmp close_and_exit

# Exit due to a tape bounding error during bracket matching
failtape_exit:
	movl $msg_failtape, %edi
	stdout_nchar msg_failtape_len
	jmp exit

# Exit due to being unable to allocate tape memory
failtape_alloc_exit:
	movl $msg_failtape_alloc, %edi
	stdout_nchar msg_failtape_alloc_len
	jmp exit

# ------------------------------------------------------------------
#  Entry Point
# ------------------------------------------------------------------
_start:
# NB:
#  Command line arguments in linux executables are arranged on the stack. 
#   argc comes first, followed by an array of pointers (**argv) 
#   to the strings on the command line followed by a NULL pointer. 
#   Next comes an array of pointers to the environment (**envp). 
checkargs:


# ==================================================================
#  Check CMDLine arguments
# ==================================================================
	# Check ARGC >= 2  and <= 3
	popq		%rcx						# Read ARGC
	cmp		$2, %rcx						# Test for 2
	jl		usage_exit						# Jump if ARGC != 2

	# Check ARGV <= 3
	cmp		$3, %rcx						# Test for 3
	jg		usage_exit						# Jump if ARGC > 3

	# Skip over argv[0]
	addq	$8, %rsp						# Skip argv[0]
	# Arguments are valid here.  

# ------------------------------------------------------------------
#  Set EOF behaviour
# ------------------------------------------------------------------
	# If there are only two arguments, DO NOT read the EOF flags
	cmp		$2, %rcx						# Test for 2
	je		read_file						# Jump if ARGC == 2

	# pop argv[1]
	popq	%rsi							# First argument is pointer to flags
	movb	(%rsi), %dl						# Load first byte of string

	# Test it is valid
	cmpb	$chr_z, %dl
	je		flag_valid
	cmpb	$chr_n, %dl
	je		flag_valid

	# At this point the flag is invalid, so give usage
	jmp		usage_exit

	# The flags conform to USAGE requirements
	flag_valid:

	# Now save the flags so we can access them later
	movb	%dl, (flags)					# And save to memory

# ==================================================================
#  Allocate memory
# ==================================================================
# 432 * Register setup:
# 433 * rax  system call number
# 434 * rdi  arg0
# 435 * rcx  return address for syscall/sysret, C arg3
# 436 * rsi  arg1
# 437 * rdx  arg2
# 438 * r10  arg3    (--> moved to rcx for C)
# 439 * r8   arg4
# 440 * r9   arg5
# 441 * r11  eflags for syscall/sysret, temporary for C
# 442 * r12-r15,rbp,rbx saved by C code, not touched.

#	# 64-bit sys_brk calls
#	# syscall number is 12 for 64-bit.
#	movq	$12, %rax			# Set sys_brk
#	xorq	%rdi, %rdi			# Set break at 0
#	syscall
#
#	addq	$data_tape_len*8, %rax		# Increase break by 30k
#	movq	%rax, %rdi
#	movq	$12, %rax
#	syscall
#
#	# test for error.  
#	cmp		$0, %rax
#	jl		failtape_exit	
#
#
#	# TODO: there is a more efficient way of doing this
#	setup_d_tape:
#		# count backwards and zero tape
#		movq	$0, (%rax)
#		decq	%rax
#
#		# count down until tape start
#		cmp $data_tape, %rax
#		jg	setup_d_tape
#
#  FIXME: TODO: this causes segfaults, so for now static allocation is used.
#               curiously it doesn't segfault in gdb
#               more curiously it makes some answers go slightly wrong.
#				perhaps the alloc

# See http://rudy.mif.pg.gda.pl/~bogdro/linux/alloc_tut_linux_en.html 
# TODO: complete this algorithm!
# 
#  1) read allocated space
#  2) Set to all 0
#  3) Store offset somewhere for later use
#  4) free memory later? is this necessary? 
		
# ==================================================================
#  Open File
# ==================================================================
read_file:
	# pop argv[1] OR. if, three args were given, argv[2]
	popq	%rsi							# First argument is filename

	# 64-bit syscall to open file
	# See http://stackoverflow.com/questions/3730064/process-command-line-in-linux-64-bit
	# and http://lxr.linux.no/linux+v2.6.35/arch/x86/kernel/entry_64.S#L431
	# and http://lxr.linux.no/linux+v2.6.35/arch/x86/include/asm/unistd_64.h
	# and http://syscalls.kernelgrok.com/
	movq	$2, %rax				
	movq	%rsi, %rdi	# arg0 is filename		
	movq	$0, %rsi	# arg1 is flags, 0 for readonly O_RDONLY = 0
	movq	$0, %rdx	# arg2 is mode, 0 for read
	syscall				# 64-bit syscall interface

	# Read file descriptor
	movq	%rax, (file_descriptor)
	
	#Check we have opened the file correctly
	# eax == -1 on failure
	test    %rax, %rax						# compare
	js		failopen_exit					# jump if sign bit set

# ------------------------------------------------------------------
#  Read file char-by-char, ignoring non-bf chars
# ------------------------------------------------------------------
# rsi = instruction pointer
# ------------------------------------------------------------------

	movq	$instruction_tape, %rsi				# Start at the head of the instruction tape

selective_read_loop:
	movq	$3, %rax							# Read
	movq	(file_descriptor), %rbx				# File descriptor into ebx
	movq	%rsi, %rcx							# Address to read into
	movq	$1, %rdx							# Amount to read
	int		$0x80

	# Check we have not made an error
	cmp		$0, %rax
	jl		failread_exit

	# Check we've read a byte.  If not, close file and stop reading.
	test	%rax, %rax
	jz		close_file

	# Check the character is one of the ones we want:  .,<>[]+-
	cmpb	$chr_decdp, (%rsi)
	je		selective_read_valid
	cmpb	$chr_incdp, (%rsi)
	je		selective_read_valid
	cmpb	$chr_incval, (%rsi)
	je		selective_read_valid
	cmpb	$chr_decval, (%rsi)
	je		selective_read_valid
	cmpb	$chr_startloop, (%rsi)
	je		selective_read_valid
	cmpb	$chr_endloop, (%rsi)
	je		selective_read_valid
	cmpb	$chr_input, (%rsi)
	je		selective_read_valid
	cmpb	$chr_output, (%rsi)
	je		selective_read_valid

	# Continue to loop without incrementing
	selective_read_invalid:
		movq	$0, (%rsi)						# Overwrite with a \0 for neatness :-)
		jmp selective_read_continue				# Skip increments, so the next read overwrites this slot

	# Continue the loop but increment the pointer
	selective_read_valid:
		incq	%rsi							# Increment the instruction pointer by one char to read another

		# If we have exceeded the input buffer then the program is
		# too big for the interpreter, so wuss out and quit.
		cmp		$instruction_tape_end, %rsi
		jg		failread_instructions
	
	# Continue the loop and jump back to the start
	selective_read_continue:
		jmp selective_read_loop					# Loop.
		
	# Fallen through? Close file
close_file:
	movl	$6, %eax						
	movl	(file_descriptor), %ebx			# file descriptor
	int		$0x80			


# ==================================================================
#  Interpreter Loop
# ==================================================================
start_intrepreter:
#  rsi = instruction pointer
#  rdi = data pointer
# ==================================================================
	movq	$data_tape, %rdi				# RDI holds Data pointer
	movq	$instruction_tape, %rsi			# RSI holds inStruction pointer

interpret_loop:
# ------------------------------------------------------------------
# Increment Data Value
# ------------------------------------------------------------------
	# Check for '+'
	cmpb	$chr_incval, (%rsi)
	jne		skip_incval

	# Increment value by 1
	addb    $1, (%rdi)				# Increment with wraparound
	jmp		skip_iteration			# Jump to end of if-test

skip_incval:	
# ------------------------------------------------------------------
#  Decrement Data Value
# ------------------------------------------------------------------
	# Check for '-'
	cmpb	$chr_decval, (%rsi)
	jne		skip_decval

	# Decrement value by 1
	subb    $1, (%edi)				# Increment with wraparound
	jmp		skip_iteration			# Jump to end of if-test

skip_decval:	
# ------------------------------------------------------------------
#  PutChar (Output)
# ------------------------------------------------------------------
	# Check for '.'
	cmpb	$chr_output, (%rsi)
	jne		skip_putchar

	# stdout one character from the data tape
	movq $1,	%rdx				# Load n into edx
	movq %rdi,  %rcx				# Load the address to print into ecx
	movq $1,    %rbx				# file handle (stdout)
	movq $4,    %rax				# syscall number (sys_write)
	int $0x80						# call the OS

	jmp		skip_iteration			# Jump to end of if-test

skip_putchar:	
# ------------------------------------------------------------------
#  GetChar (Input)
# ------------------------------------------------------------------
	# Check for ','
	cmpb	$chr_input, (%rsi)
	jne		skip_getchar

	# stdin one char from the data tape.
	movl $1,	%edx				# Load n into edx
	movl %edi,	%ecx				# Load the address to read into
	movl $0,	%ebx				# File handle (stdin)
	movl $3,	%eax				# Syscall number (sys_read)
	int $0x80						# Call the OS

	# Test for EOF, and go through EOF behaviour if so...
	cmpb	$0, (%rdi)
	jne		getc_skipeof

	# Load the EOF settings
	movb	(flags), %dl				# Load the flags

	############
	# Flags == 'z'?
	# Handle Zero-eof behaviour by moving buffer
	# Since eof == 0, this requires no work
	#cmpb	$chr_z, %dl					# Test for Z
	#jne		getc_skipz					# Skip to N test if not

	#movb	$0, (%rdi)
	
	#getc_skipz:
	############

	# flags == 'n'?
	cmpb	$chr_n, %dl					# Test for N
	jne getc_skipeof					# Skip to end if not

	# Handle -1-EOF behaviour
	movb	$-1, (%rdi)
	#jmp getc_skipeof

	# end of all EOF behaviour
	getc_skipeof:
	jmp		skip_iteration			# Jump to end of if-test


skip_getchar:	
# ------------------------------------------------------------------
#  Increment Data Pointer
# ------------------------------------------------------------------
	# Check for '>'
	cmpb	$chr_incdp, (%rsi)
	jne		skip_incdp

	# Increment the data pointer
	incq	%rdi
	
	# TODO: optional roll-around vs error message?
	cmp		$data_tape + data_tape_len, %rdi		# IF dp > dp_end
	jl		incdp_limit
	movq	$data_tape + data_tape_len - 1, %rdi		# THEN dp = dp_start
	incdp_limit:						# ELSE continue...

	jmp		skip_iteration			# Jump to end of if-test

skip_incdp:	
# ------------------------------------------------------------------
#  Decrement Data Pointer
# ------------------------------------------------------------------
	# Check for '<'
	cmpb	$chr_decdp, (%rsi)
	jne		skip_decdp

	# Decrement the data pointer
	decq	%rdi

	# TODO: (DP=0) - 1 = 0 for now. should I do two-way rollaround?
	
	cmp		$data_tape, %rdi			# IF dp < dp_start
	jg		decdp_limit					
	movq	$data_tape, %rdi			# THEN dp == dp_end
	decdp_limit:						# ELSE continue....

	jmp		skip_iteration				# Jump to end of if-test

skip_decdp:	
# ------------------------------------------------------------------
#  Start Loop
# ------------------------------------------------------------------
	# Check for '['
	cmpb	$chr_startloop, (%rsi)
	jne		skip_startloop

	
	cmpb	$0, (%rdi)						# IF (%rdi) == 0 THEN continue...
	jne		skip_iteration				
	
	# Set up count vars
	# TODO: accomplish this using the stack.
	movq	$1, %rdx						# Count nesting depth of [] pairs

	startloop_loop:
		incq	%rsi						# Increment the instruction pointer

		cmp		$instruction_tape_end, %rsi	# Ensure we haven't gone off the end.
		jg		failtape_exit						

		cmpb	$chr_endloop, (%rsi)					# IF instruction[rsi] = ']'	
		jne		startloop_skipdec
		decq	%rdx						# THEN decrement the depth
		startloop_skipdec:					# ELSE continue
		
		cmpb	$chr_startloop, (%rsi)					# IF instruction[rsi] = '['
		jne		startloop_skipinc
		incq	%rdx						# THEN increment the depth
		startloop_skipinc:					# ELSE continue
		
		cmpq	$0, %rdx					# IF depth > 0
		jne startloop_loop					# THEN loop

	jmp		skip_iteration					# Jump to end of if-test

skip_startloop:
# ------------------------------------------------------------------
#  End Loop
# ------------------------------------------------------------------
	# Check for ']'
	cmpb	$chr_endloop, (%rsi)
	jne		skip_endloop


	
	cmpb	$0, (%rdi)						# IF (%rdi) != 0 THEN
	je		skip_iteration

	# Set up count vars
	# TODO: use the stack for this?	
	movq	$1, %rdx						# Count [] pair depth in rdx

	endloop_loop:
		decq	%rsi						# Decrement the instruction pointer

		cmp		$instruction_tape, %rsi		# Check we haven't gone off the start
		jl		failtape_exit

		cmpb	$chr_startloop, (%rsi)					# IF instruction[rsi] = '['	
		jne		endloop_skipdec
		decq	%rdx						# THEN decrement the depth
		endloop_skipdec:					# ELSE
		
		cmpb	$chr_endloop, (%rsi)					# IF instruction[rsi] = ']'
		jne		endloop_skipinc
		incq	%rdx						# THEN increment the depth
		endloop_skipinc:					# ELSE

		cmpq	$0, %rdx					# IF depth > 0
		jne endloop_loop					# THEN loop


	# not necessary, let it fall through
	#jmp		skip_iteration				# Jump to end of if-test

skip_endloop:
skip_iteration:
# ------------------------------------------------------------------
#  Increment Instruction Pointer (automatic)
# ------------------------------------------------------------------
	incq	%rsi							# Increment instruction pointer

	# End condition, instruction pointer > instruction tape length
	cmp		$instruction_tape_end, %rsi
	jl		interpret_loop

# ------------------------------------------------------------------
#  End of Interpreter Loop
# ------------------------------------------------------------------
interpret_end:


	# If we reach the end naturally, skip over the file closing routine!
	jmp exit

# ------------------------------------------------------------------
#  fclose AND exit, used for when a file is in play
# ------------------------------------------------------------------
close_and_exit:
	movl	$6, %eax						
	movl	(file_descriptor), %ebx			# file descriptor
	int		$0x80			
# ------------------------------------------------------------------
#  Plain old Exit.
# ------------------------------------------------------------------
exit:						
	# Exit
	movl	$1, %eax		
	xorl	%ebx, %ebx						#exit code (0)
	int		$0x80


# ==================================================================
# Data Section.  For holding working and named values
# ==================================================================
.data
.set data_tape_len,		30000
.set chr_decdp,			60	# '<'
.set chr_incdp,			62	# '>'
.set chr_incval,		43 # '+'
.set chr_decval,		45 # '-'
.set chr_startloop,		91	# '['
.set chr_endloop,		93 # ']'
.set chr_input,			44 # ','
.set chr_output,		46 # '.'

# EOF flags
.set chr_n,				110 # 'n'
.set chr_z,				122 # 'z'


# Store the input file's descriptor
file_descriptor:
	.quad 0

# Contains the EOF flag, if any are set
flags:
	.word 0

instruction_tape:
	.fill 50096, 1, 0	# buffer size for input commands	
	.set instruction_tape_len, . - instruction_tape
instruction_tape_end:

# A message to tell people to provide a filename
msg_usage:
	.string "USAGE:\n  bfi [EOF] FILE\n\nEOF can be:\n  z : Set EOF == 0.\n  n : Set EOF == -1.\n\n"
	.set msg_usage_len, . - msg_usage

# A message for when a file cannot be read
msg_failread:
	.string "Could not read file.\n"
	.set msg_failread_len, . - msg_failread

# A message for when we cannot even open the file handle
msg_failopen:
	.string "Could not open file.\n"
	.set msg_failopen_len, . - msg_failopen

# A message for when we run off the end of a loop whilst bracket-matching
msg_failtape:
	.string "Mismatched loops.\n"
	.set msg_failtape_len, . - msg_failtape

# A message for when we cannot fit the instruction input into the input buffer.
msg_failread_instr:
	.string "Cannot fit that input in the input buffer.\nI can load at most 50096 brainfuck instructions.\n"
	.set msg_failread_instr_len, . - msg_failread_instr

msg_failtape_alloc:
	.string "Cannot allocate data tape.  How do you not have 30k of ram?.\n"
	.set msg_failtape_alloc_len, . - msg_failtape_alloc

# The data tape for bf
# TODO: switch back to dyanmic allocation
data_tape:
	.fill 30000, 1, 0  # 30,000 cells at 1 word and set to 0
