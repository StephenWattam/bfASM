# Linker
LD=ld
#LDFLAGS=-s -e $(ENTRY_POINT) 
LDFLAGS=-e $(ENTRY_POINT) 
ENTRY_POINT=_start # ld -e $(ENTRY_POINT)

# Assembler flags
GAS=as
#GASFLAGS= --64 -mtune=i686 -o $(OBJ)
GASFLAGS= --64 -o $(OBJ) -g

# Output name
OBJ=bfi.o
NAME=bfi


all:
		$(GAS) $(GASFLAGS) bfi.s
		$(LD) $(LDFLAGS) $(OBJ) -o $(NAME)

test: all
		@echo "-----------"
		./bfi

clean:
		rm $(OBJ) $(NAME)
