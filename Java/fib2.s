
 # fib2.s engendre par ml2mips 




.data 

b___1: .word 52
value___22: .word 5
 

.text
main:
## Transmission des argument et sauvgarde
addiu $29 $29 -4
sw $4, 0($29)
li $4, 5
## Appelle a la fonction
jal fib___5
## Recupereation des anciens registres et dépile
lw $4, 0($29)
addiu $29 $29 4
syscall



##### nom fonction : fib___5
fib___5 : 
## SAVE PILE
addiu $29 $29 -16
sw $31, 0($29)
sw $8, 4($29)
move $8, $4
sw $26, 8($29)
sw $27, 12($29)
## INSTRUCTION 


lw $26, x___6($29) 
ori $3, $0, 2
 lw $2 $t1($29)
slt $2, $26, $27
 sw $2 $t1($29)
lw $26, $t7($29) 
beq $2,$0,Else0
ori $3, $0, 1
lw $26, $t2($29) 
j Endif0
Else0:
## Transmission des argument et sauvgarde
addiu $29 $29 -8
sw $4, 0($29)
lw $4, x___6
sw $5, 4($29)
li $5, 1
## Appelle a la fonction
jal fib___5
## Recupereation des anciens registres et dépile
lw $4, 0($29)
lw $5, 4($29)
addiu $29 $29 8
## Transmission des argument et sauvgarde
addiu $29 $29 -8
sw $4, 0($29)
lw $4, x___6
sw $5, 4($29)
li $5, 2
## Appelle a la fonction
jal fib___5
## Recupereation des anciens registres et dépile
lw $4, 0($29)
lw $5, 4($29)
addiu $29 $29 8
Endif0:

## RESTORATION PILE
lw $31, 0($29)
lw $4, 4($29)
lw $26, 8($29)
lw $27, 12($29)
addiu $29 $29 16
addi $v0 $2 0 
jr $ra 

# fin du fichier fib2.s
