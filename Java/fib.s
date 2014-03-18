
 # fib.s engendre par ml2mips 




.data 

value___5: MLvalue 1
 

.text
main:
## Transmission des argument et sauvgarde
addiu $29 $29 -4
sw $4, 0($29)
li $4, 1
## Appelle a la fonction
jal fun___1
## Recupereation des anciens registres et dépile
lw $4, 0($29)
addiu $29 $29 4
syscall



##### nom fonction : fun___1
fun___1 : 
## SAVE PILE
addiu $29 $29 -16
sw $31, 0($29)
sw $8, 4($29)
move $8, $4
sw $26, 8($29)
sw $27, 12($29)
## INSTRUCTION 


lw $26, x___2($29) 

## RESTORATION PILE
lw $31, 0($29)
lw $4, 4($29)
lw $26, 8($29)
lw $27, 12($29)
addiu $29 $29 16
addi $v0 $2 0 
jr $ra 

# fin du fichier fib.s
