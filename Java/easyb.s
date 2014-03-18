
 # easyb.s engendre par ml2mips 




  .data 


a___3:
      .word 1

b___5:
      .word 2

c___7:
      .word 3

r___11:
      .asciiz 
 
##### nom fonction : id___1
id___1 : 
## SAVE PILE
addiu $29 $29 -16
sw $31, 0$(29)
sw $8, 4$(29)
move $8, $4
sw $26, 8$(29)
sw $27, 12$(29)
## INSTRUCTION 


lw $2, i___2($29) 

## RESTORATION PILE
lw $31, 0$(29)
lw $4, 4$(29)
lw $26, 8$(29)
lw $27, 12$(29)
addiu $29 $29 16


main:
 # Fait l'appelle de fonction ici si il y en a une qui a ete cree ( metre une variable a true lorque l'on créé une fonction , stoqué son nom , ces argument dans une liste , cree une fonction let qui prends le nom et les arguments en paramettre , remettre la variable a false ici quand on l'a créé ) (pour la fonction https://wiki.engr.illinois.edu/download/attachments/217841775/L17-before.pdf?version=1&modificationDate=1360777320000
 # Fait l'appelle de fonction ici si il y en a une qui a ete cree ( metre une variable a true lorque l'on créé une fonction , stoqué son nom , ces argument dans une liste , cree une fonction let qui prends le nom et les arguments en paramettre , remettre la variable a false ici quand on l'a créé ) (pour la fonction https://wiki.engr.illinois.edu/download/attachments/217841775/L17-before.pdf?version=1&modificationDate=1360777320000
