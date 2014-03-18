
 # easy.s engendre par ml2mips 




  .data 


a___1:
      .word 11

c___3:
      .float 12.55

b___5:
      .word 12

value___11:
      .word 5
 
test___7:
# this need to be calculated
 addiu $29 $29 0
 #introduction stuff 


 lw $2, x___8($29) 
 ori $3, $0, 1
 # going to do sub 
 lw $4 T___10($29)
 sub $4 $2 $3



main:
 addiu $29 $29 4

# fin du fichier easy.s
