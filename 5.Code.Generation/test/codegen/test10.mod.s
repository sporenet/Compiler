##################################################
# test10
#

    #-----------------------------------------
    # text section
    #
    .text
    .align 4

    # entry point and pre-defined functions
    .global main
    .extern Input
    .extern Output

    # scope gcd_iter
gcd_iter:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $8, %esp                # make room for locals

l_gcd_iter_1_while_cond:
    movl    8(%ebp), %eax           #   1:     if     a # b goto 2_while_body
    cmpl    12(%ebp), %eax         
    jne     l_gcd_iter_2_while_body
    jmp     l_gcd_iter_0            #   2:     goto   0
l_gcd_iter_2_while_body:
    movl    8(%ebp), %eax           #   4:     if     a > b goto 4_if_true
    cmpl    12(%ebp), %eax         
    jg      l_gcd_iter_4_if_true   
    jmp     l_gcd_iter_5_if_false   #   5:     goto   5_if_false
l_gcd_iter_4_if_true:
    movl    8(%ebp), %eax           #   7:     sub    t0 <- a, b
    subl    12(%ebp), %eax         
    movl    %eax, -16(%ebp)        
    movl    -16(%ebp), %eax         #   8:     assign a <- t0
    movl    %eax, 8(%ebp)          
    jmp     l_gcd_iter_3            #   9:     goto   3
l_gcd_iter_5_if_false:
    movl    12(%ebp), %eax          #  11:     sub    t1 <- b, a
    subl    8(%ebp), %eax          
    movl    %eax, -20(%ebp)        
    movl    -20(%ebp), %eax         #  12:     assign b <- t1
    movl    %eax, 12(%ebp)         
l_gcd_iter_3:
    jmp     l_gcd_iter_1_while_cond #  14:     goto   1_while_cond
l_gcd_iter_0:
    movl    8(%ebp), %eax           #  16:     return a
    jmp     l_gcd_iter_exit        

l_gcd_iter_exit:
    addl    $8, %esp                # remove locals
    popl    %edi                   
    popl    %esi                   
    popl    %ebx                   
    popl    %ebp                   
    ret                            

    # scope gcd_mod
gcd_mod:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $16, %esp               # make room for locals

l_gcd_mod_1_while_cond:
    movl    12(%ebp), %eax          #   1:     if     b # 0 goto 2_while_body
    cmpl    $0, %eax               
    jne     l_gcd_mod_2_while_body 
    jmp     l_gcd_mod_0             #   2:     goto   0
l_gcd_mod_2_while_body:
    movl    12(%ebp), %eax          #   4:     assign t <- b
    movl    %eax, -16(%ebp)        
    movl    8(%ebp), %eax           #   5:     div    t0 <- a, t
    cdq                            
    movl    -16(%ebp), %ebx        
    idivl   %ebx                   
    movl    %eax, -20(%ebp)        
    movl    -20(%ebp), %eax         #   6:     mul    t1 <- t0, t
    imull   -16(%ebp)              
    movl    %eax, -24(%ebp)        
    movl    8(%ebp), %eax           #   7:     sub    t2 <- a, t1
    subl    -24(%ebp), %eax        
    movl    %eax, -28(%ebp)        
    movl    -28(%ebp), %eax         #   8:     assign b <- t2
    movl    %eax, 12(%ebp)         
    movl    -16(%ebp), %eax         #   9:     assign a <- t
    movl    %eax, 8(%ebp)          
    jmp     l_gcd_mod_1_while_cond  #  10:     goto   1_while_cond
l_gcd_mod_0:
    movl    8(%ebp), %eax           #  12:     return a
    jmp     l_gcd_mod_exit         

l_gcd_mod_exit:
    addl    $16, %esp               # remove locals
    popl    %edi                   
    popl    %esi                   
    popl    %ebx                   
    popl    %ebp                   
    ret                            

    # scope gcd_rec
gcd_rec:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $16, %esp               # make room for locals

    movl    12(%ebp), %eax          #   0:     if     b = 0 goto 1_if_true
    cmpl    $0, %eax               
    je      l_gcd_rec_1_if_true    
    jmp     l_gcd_rec_2_if_false    #   1:     goto   2_if_false
l_gcd_rec_1_if_true:
    movl    8(%ebp), %eax           #   3:     return a
    jmp     l_gcd_rec_exit         
    jmp     l_gcd_rec_0             #   4:     goto   0
l_gcd_rec_2_if_false:
    movl    8(%ebp), %eax           #   6:     div    t0 <- a, b
    cdq                            
    movl    12(%ebp), %ebx         
    idivl   %ebx                   
    movl    %eax, -16(%ebp)        
    movl    -16(%ebp), %eax         #   7:     mul    t1 <- t0, b
    imull   12(%ebp)               
    movl    %eax, -20(%ebp)        
    movl    8(%ebp), %eax           #   8:     sub    t2 <- a, t1
    subl    -20(%ebp), %eax        
    movl    %eax, -24(%ebp)        
    movl    -24(%ebp), %eax         #   9:     param  1 <- t2
    pushl   %eax                   
    movl    12(%ebp), %eax          #  10:     param  0 <- b
    pushl   %eax                   
    call    gcd_rec                 #  11:     call   t3 <- gcd_rec
    addl    $8, %esp               
    movl    %eax, -28(%ebp)        
    movl    -28(%ebp), %eax         #  12:     return t3
    jmp     l_gcd_rec_exit         
l_gcd_rec_0:

l_gcd_rec_exit:
    addl    $16, %esp               # remove locals
    popl    %edi                   
    popl    %esi                   
    popl    %ebx                   
    popl    %ebp                   
    ret                            

    # scope test10
main:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $20, %esp               # make room for locals

    call    Input                   #   0:     call   t0 <- Input
    movl    %eax, -16(%ebp)        
    movl    -16(%ebp), %eax         #   1:     assign a <- t0
    movl    %eax, a                
    call    Input                   #   2:     call   t1 <- Input
    movl    %eax, -20(%ebp)        
    movl    -20(%ebp), %eax         #   3:     assign b <- t1
    movl    %eax, b                
    movl    b, %eax                 #   4:     param  1 <- b
    pushl   %eax                   
    movl    a, %eax                 #   5:     param  0 <- a
    pushl   %eax                   
    call    gcd_iter                #   6:     call   t2 <- gcd_iter
    addl    $8, %esp               
    movl    %eax, -24(%ebp)        
    movl    -24(%ebp), %eax         #   7:     param  0 <- t2
    pushl   %eax                   
    call    Output                  #   8:     call   Output
    addl    $4, %esp               
    movl    b, %eax                 #   9:     param  1 <- b
    pushl   %eax                   
    movl    a, %eax                 #  10:     param  0 <- a
    pushl   %eax                   
    call    gcd_mod                 #  11:     call   t3 <- gcd_mod
    addl    $8, %esp               
    movl    %eax, -28(%ebp)        
    movl    -28(%ebp), %eax         #  12:     param  0 <- t3
    pushl   %eax                   
    call    Output                  #  13:     call   Output
    addl    $4, %esp               
    movl    b, %eax                 #  14:     param  1 <- b
    pushl   %eax                   
    movl    a, %eax                 #  15:     param  0 <- a
    pushl   %eax                   
    call    gcd_rec                 #  16:     call   t4 <- gcd_rec
    addl    $8, %esp               
    movl    %eax, -32(%ebp)        
    movl    -32(%ebp), %eax         #  17:     param  0 <- t4
    pushl   %eax                   
    call    Output                  #  18:     call   Output
    addl    $4, %esp               

l_test10_exit:
    addl    $20, %esp               # remove locals
    popl    %edi                   
    popl    %esi                   
    popl    %ebx                   
    popl    %ebp                   
    ret                            

    # end of text section
    #-----------------------------------------

    #-----------------------------------------
    # global data section
    #
    .data
    .align 4

    # scope: test10
a:      .skip    4                  # <int>
b:      .skip    4                  # <int>

    # end of global data section
    #-----------------------------------------

    .end
##################################################
