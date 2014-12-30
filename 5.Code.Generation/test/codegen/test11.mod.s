##################################################
# test11
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

    # scope primefactor
primefactor:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $20, %esp               # make room for locals

    movl    8(%ebp), %eax           #   0:     if     n < 1 goto 1_if_true
    cmpl    $1, %eax               
    jl      l_primefactor_1_if_true
    jmp     l_primefactor_2_if_false #   1:     goto   2_if_false
l_primefactor_1_if_true:
    jmp     l_primefactor_exit     
    jmp     l_primefactor_0         #   4:     goto   0
l_primefactor_2_if_false:
    movl    8(%ebp), %eax           #   6:     if     n = 1 goto 5_if_true
    cmpl    $1, %eax               
    je      l_primefactor_5_if_true
    jmp     l_primefactor_6_if_false #   7:     goto   6_if_false
l_primefactor_5_if_true:
    movl    $1, %eax                #   9:     param  0 <- 1
    pushl   %eax                   
    call    Output                  #  10:     call   Output
    addl    $4, %esp               
    jmp     l_primefactor_4         #  11:     goto   4
l_primefactor_6_if_false:
l_primefactor_9_while_cond:
    movl    8(%ebp), %eax           #  14:     if     n > 1 goto 10_while_body
    cmpl    $1, %eax               
    jg      l_primefactor_10_while_body
    jmp     l_primefactor_8         #  15:     goto   8
l_primefactor_10_while_body:
    movl    $2, %eax                #  17:     assign f <- 2
    movl    %eax, -16(%ebp)        
l_primefactor_13_while_cond:
    movl    -16(%ebp), %eax         #  19:     if     f <= n goto 14_while_body
    cmpl    8(%ebp), %eax          
    jle     l_primefactor_14_while_body
    jmp     l_primefactor_12        #  20:     goto   12
l_primefactor_14_while_body:
    movl    8(%ebp), %eax           #  22:     div    t0 <- n, f
    cdq                            
    movl    -16(%ebp), %ebx        
    idivl   %ebx                   
    movl    %eax, -20(%ebp)        
    movl    -20(%ebp), %eax         #  23:     mul    t1 <- t0, f
    imull   -16(%ebp)              
    movl    %eax, -24(%ebp)        
    movl    -24(%ebp), %eax         #  24:     if     t1 = n goto 16_if_true
    cmpl    8(%ebp), %eax          
    je      l_primefactor_16_if_true
    jmp     l_primefactor_17_if_false #  25:     goto   17_if_false
l_primefactor_16_if_true:
    movl    -16(%ebp), %eax         #  27:     param  0 <- f
    pushl   %eax                   
    call    Output                  #  28:     call   Output
    addl    $4, %esp               
    movl    8(%ebp), %eax           #  29:     div    t2 <- n, f
    cdq                            
    movl    -16(%ebp), %ebx        
    idivl   %ebx                   
    movl    %eax, -28(%ebp)        
    movl    -28(%ebp), %eax         #  30:     assign n <- t2
    movl    %eax, 8(%ebp)          
    jmp     l_primefactor_15        #  31:     goto   15
l_primefactor_17_if_false:
    movl    -16(%ebp), %eax         #  33:     add    t3 <- f, 1
    addl    $1, %eax               
    movl    %eax, -32(%ebp)        
    movl    -32(%ebp), %eax         #  34:     assign f <- t3
    movl    %eax, -16(%ebp)        
l_primefactor_15:
    jmp     l_primefactor_13_while_cond #  36:     goto   13_while_cond
l_primefactor_12:
    jmp     l_primefactor_9_while_cond #  38:     goto   9_while_cond
l_primefactor_8:
l_primefactor_4:
l_primefactor_0:

l_primefactor_exit:
    addl    $20, %esp               # remove locals
    popl    %edi                   
    popl    %esi                   
    popl    %ebx                   
    popl    %ebp                   
    ret                            

    # scope test11
main:
    pushl   %ebp                   
    movl    %esp, %ebp             
    pushl   %ebx                   
    pushl   %esi                   
    pushl   %edi                   
    subl    $4, %esp                # make room for locals

    call    Input                   #   0:     call   t0 <- Input
    movl    %eax, -16(%ebp)        
    movl    -16(%ebp), %eax         #   1:     param  0 <- t0
    pushl   %eax                   
    call    primefactor             #   2:     call   primefactor
    addl    $4, %esp               

l_test11_exit:
    addl    $4, %esp                # remove locals
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

    # scope: test11

    # end of global data section
    #-----------------------------------------

    .end
##################################################
