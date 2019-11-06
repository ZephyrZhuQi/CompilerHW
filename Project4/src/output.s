.data
_g_n: .word 0
.text
.text
_start_fact:
str x30, [sp, #0]
str x29, [sp, #-8]
add x29, sp, #-8
add sp, sp, #-16
ldr x30, =_frameSize_fact
ldr w30, [x30, #0]
sub sp, sp, w30
str x9, [sp, #8]
str x10, [sp, #16]
str x11, [sp, #24]
str x12, [sp, #32]
str x13, [sp, #40]
str x14, [sp, #48]
str x15, [sp, #56]
str x19, [sp, #64]
str x20, [sp, #72]
str x21, [sp, #80]
str x22, [sp, #88]
str x23, [sp, #96]
str x24, [sp, #104]
str x25, [sp, #112]
str x26, [sp, #120]
str x27, [sp, #128]
str x28, [sp, #136]
str x29, [sp, #144]
str s16, [sp, #152]
str s17, [sp, #156]
str s18, [sp, #160]
str s19, [sp, #164]
str s20, [sp, #168]
str s21, [sp, #172]
str s22, [sp, #176]
str s23, [sp, #180]
ldr x14, =_g_n
ldr w9, [x14,#0]
.data
_CONSTANT_0: .word 1
.align 3
.text
ldr w10, _CONSTANT_0
cmp w9, w10
cset w9, eq
cmp w9, #0
beq _elseLabel_0
ldr x14, =_g_n
ldr w9, [x14,#0]
mov w0, w9
b _end_fact
b _ifExitLabel_0
_elseLabel_0:
ldr x14, =_g_n
ldr w9, [x14,#0]
.data
_CONSTANT_1: .word 1
.align 3
.text
ldr w10, _CONSTANT_1
sub w9, w9, w10
ldr x10, =_g_n
str w9, [x10, #0]
ldr x14, =_g_n
ldr w9, [x14,#0]
bl _start_fact
mov w10, w0
mul w9, w9, w10
mov w0, w9
b _end_fact
_ifExitLabel_0:
_end_fact:
ldr x9, [sp, #8]
ldr x10, [sp, #16]
ldr x11, [sp, #24]
ldr x12, [sp, #32]
ldr x13, [sp, #40]
ldr x14, [sp, #48]
ldr x15, [sp, #56]
ldr x19, [sp, #64]
ldr x20, [sp, #72]
ldr x21, [sp, #80]
ldr x22, [sp, #88]
ldr x23, [sp, #96]
ldr x24, [sp, #104]
ldr x25, [sp, #112]
ldr x26, [sp, #120]
ldr x27, [sp, #128]
ldr x28, [sp, #136]
ldr x29, [sp, #144]
ldr s16, [sp, #152]
ldr s17, [sp, #156]
ldr s18, [sp, #160]
ldr s19, [sp, #164]
ldr s20, [sp, #168]
ldr s21, [sp, #172]
ldr s22, [sp, #176]
ldr s23, [sp, #180]
ldr x30, [x29, #8]
mov sp, x29
add sp, sp, #8
ldr x29, [x29,#0]
RET x30
.data
_frameSize_fact: .word 184
.text
_start_MAIN:
str x30, [sp, #0]
str x29, [sp, #-8]
add x29, sp, #-8
add sp, sp, #-16
ldr x30, =_frameSize_MAIN
ldr w30, [x30, #0]
sub sp, sp, w30
str x9, [sp, #8]
str x10, [sp, #16]
str x11, [sp, #24]
str x12, [sp, #32]
str x13, [sp, #40]
str x14, [sp, #48]
str x15, [sp, #56]
str x19, [sp, #64]
str x20, [sp, #72]
str x21, [sp, #80]
str x22, [sp, #88]
str x23, [sp, #96]
str x24, [sp, #104]
str x25, [sp, #112]
str x26, [sp, #120]
str x27, [sp, #128]
str x28, [sp, #136]
str x29, [sp, #144]
str s16, [sp, #152]
str s17, [sp, #156]
str s18, [sp, #160]
str s19, [sp, #164]
str s20, [sp, #168]
str s21, [sp, #172]
str s22, [sp, #176]
str s23, [sp, #180]
.data
_CONSTANT_2: .ascii "Enter a number:
\000"
.align 3
.text
ldr x9, =_CONSTANT_2
mov x0, x9
bl _write_str
bl _read_int
mov w9, w0
ldr x10, =_g_n
str w9, [x10, #0]
ldr x14, =_g_n
ldr w9, [x14,#0]
.data
_CONSTANT_3: .word 1
.align 3
.text
ldr w10, _CONSTANT_3
add w9, w9, w10
ldr x10, =_g_n
str w9, [x10, #0]
ldr x14, =_g_n
ldr w9, [x14,#0]
.data
_CONSTANT_4: .word 1
.align 3
.text
ldr w10, _CONSTANT_4
cmp w9, w10
cset w9, gt
cmp w9, #0
beq _elseLabel_1
bl _start_fact
mov w9, w0
str w9, [x29, #-4]
b _ifExitLabel_1
_elseLabel_1:
.data
_CONSTANT_5: .word 1
.align 3
.text
ldr w9, _CONSTANT_5
str w9, [x29, #-4]
_ifExitLabel_1:
.data
_CONSTANT_6: .ascii "The factorial is 
\000"
.align 3
.text
ldr x9, =_CONSTANT_6
mov x0, x9
bl _write_str
ldr w9, [x29, #-4]
mov w0, w9
bl _write_int
.data
_CONSTANT_7: .ascii "\n
\000"
.align 3
.text
ldr x9, =_CONSTANT_7
mov x0, x9
bl _write_str
_end_MAIN:
ldr x9, [sp, #8]
ldr x10, [sp, #16]
ldr x11, [sp, #24]
ldr x12, [sp, #32]
ldr x13, [sp, #40]
ldr x14, [sp, #48]
ldr x15, [sp, #56]
ldr x19, [sp, #64]
ldr x20, [sp, #72]
ldr x21, [sp, #80]
ldr x22, [sp, #88]
ldr x23, [sp, #96]
ldr x24, [sp, #104]
ldr x25, [sp, #112]
ldr x26, [sp, #120]
ldr x27, [sp, #128]
ldr x28, [sp, #136]
ldr x29, [sp, #144]
ldr s16, [sp, #152]
ldr s17, [sp, #156]
ldr s18, [sp, #160]
ldr s19, [sp, #164]
ldr s20, [sp, #168]
ldr s21, [sp, #172]
ldr s22, [sp, #176]
ldr s23, [sp, #180]
ldr x30, [x29, #8]
mov sp, x29
add sp, sp, #8
ldr x29, [x29,#0]
RET x30
.data
_frameSize_MAIN: .word 192
