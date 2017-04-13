; INTEGER*4 FUNCTION ISAMAX (NSX, SX, INCSX)
;-----------------------------------------------------------------------
;! Convex Pseudo AP routine: Find max. abs. element in a vector.
;#AP-appl
;-----------------------------------------------------------------------
;;  Copyright (C) 1995
;;  Associated Universities, Inc. Washington DC, USA.
;;
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 2 of
;;  the License, or (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public
;;  License along with this program; if not, write to the Free
;;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
;;  MA 02139, USA.
;;
;;  Correspondence concerning AIPS should be addressed as follows:
;;         Internet email: aipsmail@nrao.edu.
;;         Postal address: AIPS Project Office
;;                         National Radio Astronomy Observatory
;;                         520 Edgemont Road
;;                         Charlottesville, VA 22903-2475 USA
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
; INTEGER*4                  NSX, INCSX
; REAL*4                     SX(NSX)
;
; ISAMAX gets the index of the maximum absolute value of a vector.
; It is an integer function that accepts as arguments:
; a vector, the stride between elements, and the length of the vector.
; The input vector is single precision floating point.
;
; 0(ap) is N, 4(ap) is SX the vector to be analysed, 8(ap) is the
; stride, incx,  between elements  (the stride in unity elements, must be
; converted to bytes. NOTE: INCSX is not allowed to be negative!
;
        .globl _isamax_
_isamax_:
        sub.w   s0,s0           ; zero S0 for return and index value
        ld.w    #0x7fffffff,s5  ; constant for absolute value
        ld.s    #-1.0,s1        ; s1=-1.0 is initial minimum.
        ld.w    #1,s4           ; s4 is 1 for scalar return section
        ld.w    @0(ap),a3       ; n vector length
        le.w    #0,a3           ; 0.LE.NSX ?
        brs.f   rtn             ; if not, quit  (i.e., NSX.LE.0)
        ld.w    4(ap),a4        ; the address of the sx vector
        le.w    #9,a3           ; is vector length less than or equal 9
        bra.f   scalar          ; scalar routine
        ld.w    @8(ap),s3       ; fetch the stride
        shf     #2,s3           ; from words to bytes
        mov.w   s3,vs           ; and put into vector stride register
                                ; (VS constant throughout the computation)
        mul.w   #128,s3         ; no. bytes between vector loads
        mov     s3,a2           ; increment on address of SX
        sub.w   s3,s3           ; used for trip section count
        add.w   #1,s3           ; now origin 1 index is generated
        ld.w    #1,s4           ; s4 now odd to fetch odd part of vm(64..127)
        sub.l   s6,s6           ; constant 0 all 64 bits
;
; NOTE: masking only works for floating point; ABS of integers is different.
;
again:  mov     a3,vl           ; load vl
        ld.w    0(a4),v0        ; load 128 or less elements into v0
        and     v0,s5,v1        ; chain and take absolute via or 1 in sign bit
        add.w   a2,a4           ; update SX address
        sub.w   #128,a3         ; decrement count
        lt.s    s1,v1           ; s1.LT.v1(i) ?
        plc.t   vm,s2           ; get pop count of VM
        lt.w    #0,s2           ; 0.LT.s2 ?
        brs.t   newmax          ; if so, a new max has been detected.
        add.w   #128,s3         ; increment index count
        lt.w    #0,a3           ; 0.LT.a3 ?
        bra.t   again           ; if so, not finished, keep searching
        br      rtn             ; else, quit
newmax: max.s   v1              ; max absolute into s1, max(s1,v1)
        eq.s    s1,v1           ; generate bit mask to determine index
        mov     s6,vm,s7        ; move first 64 bits of vm, use s6 as selector
;
; test for 0 first.  if 0 don't do the tzc, save time
;
        eq.l    s6,s7           ; any hits?
        brs.t   skip            ; no, check next part of vm
        tzc     s7,s7           ; count the number of 0
        mov.w   s7,s0           ; new index relative to vector load
                                ; a new max has been found.
        add.w   s3,s0           ; fix the index relative to array
        lt.w    #0,a3           ; 0.LT.a3 ?
        bra.f   rtn             ; if not, quit
        add.w   #128,s3         ; update to reference next section
        br      again           ; not finished, keep searching
;
; move second half of vm to s7
;
skip:   add.w   #64,s3          ; update to reference next section
        mov     s4,vm,s7        ; move last 64 bits into s1
        tzc     s7,s7           ; count the number of 0's
        mov.w   s7,s0           ; new index relative to 0
        add.w   s3,s0           ; fix the index relative to 64
        lt.w    #0,a3           ; 0.LT.a3 ?
        bra.f   rtn             ; if not, quit
        add.w   #64,s3          ; update to reference next section
        br      again           ; not finished, keep searching
;
;at this time a full 128 or less  element section of sx has been searched.
rtn:    rtn

scalar: eq.w    #0,a3           ; is there any elements left?
        bra.t   srtn            ; if 0 exit
        ld.w    0(a4),s2        ; load the next element
        and     s5,s2           ; take the absolute value
        le.w    s2,s1           ; new is less than or equ prev(s1)
        brs.t   sagain          ; if true max still in s1
        mov.w   s2,s1           ; new element to be compared with
        mov.w   s4,s0           ; new index element
sagain: add.w   #1,s4           ; bump trip here for origin 0
        add.w   #4,a4           ; next sx element
        add.w   #-1,a3          ; decrement count by y
        br      scalar          ; top of loop and test
srtn:   rtn                     ; goodbye

