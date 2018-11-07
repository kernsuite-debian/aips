        .globl  _whnalt_
;-----------------------------------------------------------------------
;! Convex Pseudo AP routine: Find elements with abs value less than...
;# AP-appl
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
; Convex assembly routine
;
; WHNALT returns a list of all locations in an integer array
; for which the integer absolute value is less than a specified
; target value. This is very similar to the Cray library routine
; WHENILT, the only difference being the absolute value. The
; algorithm is equivalent to the following FORTRAN code:
;
;       SUBROUTINE WHNALT (N, IARRAY, INC, ITARGET, INDEX, NVAL)
;C      (arg_pack offsets: 0     4     8     12      16     20 )
;       INTEGER*4          N, IARRAY, INC, ITARGET, INDEX, NVAL
;       INTEGER*4          I, J
;       NVAL = 0
;       J = 1
;       DO I = 1, N
;               IF (IABS(IARRAY(J)).LT.ITARGET) THEN
;                       NVAL = NVAL + 1
;                       INDEX(NVAL) = I
;               ENDIF
;               J = J + INC
;       ENDDO
;       RETURN
;       END
;
; Don Wells, NRAO-CV, 16-18June85.
; minor mod to speed up by Steve Wallach, Convex, Sept85.
; minor bug fix and text cleanup by Don Wells, 29Jan86
; trivial syntactic error fixed, DCW 14Feb86.
; ===============================================================
;               Register assignments:
; a1 = INC*4 (stride)   s1 = N counter          v1 = "iota" vector
; a2 = IARRAY pointer   s2 = ITARGET            v2 = IARRAY load reg
; a3 = INDEX pointer    s3 = NVAL counter       v3 = INDEX store reg
; a4 = temporary        s4 = pop_cnt last cmprs v4 = negated IARRAY
; a5 = INC*4*128        s5 = "iota" bump const  v5 = IABS() scratch
; ================================================================
_whnalt_:
        ld.w    @0(ap),s1       ; get N
        ld.w    @12(ap),s2      ; get ITARGET
        ld.w    #0,s3           ; init NVAL
        st.w    s3,@20(ap)      ;  "     "
        lt.w    #0,s1           ; 0.LT.N ?
        jbrs.f  quit            ; if not, go quit.
        ld.w    #4,vs           ; vs = 4 bytes
        ld.w    #128,vl         ; vl = 128
        ld.w    _mth$j_indx,v1  ; init "iota": 1...128
        ld.w    4(ap),a2        ; init IARRAY() pointer
        ld.w    16(ap),a3       ; init INDEX() pointer
        ld.w    #0,s4           ; prev_popcnt=0 initially
        ld.w    @8(ap),a1       ; get INC
        shf     #2,a1           ; INC*4 (stride in bytes)
        mov.w   a1,a5           ;
        shf     #7,a5           ; INC*4*128 (register stride)
        ld.w    #128,s5         ; init "iota" bump constant
looptop:                        ;
        mov.w   s1,vl           ; vl = N<6..0> (usually 128)
        mov.w   a1,vs           ; vs = INC*4
        ld.w    (a2),v2         ; load next 128 from IARRAY()
        neg.w   v2,v4           ; get negation of IARRAY(i)
        lt.w    v2,v4           ; IARRAY(i).LT.negated_IARRAY(i) ?
        mask.t  v2,v4,v5        ; v4 = v4 if T, v2 if F (IABS op)
        le.w    s2,v5           ; ITARGET.LE.IABS(IARRAY(i)) ?
        ;               following scalar ops execute concurrently
        ;               with the vector compare instruction:
        add.w   a5,a2           ; IARRAY = IARRAY + INC*4*128
        add.w   s4,s3           ; NVAL = NVAL + prev_vl
        mov.w   s4,a4           ;
        shf     #2,a4           ; words to bytes
        add.w   a4,a3           ; INDEX = INDEX + prev_vl*4
        sub.w   #128,s1         ; N = N - 128
        ld.w    #4,vs           ; vs = unity stride for INDEX() store
        ;               following vector compress is inhibited until
        ;               the mask operation above completes:
        cprs.f  v1,v3           ; compress "iota_of_hits" using vm
        plc.f   vm,s4           ; get pop count of non-zeroes
        mov.w   s4,vl           ; pop count is result vector length
        st.w    v3,(a3)         ; store compressed "iota" in INDEX()
        lt.w    #0,s1           ; 0.LT.N ?
        mov.w   s5,vl           ; vl = 128
        add.w   v1,s5,v1        ; iota(i) = iota(i) + 128
        jbrs.t  looptop         ; if so, loop back
;
        add.w   s4,s3           ; NVAL = NVAL + VL
        st.w    s3,@20(ap)      ; store NVAL
quit:   rtn
