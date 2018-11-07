;-----------------------------------------------------------------------
;! Convex Pseudo AP routine:Vector polar to rectangular conversion.
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
; Convex assembly routine
; Hand revised version of the following Fortran routine:
;  1       SUBROUTINE QRECT (A, I, C, K, N)
;  2 C---------------------------------------------------------------------
;  3 C   Vector table lookup version
;  4 C   Vector polar to rectangular conversion:
;  5 C      C(mK) = A(mI) * COS (A(mI+1))
;  6 C      C(mK+1) = A(mI) * SIN (A(mI+1)) for m = 0 to N-1
;  7 C   Does a table lookup with a third order series expansion correction:
;  8 C     COS (tab+fract) = COS(tab)*COS(fract) - SIN(tab)*SIN(fract)
;  9 C     SIN (tab+fract) = SIN(tab)*COS(fract) + COS(tab)*SIN(fract)
; 10 C    Inputs:
; 11 C     A  I*4  Source vector base address
; 12 C     I  I*4  A address increment
; 13 C     C  I*4  Destination vector base address
; 14 C     K  I*4  C address increment
; 15 C     N  I*4  Element count
; 16 C---------------------------------------------------------------------
; 17       INTEGER*4 A, I, C, K, N, JA, JC, LOOP, IARG, ISADD, JARG
; 18       REAL*4    PIO2, CLUT, SLUT, CSER, SSER, FRACT, FRACT2,
; 19      *   RMHALF, RMSIXT
; 20       REAL*4    ARG, XARG
; 21       INTEGER*4 LENLUT, CTMASK
; 22       REAL*4    COSTAB(512), CTBLBL, CHINDE
; 23       REAL*4    CLUTSC, CLUTSI
; 24 C                                                          Include DAPC
; 25 C                                       Include for AP memory and work
; 43 C                                                          Include CAPC
; 44 C                                       Include for AP memory and work
; 51       COMMON /COSLUT/ CLUTSC, CLUTSI, LENLUT, CTMASK, CTBLBL, COSTAB
; 52 C                                                          Include EAPC
; 53 C                                       Include for AP memory and work
; 64       DATA CHINDE /'INDE'/
; 65       DATA PIO2 /1.570796327/
; 66 C---------------------------------------------------------------------
; 67       IF (N.LE.0) GO TO 999
; 68       IF (CTBLBL.EQ.CHINDE) GO TO 50
; 69 C                                       Initialize.
; 70          CALL QCTLUT
; 74 C                                       Addresses 1-rel
; 71  50   RMHALF = -1.0 / 2.0
; 72       RMSIXT = -1.0 / 6.0
; 73       ISADD = -PIO2 * CLUTSC + 0.5 + LENLUT
; 75       JA = A + 1
; 76       JC = C + 1
; 77 C                                       Loop
; 78 C$DIR NO_RECURRENCE
; 79
; 80       DO 100 LOOP = 1,N
; 81 C                                       COS lookup
; 82          ARG = APCORE(JA+1) * CLUTSC
; 83          IARG = ARG
; 84 C                                       FRACT in radians
; 85          XARG = IARG
; 86          FRACT = (ARG - XARG) * CLUTSI
; 87          FRACT2 = FRACT * FRACT
; 88 C                                       Convert IARG to table pointer,
; 89 C                                       following trick works for 2's
; 90 C                                       complement.
; 91          IARG = IAND (IARG, CTMASK)
; 92 C                                       Lookup
; 93          CLUT = COSTAB (IARG+1) * APCORE(JA)
; 94 C                                       SIN lookup
; 95          IARG = IARG + ISADD
; 96 C                                       Modulus
; 97          IARG = IAND (IARG, CTMASK)
; 98          SLUT = COSTAB (IARG+1) * APCORE(JA)
; 99 C                                       Series expansions
;100          CSER = 1.0 + RMHALF * FRACT2
;101          SSER = FRACT + RMSIXT * FRACT * FRACT2
;102          APCORE(JC) = (CLUT * CSER - SLUT * SSER)
;103          APCORE(JC+1) = (SLUT * CSER + CLUT * SSER)
;104          JA = JA + I
;105          JC = JC + K
;106  100     CONTINUE
;107 C
;108  999  RETURN
;109       END

; INITIALIZED DATA

        .data
        .align  8
LI:
        ds.w    1(0x494e4445)  ; 'INDE'
        ds.s    1.570796327    ; PIO2   = Pi/2
MAJIK:  ds.s    1.2582912e7    ; Magic floating point value.

; UNINITIALIZED DATA

        .bss
        .align  8
LU:
        bs.b    4 ; +0, JA
        bs.b    4 ; +4, JC
        bs.b    4 ; +8,
        bs.b    4 ; +12, Used for n_left
        bs.b    4 ; +16, ISADD*4
        bs.b    4 ; +20, Used for I*4 (stride in bytes)
        bs.b    4 ; +24, Used for K*4 (stride in bytes)
        bs.b    4 ; +28, Used for CTMASK*4
        .text   1
LC:
; CALL PACKET AT LC+0
        ds.w    0
        .data
        ds.w    5(0)
;     Register useage in loop:

;     A0 = sacred
;     A1 =
;     A2 =
;     A3 = offset of APCORE(JA)
;     A4 = offset of APCORE(JC)
;     A5 = base address of COSTAB(1) used by ldvi
;     A6 = sacred
;     A7 = sacred

;     S0 =
;     S1 =
;     S2 =
;     S3 = RMHALF (-1./2.)
;     S4 = RMSIXT (-1./6.)
;     S5 = CLUTSI
;     S6 = CLUTSC
;     S7 =

;     V0 =
;     V1 = CSER
;     V2 = FRACT
;     V3 = IARG
;     V4 =
;     V5 = SLUT * APCORE(JA)
;     V6 = CLUT * APCORE(JA)
;     V7 = SSER
;


; INSTRUCTIONS

        .text
        ds.w    0x1080000
        ds.b    "-O2\0"
        .globl  _qrect_
_qrect_:
        sub.w   #0x0000670,sp   ; #1
;67       IF (N.LE.0) GO TO 999
        ld.w    @16(ap),s0      ; #67, N
        lt.w    #0x0000000,s0   ; #67
        jbrs.f  L2              ; #67
;68       IF (CTBLBL.EQ.CHCOST) GO TO 50
        ld.w    LI,s2           ; #68, CHCOST
        ld.w    __coslut_+16,s1 ; #68, CTBLBL
        eq.w    s1,s2           ; #68
        jbrs.t  L3              ; #68
;70          CALL QCTLUT
        ldea    LC+4,ap         ; #70, ?LC
        calls   _qctlut_        ; #70, QCTLUT
        ld.w    12(fp),ap       ; #70
L4:                             ; Stmt _L4
L3:             ; Stmt 50
;71 50    RMHALF = -1.0 / 2.0
;72       RMSIXT = -1.0 / 6.0
;73       ISADD = -PIO2 * CLUTSC + 0.5 + LENLUT
        ld.w    LI+4,s3         ; #73, PIO2 in s3
        ld.w    __coslut_,s4    ; #73, CLUTSC in s4
        ld.s    #0.5,s5         ; #73, 0.5 in s5
        ld.w    __coslut_+8,s6  ; #73, LENLUT in s6
        mul.s   s4,s3   ; #73 PIO2 * CLUTSC
        sub.s   s3,s5   ; #73 0.5 - PIO2 * CLUTSC
        cvtw.s  s6,s6   ; #73 float (LENLUT)
        add.s   s6,s5   ; #73 LENLUT + 0.5 - PIO2*CLUTSC
        cvts.w  s5,s5   ; #73 fix ISADD
        shf     #2,s5   ; #73 convert to bytes
        st.w    s5,LU+16        ; #73, store ISADD
;75       JA = A + 1
;76       JC = C + 1
        ld.w    @16(ap),a2      ; load N
        st.w    a2,LU+12        ; store n_left
        ld.w    @0(ap),a3       ; #75, A
        ld.w    @8(ap),a4       ; #76, C
        shf     #2,a3           ; #75 convert to bytes
        shf     #2,a4           ; #76 convert to bytes
        ldea    __coslut_+20,a5 ; load base address of COSTAB(1)
        ld.s    #-0.5,s3        ; load RMHALF
        ld.s    #-0.1666666667,s4     ; load RMSIXT
        ld.s    __coslut_,s5    ; load CLUTSC
        ld.s    __coslut_+4,s6  ; load CLUTSI
        ld.w    __coslut_+12,a1 ; load CTMASK
        shf     #2,a1           ; shift to mask in bytes
        st.w    a1,LU+28        ; store CTMASK*4
        mov     a2,vl           ; vector length = n
        ld.w    @12(ap),a1      ; set output vector stride
        shf     #2,a1           ; K to bytes
        st.w    a1,LU+24        ; save
        ld.w    @4(ap),a1       ; load I => input stride
        shf     #2,a1           ; I to bytes
        st.w    a1,LU+20        ; save
        mov     a1,vs           ; vector stride

LOOP:
;80       DO 100 LOOP = 1,N
;81 C                                       COS lookup
;82          ARG = APCORE(JA+1) * CLUTSC
        ld.w   MAJIK,s0         ; magic number
        ld.w   __coslut_+12,s1  ; mask to get table entry number
        ld.s    __apfake_+4(a3),v6   ; #82 load APCORE(JA+1)
        mul.s   v6,s5,v1        ; #82 APCORE(JA+1) * CLUTSC
;83          IARG = ARG
;
;   fix v1 -> v3 & mask to range (0,LENLUT-1)
;84 C                                       FRACT in radians
;85          XARG = IARG

;   float v3 -> v4
;   add floating number which causes the lsb to be the one bit.
        add.s  v1,s0,v4         ; discard fractional bits
        and    v4,s1,v3         ; now have int(abs(v1)) as table pointer
        sub.s  v4,s0,v4         ; restore v4 to float(int(v1))
;
;86          FRACT = (ARG - XARG) * CLUTSI
        sub.s   v1,v4,v5        ; #86 ARG - XARG
        mul.s   v5,s6,v2        ; #86 FRACT = (ARG-XARG) * CLUTSI
        ld.w    LU+16,s2        ; #95 load s2 with ISADD
        ld.w    #2,s1           ; #93 to shift table pointers to bytes
        ld.s    __apfake_(a3),v7   ; #93 load APCORE(JA) to v7
;87          FRACT2 = FRACT * FRACT
        mul.s   v2,v2,v4        ; #87 FRACT2 = FRACT*FRACT
;91          IARG = IAND (IARG, CTMASK)
;         already done above
        shf     s1,v3           ; #93 table pointers to bytes
;93          CLUT = COSTAB (IARG+1) * APCORE(JA)
        ld.w    LU+28,s7        ; #97 load CTMASK*4 to s7
        ldvi.s  v3,v1           ; #93 gather COSTAB values
        mul.s   v1,v7,v6        ; #93 CLUT = COSTAB(IARG+1)*APCORE(JA)
;95          IARG = IARG + ISADD
        add.w   v3,s2,v5        ; #95 IARG = IARG + ISADD
;97          IARG = IAND (IARG, CTMASK)
        and     v5,s7,v0        ; #97 IARG = IAND (IARG, CTMASK)
;98          SLUT = COSTAB (IARG+1) * APCORE(JA)
        ld.s    #1.0e0,s1       ; #100 load 1.0 to s1
        ldvi.s  v0,v3           ; #98 gather COSTAB values
        mul.s   v3,v7,v5        ; #98 SLUT = COSTAB(IARG+1)*APCORE(JA)
;100          CSER = 1.0 + RMHALF * FRACT2
        mul.s   v4,s3,v0        ; #100 RMHALF*FRACT2
        add.s   v0,s1,v1        ; #100 CSER = 1.0+RMHALF*FRACT2
;101          SSER = FRACT + RMSIXT * FRACT * FRACT2
        mul.s   v2,s4,v3        ; #101 $t2=RMSIXT*FRACT
        mul.s   v3,v4,v0        ; #101 $t3=$t2*FRACT2
        add.s   v0,v2,v7        ; #101 SSER=$t3+FRACT
;102          APCORE(JC) = (CLUT * CSER - SLUT * SSER)
        ld.w    LU+24,a2        ; #102 set vector stride = K*4
        ld.w    LU+20,a1        ; load I*4 to a1
        mov     a2,vs           ; #102 vector stride
        mul.s   v6,v1,v3        ; #102 $t4=CLUT*CSER
        mul.s   v5,v7,v0        ; #102 $t5=SLUT*SSER
        sub.s   v3,v0,v4        ; #102 APCORE(JC)=$t4-$t5
        st.s    v4,__apfake_(a4)     ; #102 store APCORE(JC)
;103          APCORE(JC+1) = (SLUT * CSER + CLUT * SSER)
        ld.w    LU+12,a2        ; load n_left in a2
        mul.s   v5,v1,v3        ; #102 $t6=SLUT*CSER
        mul.s   v6,v7,v0        ; #102 $t7=CLUT*SSER
        add.s   v3,v0,v2        ; #102 APCORE(JC)=$t6+$t7
        st.s    v2,__apfake_+4(a4)   ; #102 store APCORE(JC+1)
;   loop cleanup  - a bit of shuffling to avoid memory references
        mov     a5,s1           ; save a5 in s1
        mov     vs,a5           ; save vs (K*4) in a5=>s2
        mov     a5,s2           ;
        mov     a1,vs           ; vector stride I*4 for next pass
        mov     vl,a5           ; vector length to a5
        mul.w   a5,a1           ; vl * I * 4
        add.w   a1,a3           ; increment JA+1
        mov     s2,a1           ; K*4 to a1
        mul.w   a5,a1           ; vl * K*4
        add.w   a1,a4           ; increment JC
        sub.w   a5,a2           ; decrement N by vector length
        mov     a2,vl           ; new vector length
        mov     s1,a5           ; restore old a5
        st.w    a2,LU+12        ; store new n_left
        lt.w    #0x0000000,a2   ; #80 loop if not done
        jbra.t  LOOP            ; #80
L2:                             ; Stmt 999
        rtn                     ; #108
        .comm   __coslut_,2068
        .comm   __spf_,64
        .comm   __apfake_,1441836
