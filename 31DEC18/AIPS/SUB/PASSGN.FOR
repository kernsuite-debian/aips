      SUBROUTINE  PASSGN
C-----------------------------------------------------------------------
C! handles array(i) = value1,value2,...,valuen constructs
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 2000
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   PASSGN alters stacks to handle ARRAY or ARRAY(i) = v1,v2,v3,...vn
C   where array i -> i+n-1 are the only values altered
C   Commons alterred: K array; SP and TAG in /POPS/ & /SMSTUF/,resp.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   STEMP(100), LHSPTR, LHSDSC, LHSLEN, LHSNEL, RHSDSC, J,
     *   RHSLEN, RHSNEL, NVALS, WORKSP, RHSTAG, III, ITAG, JJJ, POTERR
      REAL      VTEMP(100), RM
      LOGICAL   ISARR
      HOLLERITH HM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (RM, HM)
      DATA PRGNAM /'PASSGN'/
C-----------------------------------------------------------------------
C                                       Save copies of STACK and V.
      CALL RCOPY (100, V, VTEMP)
      CALL COPY (100, STACK, STEMP)
C                                       NVALS is no. of RHS items.
      POTERR = 8
      IF (SP.LE.0) GO TO 980
      NVALS = STEMP(SP)
      WORKSP = SP - 1
      POTERR = 23
C                                       Work down stack to LHS values.
C                                       Test for RHS type
      DO 20 III = 1,NVALS
         IF (STEMP(WORKSP).EQ.2) WORKSP = WORKSP - 3
         WORKSP = WORKSP - 1
 20      CONTINUE
C                                       Set LHS values
      ISARR = STEMP(WORKSP).EQ.2
C                                       LHS = array name
      IF (ISARR) THEN
         IF (WORKSP.LT.4) GO TO 980
         LHSPTR = WORKSP - 3
         LHSDSC = STEMP(LHSPTR+1)
         TAG = STEMP(LHSPTR+2)
      ELSE
         LHSPTR = WORKSP
         TAG = STEMP(LHSPTR)
         END IF
C                                       check address
      POTERR = 25
      IF (TAG.LE.K(KXORG+7-1)) GO TO 980
C                                       Zero stack.
      J = SP - LHSPTR + 1
      CALL FILL (J, 0, STACK(LHSPTR))
C                                       WORKSP = beg of RHS list.
      WORKSP = WORKSP + 1
C                                       Check on LHS data type.
      POTERR = 8
      IF ((.NOT.ISARR) .OR. (STEMP(LHSPTR).EQ.2)) THEN
C                                       LHS numeric array.
         DO 40 III = 1,NVALS
            POTERR = 23
C                                       RHS item is constant or scalar
            IF (STEMP(WORKSP).NE.2) THEN
               IF ((STEMP(WORKSP).LT.16) .AND. (STEMP(WORKSP).GT.0))
     *            GO TO 980
               C(TAG) = VTEMP(WORKSP)
               WORKSP = WORKSP + 1
               TAG = TAG + 1
C                                       RHS is an array.
            ELSE
               IF (STEMP(WORKSP+3).NE.2) GO TO 980
               RHSDSC = STEMP(WORKSP+1)
               RHSNEL = K(RHSDSC)
               ITAG = STEMP(WORKSP+2)
               DO 35 J = 1,RHSNEL
                  C(TAG) = C(ITAG)
                  TAG = TAG + 1
                  ITAG = ITAG + 1
 35               CONTINUE
               WORKSP = WORKSP + 4
               END IF
 40         CONTINUE
C                                       LHS is an alpha array
      ELSE IF ((STEMP(LHSPTR).EQ.7) .OR. (STEMP(LHSPTR).EQ.8)) THEN
         IF (STEMP(LHSPTR).EQ.7) THEN
            LHSLEN = K(LHSDSC+3)
         ELSE
            LHSLEN = STEMP(LHSPTR+1)
            END IF
         IF (LHSLEN.LE.0) GO TO 980
         LHSNEL = (LHSLEN+3) / 4
         DO 80 III = 1,NVALS
            POTERR = 23
            IF (STEMP(WORKSP+3).NE.2) GO TO 980
            IF (STEMP(WORKSP).NE.7) THEN
               IF ((STEMP(WORKSP).NE.8) .AND. (STEMP(WORKSP).NE.14)
     *            .AND. (STEMP(WORKSP).NE.9)) GO TO 980
C                                       RHS is holerith or scalar.
C                                       Put RHS discriptors on stack.
               CALL COPY (4, STEMP(WORKSP), STACK(LHSPTR+4))
C                                       Put LHS discriptors on stack.
               STACK(LHSPTR) = 8
               STACK(LHSPTR+1) = LHSLEN
               STACK(LHSPTR+2) = TAG
               STACK(LHSPTR+3) = 2
               SP = LHSPTR + 7
               CALL ASSGN
               IF (ERRNUM.NE.0) GO TO 980
               TAG = TAG + LHSNEL
               WORKSP = WORKSP + 4
C                                       RHS is alpha array.
            ELSE
               RHSDSC = STEMP(WORKSP+1)
               RHSLEN = K(RHSDSC+3)
               IF (RHSLEN.LE.0) GO TO 980
               RHSLEN = (RHSLEN+3) / 4
               RHSNEL = K(RHSDSC) / RHSLEN
               RHSTAG = STEMP(WORKSP+2)
C                                       Set up LHS discriptor.
               STACK(LHSPTR) = 8
               STACK(LHSPTR+1) = LHSLEN
               STACK(LHSPTR+3) = 2
C                                       Set up RHS discriptor.
               STACK(LHSPTR+4) = 8
               STACK(LHSPTR+5) = K(RHSDSC+3)
               STACK(LHSPTR+7) = 2
               POTERR = 6
               DO 75 JJJ = 1,RHSNEL
                  SP = LHSPTR + 7
                  STACK(LHSPTR+2) = TAG
                  STACK(LHSPTR+6) = RHSTAG
                  CALL ASSGN
                  TAG = TAG + LHSNEL
                  RHSTAG = RHSTAG + RHSLEN
                  IF (ERRNUM.NE.0) GO TO 980
 75              CONTINUE
               WORKSP = WORKSP + 4
               END IF
 80         CONTINUE
C                                       Bad type
      ELSE
         GO TO 980
         END IF
      SP = LHSPTR - 1
      GO TO 999
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
