      SUBROUTINE ASSGN
C-----------------------------------------------------------------------
C! performs the assignment functions of scalar/vector = scalar/vector
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C   ASSGN performs the assignment functions: scalar = constant,
C   vector = constant, and vector = vector.
C   Output in commons:
C      K array
C      SP in /POPS/
C      SYTYPE and TAG in /SMSTUF/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   POTERR, ICSIZE, ISIZE, IOFF, ITAG, ITYPE, J, M, NCSIZE,
     *   NSIZE, IACT, NOFF, NACT, IXPT, LXPT, NCWORD, ICWORD
      CHARACTER CTEMP*256
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'ASSGN '/
C-----------------------------------------------------------------------
C                                       find type of assignment
      POTERR = 7
      IF (SP.LT.2) GO TO 980
      M = STACK(SP)
      IF (M.EQ.2) GO TO 30
      M = STACK(SP-1)
C                                       scalar = constant
      IF (M.EQ.2) GO TO 10
         POTERR = 25
         IF (M.LE.K(KXORG+7-1)) GO TO 980
         C(M) = V(SP)
         SP = SP - 2
         GO TO 999
C                                       Vector = Constant.
 10   IF (SP.LT.5) GO TO 980
      SYTYPE = STACK(SP-4)
      POTERR = 23
      IF (SYTYPE.NE.2) GO TO 980
         TAG = STACK(SP-2)
         POTERR = 25
         IF (TAG.LE.K(KXORG+7-1)) GO TO 980
         NSIZE = STACK(SP-3)
         NSIZE = K(NSIZE)
         M = TAG + NSIZE - 1
         DO 15  J = TAG,M
            C(J) = V(SP)
 15         CONTINUE
         SP = SP - 5
         GO TO 999
C                                       vector = vector
 30   IF (SP.LT.8) GO TO 980
      ITAG = STACK(SP-1)
      ISIZE = STACK(SP-2)
      ITYPE = STACK(SP-3)
      TAG = STACK(SP-5)
      NSIZE = STACK(SP-6)
      SYTYPE = STACK(SP-7)
      POTERR = 8
      IF (STACK(SP-4).NE.2) GO TO 980
      IF ((SYTYPE.EQ.2) .AND. (ITYPE.NE.2) .AND. (ITYPE.NE.12))
     *   GO TO 980
      IF ((SYTYPE.NE.2) .AND. (ITYPE.EQ.2)) GO TO 980
      IF ((SYTYPE.NE.2) .AND. (ITYPE.EQ.12)) GO TO 980
      IF (SYTYPE.EQ.12) GO TO 980
      IF (SYTYPE.EQ.14) GO TO 980
      POTERR = 25
      IF (TAG.LE.K(KXORG+7-1)) GO TO 980
      SP = SP - 8
C                                       Real vector = vector
      IF (SYTYPE.NE.2) GO TO 40
         IF (ITYPE.EQ.2) ISIZE = K(ISIZE)
         NSIZE = K(NSIZE)
         POTERR = 13
         IF (ISIZE.GT.NSIZE) GO TO 980
         CALL RCOPY (ISIZE, C(ITAG), C(TAG))
         NSIZE = NSIZE - ISIZE
         IF (NSIZE.GT.0) CALL RFILL (NSIZE, 0.0, C(TAG+ISIZE))
         GO TO 999
C                                       char element/subst = chars
 40   IF (SYTYPE.EQ.7) GO TO 50
         ICSIZE = ISIZE
         IF (ITYPE.EQ.7) ICSIZE = K(ISIZE+3)
         IF (ITYPE.EQ.9) ICSIZE = MOD (ISIZE, 1024)
         IOFF = 1
         IF (ITYPE.EQ.9) IOFF = ISIZE / 1024
         NOFF = 1
         IF (SYTYPE.EQ.9) NOFF = NSIZE / 1024
         IF (SYTYPE.EQ.9) NSIZE = MOD (NSIZE, 1024)
C                                       # actual chars on RHS
         CALL SPFIL (CH(ITAG), ICSIZE, IACT)
         POTERR = 13
         IF (NSIZE-NOFF.LT.IACT-IOFF) GO TO 980
         IF ((ITYPE.EQ.7) .AND. (K(ISIZE+1).NE.1)) GO TO 980
C                                       Do copy, then fill
         NACT = NSIZE - NOFF + 1
         IACT = IACT - IOFF + 1
         IF (IACT.GT.0) THEN
            CALL H2CHR (IACT, IOFF, CH(ITAG), CTEMP)
            CALL CHR2H (IACT, CTEMP, NOFF, CH(TAG))
            NOFF = NOFF + IACT
            NACT = NACT - IACT
            END IF
         IF (NACT.GT.0) CALL CHFILL (NACT, HBLANK, NOFF, CH(TAG))
         GO TO 999
C                                        char vector = const/substr
 50   IF (ITYPE.EQ.7) GO TO 70
         NCSIZE = K(NSIZE+3)
         IF (NCSIZE.LE.0) GO TO 980
         ICSIZE = ISIZE
         IF (ITYPE.EQ.9) ICSIZE = MOD (ISIZE, 1024)
         IOFF = 1
         IF (ITYPE.EQ.9) IOFF = ISIZE / 1024
         IF ((ICSIZE.LE.0) .OR. (IOFF.LE.0)) GO TO 980
         NCWORD = (NCSIZE + 3) / 4
         NSIZE = K(NSIZE) / NCWORD
C                                       Actual # chars on RHS
 55      POTERR = 13
         CALL SPFIL (CH(ITAG), ICSIZE, IACT)
         IF (NCSIZE.LT.IACT-IOFF+1) GO TO 980
         IACT = IACT - IOFF + 1
         IACT = MAX (IACT, 0)
         IXPT = 1 + IACT
         LXPT = NCSIZE - IACT
         IF (IACT.GT.0) CALL H2CHR (IACT, IOFF, CH(ITAG), CTEMP)
         DO 60 J = 1,NSIZE
            IF (IACT.GT.0) CALL CHR2H (IACT, CTEMP, 1, CH(TAG))
            IF (LXPT.GT.0) CALL CHFILL (LXPT, HBLANK, IXPT, CH(TAG))
            TAG = TAG + NCWORD
 60         CONTINUE
          GO TO 999
C                                        char vector = char vector
 70   CONTINUE
         IOFF = 1
         NCSIZE = K(NSIZE+3)
         ICSIZE = K(ISIZE+3)
         IF ((NCSIZE.LE.0) .OR. (ICSIZE.LE.0)) GO TO 980
         NCWORD = (NCSIZE + 3) / 4
         ICWORD = (ICSIZE + 3) / 4
         NSIZE = K(NSIZE) / NCWORD
         ISIZE = K(ISIZE) / ICWORD
         POTERR = 13
         IF (ISIZE.GT.NSIZE) GO TO 980
         IF (ISIZE.EQ.1) GO TO 55
         DO 80 J = 1,NSIZE
            IF (J.GT.ISIZE) CALL CHFILL (NCSIZE, HBLANK, 1, CH(TAG))
            IF (J.GT.ISIZE) GO TO 75
               CALL SPFIL (C(ITAG), ICSIZE, IACT)
               IF (IACT.GT.NCSIZE) GO TO 980
               IXPT = (IACT + 3) / 4
               CALL RCOPY (IXPT, CH(ITAG), CH(TAG))
               IXPT = 1 + IACT
               LXPT = NCSIZE - IACT
               IF (LXPT.GT.0) CALL CHFILL (LXPT, HBLANK, IXPT, CH(TAG))
               ITAG = ITAG + ICWORD
 75         TAG  =  TAG + NCWORD
 80         CONTINUE
         GO TO 999
C                                        error return
 980  ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
