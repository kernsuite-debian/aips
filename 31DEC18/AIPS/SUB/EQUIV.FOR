      SUBROUTINE EQUIV (YES)
C-----------------------------------------------------------------------
C! checks whether two variables are logically equivalent
C# POPS-lang
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
C   EQUIV checks whether 2 variables are logically equivalent.
C   Scalars are equivalent if they are = within 1.e-6 of the larger
C   (numeric) or if the strings are the same neglecting trailing
C   blanks.  Arrays are equivalent to scalars if each element of the
C   array is equivalent to the scalar.  Arrays are equivalent to
C   arrays if they are equivalent element by element.
C   Output:
C      YES    L       T => they are equivalent
C   Commons:
C      alters K array;
C             SP in /POPS/;
C             SYTYPE and TAG in /SMSTUF/
C-----------------------------------------------------------------------
      LOGICAL   YES
C
      CHARACTER PRGNAM*6, CT*1
      LOGICAL   EQUAL
      INTEGER   POTERR, ICSIZE, ISIZE, IOFF, ITAG, ITYPE, J, M, NCSIZE,
     *   NSIZE, IACT, NOFF, NACT, ICWORD, NCWORD
      REAL      XXX, Y, Z, EPS
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'EQUIV '/
      DATA EPS /1.E-6/
C-----------------------------------------------------------------------
      YES = .FALSE.
C                                       find type of assignment
      POTERR = 7
      IF (SP.LT.2) GO TO 980
C                                       numeric scalars
      IF ((STACK(SP-1).EQ.2) .OR. (STACK(SP).EQ.2)) GO TO 10
         XXX = V(SP-1)
         Y = V(SP)
         SP = SP - 2
         Z = MAX (ABS(XXX), ABS(Y)) * EPS
         IF (Z.EQ.0.) Z = EPS
         IF (ABS(XXX-Y).LE.Z) GO TO 970
         GO TO 999
C                                       Something is a vector
 10   IF (SP.LT.5) GO TO 980
      IF ((STACK(SP).EQ.2) .AND. (STACK(SP-4).EQ.2)) GO TO 30
C                                       other is scalar
C                                       LHS scalar
      IF (STACK(SP).EQ.2) THEN
         SYTYPE = STACK(SP-3)
         TAG = STACK(SP-1)
         NSIZE = STACK(SP-2)
         Y = V(SP-4)
C                                       RHS scalar
      ELSE
         SYTYPE = STACK(SP-4)
         TAG = STACK(SP-2)
         NSIZE = STACK(SP-3)
         Y = V(SP)
         END IF
 20   SP = SP - 5
      POTERR = 23
      IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12)) GO TO 980
      IF (SYTYPE.EQ.2) NSIZE = K(NSIZE)
      M = TAG + NSIZE - 1
      DO 25  J = TAG,M
         XXX = C(J)
         Z = MAX (ABS(XXX), ABS(Y)) * EPS
         IF (Z.EQ.0.0) Z = EPS
         IF (ABS(XXX-Y).GE.Z) GO TO 999
 25      CONTINUE
      GO TO 970
C                                       Both structures
 30   IF (SP.LT.8) GO TO 980
      ITAG = STACK(SP-1)
      ISIZE = STACK(SP-2)
      ITYPE = STACK(SP-3)
      TAG = STACK(SP-5)
      NSIZE = STACK(SP-6)
      SYTYPE = STACK(SP-7)
      POTERR = 8
      IF ((SYTYPE.EQ.2) .AND. (ITYPE.NE.2) .AND. (ITYPE.NE.12))
     *   GO TO 980
      IF ((SYTYPE.EQ.12) .AND. (ITYPE.NE.2) .AND. (ITYPE.NE.12))
     *   GO TO 980
      IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12) .AND. (ITYPE.EQ.2))
     *   GO TO 980
      IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12) .AND. (ITYPE.EQ.12))
     *   GO TO 980
      SP = SP - 8
C                                       Real vector = vector
      IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.12)) GO TO 40
         IF (ITYPE.EQ.2) ISIZE = K(ISIZE)
         IF (SYTYPE.EQ.2) NSIZE = K(NSIZE)
         IF (ISIZE.NE.NSIZE) GO TO 980
         M = TAG + NSIZE - 1
         DO 35 J = TAG,M
            XXX = C(J)
            Y = C(J-TAG+ITAG)
            Z = MAX (ABS(XXX), ABS(Y))
            IF (ABS(XXX-Y).GT.EPS*Z) GO TO 999
 35         CONTINUE
         GO TO 970
C                                       Characters
 40   ICSIZE = ISIZE
      IF (ITYPE.EQ.7) ICSIZE = K(ISIZE+3)
      IF (ITYPE.EQ.9) ICSIZE = MOD (ISIZE, 1024)
      NCSIZE = NSIZE
      IF (SYTYPE.EQ.7) NCSIZE = K(NSIZE+3)
      IF (SYTYPE.EQ.9) NCSIZE = MOD (NSIZE, 1024)
      IOFF = 1
      IF (ITYPE.EQ.9) IOFF = ISIZE / 1024
      NOFF = 1
      IF (SYTYPE.EQ.9) NOFF = NSIZE / 1024
      IF ((ICSIZE.EQ.0) .AND. (NCSIZE.EQ.0)) GO TO 970
      IF ((ICSIZE.LE.0) .OR. (NCSIZE.LE.0)) GO TO 999
      IF ((IOFF.LE.0) .OR. (NOFF.LE.0)) GO TO 980
C                                       both scalar strings
      IF ((ITYPE.EQ.7) .AND. (K(ISIZE+1).GT.1)) GO TO 50
      IF ((SYTYPE.EQ.7) .AND. (K(NSIZE+1).GT.1)) GO TO 70
C                                       # actual chars
         CALL SPFIL (CH(ITAG), ICSIZE, IACT)
         CALL SPFIL (CH(TAG), NCSIZE, NACT)
         ICSIZE = MIN (ICSIZE, IACT)
         NCSIZE = MIN (NCSIZE, NACT)
C                                       both blank?
         IF ((ICSIZE-IOFF.GE.0) .OR. (NCSIZE-NOFF.GE.0)) GO TO 45
            CALL H2CHR (1, IOFF, CH(ITAG), CT)
            YES = CT.EQ.' '
            IF (YES) THEN
               CALL H2CHR (1, NOFF, CH(TAG), CT)
               YES = CT.EQ.' '
               END IF
            GO TO 999
C                                       compare
 45      CONTINUE
            IF (NCSIZE-NOFF.NE.ICSIZE-IOFF) GO TO 999
            M = ICSIZE - IOFF + 1
            CALL CHCOMP (M, NOFF, CH(TAG), IOFF, CH(ITAG), YES)
            GO TO 999
C                                        TAG scalar, ITAG array
 50   IF ((SYTYPE.EQ.7) .AND. (K(NSIZE+1).GT.1)) GO TO 90
         IF (ICSIZE.LE.0) GO TO 980
         ICWORD = (ICSIZE + 3) / 4
         M = K(ISIZE) / ICWORD
         CALL SPFIL (CH(TAG), NCSIZE, NACT)
         NCSIZE = MIN (NCSIZE, NACT)
C                                       scalar is blank
         IF (NCSIZE-NOFF.GE.0) GO TO 60
            CALL H2CHR (1, NOFF, CH(TAG), CT)
            IF (CT.NE.' ') GO TO 999
            DO 55 J = 1,M
               CALL SPFIL (CH(ITAG), ICSIZE, IACT)
               IF (MIN(ICSIZE,IACT)-IOFF.GE.0) GO TO 999
               CALL H2CHR (1, IOFF, CH(ITAG), CT)
               IF (CT.NE.' ') GO TO 999
               ITAG = ITAG + ICWORD
 55            CONTINUE
            GO TO 970
C                                       scalar not blank
 60      CONTINUE
            NACT = NCSIZE - NOFF + 1
            DO 65 J = 1,M
               CALL SPFIL (C(ITAG), ICSIZE, IACT)
               IACT = MIN (ICSIZE, IACT)
               IF (IACT-IOFF.NE.NCSIZE-NOFF) GO TO 999
               CALL CHCOMP (NACT, IOFF, CH(ITAG), NOFF, CH(TAG), EQUAL)
               IF (.NOT.EQUAL) GO TO 999
               ITAG = ITAG + ICWORD
 65            CONTINUE
            GO TO 970
C                                        ITAG scalar, TAG array
 70   CONTINUE
         IF (NCSIZE.LE.0) GO TO 980
         NCWORD = (NCSIZE+3) / 4
         M = K(NSIZE) / NCWORD
         CALL SPFIL (CH(ITAG), ICSIZE, IACT)
         ICSIZE = MIN (ICSIZE, IACT)
C                                       scalar is blank
         IF (ICSIZE-IOFF.GE.0) GO TO 80
            CALL H2CHR (1, IOFF, CH(ITAG), CT)
            IF (CT.NE.' ') GO TO 999
            DO 75 J = 1,M
               CALL SPFIL (CH(TAG), NCSIZE, NACT)
               IF (MIN(NCSIZE,NACT)-NOFF.GE.0) GO TO 999
               CALL H2CHR (1, NOFF, CH(TAG), CT)
               IF (CT.NE.' ') GO TO 999
               TAG = TAG + NCWORD
 75            CONTINUE
            GO TO 970
C                                       scalar not blank
 80      CONTINUE
            IACT = ICSIZE - IOFF + 1
            DO 85 J = 1,M
               CALL SPFIL (C(TAG), NCSIZE, NACT)
               NACT = MIN (NCSIZE, NACT)
               IF (NACT-NOFF.NE.ICSIZE-IOFF) GO TO 999
               CALL CHCOMP (IACT, IOFF, CH(ITAG), NOFF, CH(TAG), EQUAL)
               IF (.NOT.EQUAL) GO TO 999
               TAG = TAG + NCWORD
 85            CONTINUE
            GO TO 970
C                                       both arrays
 90   CONTINUE
         IF ((NCSIZE.LE.0) .OR. (ICSIZE.LE.0)) GO TO 980
         NCWORD = (NCSIZE+3) / 4
         ICWORD = (ICSIZE + 3) / 4
         NSIZE = K(NSIZE) / NCWORD
         ISIZE = K(ISIZE) / ICWORD
         IF ((NSIZE.LE.0) .OR. (ISIZE.LE.0)) GO TO 980
         IF (ISIZE.NE.NSIZE) GO TO 999
         DO 105 J = 1,NSIZE
            CALL SPFIL (CH(ITAG), ICSIZE, IACT)
            CALL SPFIL (CH(TAG), NCSIZE, NACT)
            NACT = MIN (NCSIZE, NACT)
            IACT = MIN (ICSIZE, IACT)
            IF ((NACT-NOFF.GE.0) .OR. (IACT-IOFF.GE.0)) GO TO 95
               CALL H2CHR (1, IOFF, CH(ITAG), CT)
               EQUAL = CT.EQ.' '
               IF (EQUAL) THEN
                  CALL H2CHR (1, NOFF, CH(TAG), CT)
                  EQUAL = CT.EQ.' '
                  END IF
               GO TO 100
 95         CONTINUE
               IF (NACT-NOFF.NE.IACT-IOFF) GO TO 999
                  M = IACT - IOFF + 1
                  CALL CHCOMP (M, IOFF, CH(ITAG), NOFF, CH(TAG), EQUAL)
 100        IF (.NOT.EQUAL) GO TO 999
               ITAG = ITAG + ICWORD
               TAG  = TAG + NCWORD
 105        CONTINUE
         GO TO 970
C                                       They are equiv
 970  YES = .TRUE.
      GO TO 999
C                                        error return
 980  ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
