      SUBROUTINE ADVRBS (ANAME, ATYPE, ALENG, CLENG, IVALU, RVALU,
     *   CVALU)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   finds an adverb in the POPS linked list of adverbs and sets its
C   value.
C   Inputs:
C      ANAME   C*(*)     Adverb name - left justified
C      ATYPE   C*1       'C' => convert hollerith to C in CVALU
C                        'I' -> convert reals to integer in IVALU
C                        'R' => copy real values to RVALU
C      ALENG   I         number of values
C      CLENG   I         string length in chars
C      IVALU   I(*)      Integer values 'I' only
C      RVALU   I(*)      Real values 'R' only
C      CVALU   C(*)*(*)  Character values 'C' only
C   Output in common:
C      ERRNUM =  2       not an adverb
C               72       type wrong
C               73       length wrong
C-----------------------------------------------------------------------
      CHARACTER ANAME*(*), ATYPE*1, CVALU(*)*(*)
      INTEGER   ALENG, CLENG, IVALU(*)
      REAL      RVALU(*)
C
      INTEGER   POTERR, ITAG, ILOC, ITYPE, ISIZE, I, IL, NUMC, NUMW
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DERR.INC'
      DATA PRGNAM /'ADVERB'/
C-----------------------------------------------------------------------
      POTERR = 0
C                                       get the adverb in the table
      KARBUF = ANAME
      KBPTR = 1
      CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 980
      ITAG = TAG
      ILOC = LOCSYM
      ITYPE = SYTYPE
C                                       real values
      IF (((ITYPE.EQ.1) .OR. (ITYPE.EQ.2)) .AND. ((ATYPE.EQ.'I') .OR.
     *   (ATYPE.EQ.'R'))) THEN
         ISIZE = 1
         IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
         IF (ISIZE.NE.ALENG) THEN
            POTERR = 72
         ELSE IF (ATYPE.EQ.'R') THEN
            CALL RCOPY (ISIZE, RVALU, C(ITAG))
         ELSE
            DO 10 I = 1,ISIZE
               C(ITAG+I-1) = IVALU(I)
 10            CONTINUE
         END IF
C                                       character values
      ELSE IF ((ITYPE.EQ.7) .AND. (ATYPE.EQ.'C')) THEN
         IL = K(ILOC+3)
         ISIZE = K(IL)
         NUMC  = K(IL+3)
         NUMW  = (NUMC + 3) / 4
         NUMW = MAX (1, NUMW)
         ISIZE = ISIZE / NUMW
         IF ((ISIZE.NE.ALENG) .OR. (NUMC.NE.CLENG)) THEN
            POTERR = 72
         ELSE
            DO 20 I = 1,ISIZE
               CALL CHR2H (NUMC, CVALU(I),  1, C(ITAG))
               ITAG = ITAG + NUMW
 20            CONTINUE
            END IF
C                                       type mismatch
      ELSE
         POTERR = 73
         END IF         

C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
