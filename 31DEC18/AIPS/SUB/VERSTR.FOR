      SUBROUTINE VERSTR (KPNT)
C-----------------------------------------------------------------------
C! Checks that a POPS character string has only valid characters.
C# POPS-util
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
C   VERSTR checks a POPS String adverb to verify that all characters are
C   valid.
C   Inputs:
C      KPNT     I   Pointer in POPS K array (DCON.INC) to symbol table
C                   entry for variable.
C   Output: (in common in DERR.INC
C      ERRNUM   I   Return code, 0=OK, else bad characters.
C-----------------------------------------------------------------------
      INTEGER   KPNT
C
      CHARACTER PRGNAM*6, CHTEMP*132, CHNAME*8
      INTEGER   ITYPE, ITAG,  LL, NDIM, NUMCHR, NWORD, NUMELE, I, J,
     *   IPNT, LENNAM, IERR, POTERR
      LOGICAL   EQUAL
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGNAM /'VERSTR'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get TAG and TYPE from K array
      LENNAM = K(KPNT+1) / 16
      ITYPE = K(KPNT+1) - 16*LENNAM
      ITAG = K(KPNT+2)
C                                       Only interested in character
C                                       strings.
      IF (ITYPE.NE.7) GO TO 999
C                                       Array?
      LL = K(KPNT+3)
      NDIM = K(LL+1)
      POTERR = 23
      IF (NDIM.LT.1) GO TO 990
C                                       NUMCHR = no. characters per
C                                       element.
      NUMCHR = K(LL+3)
      IF (NUMCHR.LE.0) GO TO 990
C                                       NWORD = no. words per element
      NWORD = (NUMCHR + 3) / 4
C                                       NUMELE = no. elements
      NUMELE = K(LL) / NWORD
C                                       Loop over elements
      DO 200 J = 1,NUMELE
         IPNT = ITAG + (J-1) * NWORD
         EQUAL = .TRUE.
C                                       Check if blank filled
         DO 100 I = 1,NWORD
            EQUAL = (EQUAL) .AND. (CH(IPNT+I-1).EQ.HBLANK)
 100        CONTINUE
C                                       Check validity
         IF (.NOT.EQUAL) THEN
            CALL H2CHR (NUMCHR, 1, CH(IPNT), CHTEMP)
            CALL CHKCHR (CHTEMP(1:NUMCHR), IERR)
            IF (IERR.NE.0) THEN
               POTERR = 9
               GO TO 990
               END IF
            END IF
 200     CONTINUE
      GO TO 999
C                                        Error
C                                       Symbol name
 990  CHNAME = '    '
      CALL H2CHR (LENNAM, 1, K(KPNT+4), CHNAME)
      WRITE (MSGTXT,1990) CHNAME
      CALL MSGWRT (7)
      IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('BAD CHARACTERS IN ADVERB ',A)
      END
