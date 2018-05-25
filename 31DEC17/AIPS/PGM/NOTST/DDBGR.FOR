      PROGRAM DDBGR
C-----------------------------------------------------------------------
C! Looks at disk file contents
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 2006, 2017
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
C   A service routine to debug disk contents
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, MSGBUF*80, PRGNAM*6, ASTR*4, HSTR*8
      INTEGER   IVOL, IBUF(256), TTYLUN, TTYIND, IERR, HLUN, HIND, IREC,
     *   TTY(2), IDAT(3), NC, JTRIM, I, J
      REAL      RBUF(256)
      HOLLERITH HBUF(256)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IREC, IDAT(1))
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      EQUIVALENCE (IBUF, RBUF, HBUF)
      DATA PRGNAM /'DDBGR '/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN, HLUN /5, 16/
C-----------------------------------------------------------------------
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGTXT = 'I look at disk file contents'
      CALL MSGWRT (2)
C                                       Loop to ask for vol, cat #s
 10   MSGBUF = 'Enter desired file name - QUIT to stop'
      CALL INQSTR (TTY, MSGBUF, 48, PHNAME, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 995
      IF (PHNAME.EQ.'QUIT') GO TO 995
      IVOL = 1
      IF (PHNAME(:2).EQ.'DA') CALL ZHEX10 (PHNAME(3:4), IVOL, IERR)
C                                       open the file
      CALL ZOPEN (HLUN, HIND, IVOL, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         NC = JTRIM (PHNAME)
         WRITE (MSGTXT,1010) IERR, PHNAME(:NC)
         CALL MSGWRT (8)
         IF (IERR.NE.2) GO TO 995
         GO TO 10
         END IF
 20   MSGBUF = 'Enter desired record number (0 -> enough) then word' //
     *   ' range'
      CALL INQINT (TTY, MSGBUF, 3, IDAT, IERR)
      IF ((IREC.GT.0) .AND. (IERR.EQ.0)) THEN
         CALL ZFIO ('READ', HLUN, HIND, IREC, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, IREC
            CALL MSGWRT (8)
            GO TO 20
            END IF
         IF (IDAT(2).LE.0) IDAT(2) = 1
         IF (IDAT(3).LE.0) IDAT(3) = 256
         IF (IDAT(3).LT.IDAT(2)) THEN
            IDAT(2) = 1
            IDAT(3) = 256
            END IF
         DO 30 I = IDAT(2),IDAT(3)
            CALL ZHEX (IBUF(I), 8, HSTR)
            CALL H2CHR (4, 1, HBUF(I), ASTR)
            NC = JTRIM (ASTR)
            J = (IREC-1) * 256 + I
            WRITE (MSGTXT,1025) I, J, IBUF(I), RBUF(I), HSTR, ASTR(:NC)
            CALL MSGWRT (5)
 30         CONTINUE
         GO TO 20
         END IF
C                                       close data file and loop
 90   CALL ZCLOSE (HLUN, HIND, IERR)
      GO TO 10
C                                       Terminal error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 995  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      NLUSER = 1
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' OPENING ''',A,'''')
 1020 FORMAT ('ERROR',I3,' READING RECORD',I6)
 1025 FORMAT (I3,I8,I14,1PE15.6,2X,A8,'  ''',A,'''')
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
      INTEGER FUNCTION JTRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it blanks all of the
C   STRING following the first non-printable character (< ' '), then
C   trims trailing blanks.  Use with calls like:
C         TRIMMED = GROSS(1:JTRIM(GROSS))
C   This is safer than ITRIM.
C   In/out:
C      STRING   C*(*)   String to be cleared of nulls, length found
C   Output:
C      JTRIM    I       Length to last non-blank of cleared string
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INTEGER   ILEN, I
      CHARACTER TABCHR
C-----------------------------------------------------------------------
      ILEN = LEN (STRING)
      CALL ZTAB (TABCHR)
C                                       check forwards for null
      I = 0
10    CONTINUE
         I = I + 1
         IF (I.GT.ILEN) GO TO 20
         IF (STRING(I:I).EQ.TABCHR) GO TO 10
         IF ((STRING(I:I).GE.' ') .AND. (STRING(I:I).LE.'~')) GO TO 10
         STRING(I:ILEN) = ' '
20    JTRIM = I
C                                       look backwards for non-blank
 30   CONTINUE
         JTRIM = JTRIM - 1
         IF (JTRIM.LT.1) GO TO 999
         IF (STRING(JTRIM:JTRIM).EQ.' ') GO TO 30
         IF (STRING(JTRIM:JTRIM).EQ.TABCHR) GO TO 30
C
 999  RETURN
      END
