      SUBROUTINE EXTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN,
     *   EOF, UNKNWN, IERR)
C-----------------------------------------------------------------------
C! parse FITS tape record for required extension file FITS keywords
C# FITS tape EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2007
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
C   This routine will parse a block from a FITS tape and look for the
C   required cards of a FITS extension header block, namely XTENSION,
C   BITPIX, NAXIS, NAXISn, PCOUNT, GCOUNT.
C   Inputs:
C      FDVEC     I(50)   File descriptor for TAPIO input stream
C   Input/Output:
C      TBIND     I       Pointer in FITBLK
C      TAPBUF    I(*)    TAPIO i/o buffer
C      FITBLK    C*2880  a block of FITS header data.
C      UNKNWN    I       Number unknown records skipped looking for next
C                        extension
C   Outputs:
C      ICARD     I       The number of the last card parsed.
C      EXTEN     L       T means extension record, F means no.
C      EOF       L       T means end of file on 1st record.
C      IERR      I       0=ok, 1=messed up. An error message will
C                                be printed.
C-----------------------------------------------------------------------
      CHARACTER FITBLK*2880
      INTEGER   FDVEC(50), TBIND, TAPBUF(*), ICARD, UNKNWN, IERR
      LOGICAL   EXTEN, EOF
C
      DOUBLE PRECISION X
      CHARACTER EWORD(6)*8, STRING*80, CARD*80, SYMBOL*8, CVER*1
      INTEGER   IKEYWD, NPNT, ITABNO, NCHAR, I, NLEN, TERR, IFOUND,
     *   NFOUND
      LOGICAL   ATEND
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA EWORD /'XTENSION', 'BITPIX  ',  'NAXIS   ', 'PCOUNT  ',
     *   'GCOUNT  ', ' '/
C-----------------------------------------------------------------------
C                                       Read 1st block.
 10   CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
      IF (TERR.NE.0) THEN
         IF (TERR.EQ.4) GO TO 940
         WRITE (MSGTXT,1000)
         GO TO 980
         END IF
      EOF = .FALSE.
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
      IF (FITBLK(:8).NE.EWORD(1)) THEN
         IF (FITBLK(:7).EQ.'TABNAME') GO TO 950
         IF (FITBLK(:8).EQ.'SIMPLE  ') THEN
            MSGTXT = 'EXTREQ: NEW FITS FILE FOUND WHILE LOOKING FOR'
     *         // ' TABLE EXTENSION'
            CALL MSGWRT (8)
            GO TO 950
            END IF
         UNKNWN = UNKNWN + 1
         GO TO 10
         END IF
C                                       Look for XTENSION= 'type' card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      EXTEN = .TRUE.
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
      IF (IERR.NE.0) GO TO 950
      NLEN = 16
      CALL GETSTR (CARD, 80, NLEN, NPNT, STRING, NCHAR)
      EXTTYP = STRING(1:NCHAR)
C
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT,
     *   CARD, SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Get value of BITPIX
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.0) BITPIX = X + 0.1
      IF (X.LT.0.0) BITPIX = X - 0.1
C                                       Warning-Non standard bits/pixel
      IF ((BITPIX.EQ.8) .OR. (BITPIX.EQ.16) .OR. (BITPIX.EQ.32) .OR.
     *   (BITPIX.EQ.-32)) GO TO 20
         WRITE (MSGTXT,1010) BITPIX
         CALL MSGWRT (5)
C                                       Get NAXIS
 20   ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C                                       Check for invalid no. of axis
C                                       for our buffer.
      IF (NAXIS.GT.50) GO TO 960
C                                       Find NAXISi
      DO 50 I = 1,NAXIS
         ICARD = ICARD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         STRING = ' '
         IF (I.LT.10) THEN
            WRITE (STRING,1020) I
         ELSE
            WRITE (STRING,1030) I
            END IF
         CALL GTWCRD (ICARD, 1, 0, STRING, FITBLK, NPNT, CARD,
     *      SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
         IF (IERR.NE.0) THEN
            IKEYWD = 6
            EWORD(IKEYWD) = STRING(1:8)
            GO TO 970
            END IF
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         NAXISI(I) = X + .01
 50      CONTINUE
C                                       PCOUNT
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      PCOUNT = X + 0.01
C                                       Get GCOUNT
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GTWCRD (ICARD, 1, 0, EWORD(IKEYWD), FITBLK, NPNT, CARD,
     *   SYMBOL, ITABNO, NFOUND, IFOUND, CVER, ATEND, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      GCOUNT = X + 0.01
      GO TO 999
C                                       End of file.
 940  IERR = 0
      EOF = .TRUE.
      GO TO 999
C                                       Not extension record.
 950  IERR = 0
      EXTEN = .FALSE.
      GO TO 999
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) EWORD(IKEYWD), SYMBOL
      GO TO 980
C                                       number out of range
 975  MSGTXT = 'EXTREQ: VALUE OUT OF RANGE ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING FITS TABLE')
 1010 FORMAT ('INVALID TABLE BITS PER PIXEL =',I6)
 1020 FORMAT ('NAXIS',I1)
 1030 FORMAT ('NAXIS',I2)
 1960 FORMAT ('NUMBER OF TABLE AXIS TOO LARGE FOR BUFFER =',I6)
 1970 FORMAT ('EXPECTED TABLE KEYWORD ',A8,'. FOUND ',A8,'.')
      END
