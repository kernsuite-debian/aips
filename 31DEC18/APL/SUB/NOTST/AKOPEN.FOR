      INTEGER FUNCTION AKOPEN (FIL, BNUM, ATYP, BUFFER)
C-----------------------------------------------------------------------
C! Tim Cornwell I/O routine to open files.
C# IO-util
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
C   AKOPEN opens and initializes a scratch file using ZOPEN and MINIT
C   On READing the windows are set so that only the required data is
C   passed. On WRITing, the entire image is windowed.
C   Programmer =  T.J. Cornwell                            December 1987
C-----------------------------------------------------------------------
      CHARACTER ATYP*4
      INTEGER   FIL, BNUM, LUN, IARG, IERR
      REAL      BUFFER(*)
      INTEGER   WINR(4), WINW(4)
      LOGICAL   MAP, EXCL, WAIT
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Buffer sizes
      BUFSZ(1) = 2 * XBUFSZ
      BUFSZ(2) = 2 * XBUFSZ
      BUFSZ(3) = 2 * XBUFSZ
      BUFSZ(4) = 2 * XBUFSZ
      BUFSZ(5) = 2 * XBUFSZ
      BUFSZ(6) = 2 * XBUFSZ
      ACCTYP(FIL) = ATYP
      BUFNUM(FIL) = BNUM
      IARG = BUFNUM(FIL)
      LUN = BUFNUM(FIL) + 15
      AKOPEN = 0
      CALL ZOPEN (LUN, FIND(IARG), VMVOL(FIL), VMFILE(FIL), MAP,
     *   EXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 100
         WRITE (MSGTXT,1000) IERR,FIL
         CALL MSGWRT (8)
         AKOPEN = IERR
         GO TO 999
 100  IF (ACCTYP(FIL).NE.'READ') GO TO 110
C                                       Reading whole image file
         IF ((VMSZ(1,FIL).EQ.NX).AND.(VMSZ(2,FIL).EQ.NY)) THEN
            WINR(1) = BLC(1)
            WINR(2) = BLC(2)
            WINR(3) = TRC(1)
            WINR(4) = TRC(2)
C                                       Reading half image file
         ELSE IF ((VMSZ(1,FIL).EQ.HNX).AND.(VMSZ(2,FIL).EQ.HNY)) THEN
            WINR(1) = XBEG
            WINR(2) = YBEG
            WINR(3) = XEND
            WINR(4) = YEND
C                                       Reading FT image file
         ELSE IF ((VMSZ(1,FIL).EQ.2*NY).AND.
     1            (VMSZ(2,FIL).EQ.NX/2+1)) THEN
            WINR(1) = 1
            WINR(2) = 1
            WINR(3) = VMSZ(1,FIL)
            WINR(4) = VMSZ(2,FIL)
         ELSE
C                                       Unknown image: cannot window
            IERR = 20
            GO TO 130
         END IF
         CALL MINIT ('READ', LUN, FIND(IARG), VMSZ(1,FIL), VMSZ(2,FIL),
     *      WINR, BUFFER, BUFSZ(IARG), VMBO(FIL), IERR)
         GO TO 130
 110  IF (ACCTYP(FIL).NE.'WRIT') GO TO 120
         WINW(1) = 1
         WINW(2) = 1
         WINW(3) = VMSZ(1,FIL)
         WINW(4) = VMSZ(2,FIL)
         CALL MINIT ('WRIT', LUN, FIND(IARG), VMSZ(1,FIL), VMSZ(2,FIL),
     *      WINW, BUFFER, BUFSZ(IARG), VMBO(FIL), IERR)
         GO TO 130
 120  IERR = 10
 130  IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1010) IERR, FIL
         CALL MSGWRT (8)
         AKOPEN = IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,' OPENING FILE ',I2,' IN OPEN ROUTINE:')
 1010 FORMAT ('ERROR ',I3,' INITIALIZING FILE ',I2,' IN OPEN ROUTINE:')
      END
