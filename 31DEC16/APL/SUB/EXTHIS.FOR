      SUBROUTINE EXTHIS (HLUN, HBUFF, IERR)
C-----------------------------------------------------------------------
C! adds to history file for contents of FITS extension file being read
C# FITS Tape History EXT-appl
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
C   Add history records for FITS extension table.  Starts with whatever
C   is in MSGTXT.
C   Inputs:
C      HLUN     I        LUN for open history file.
C      HBUFF    I        I/O buffer.
C      /MSGCOM/ Common   Contains 1st line to put in history file.
C      /EXTHDR/ Common   Extension file info.
C      /TBHDR/  Common   Table extension file info.
C   Output:
C      IERR     I        Error code. 0 = ok.  else HI error
C-----------------------------------------------------------------------
      INTEGER   HLUN, HBUFF(256), IERR
C
      CHARACTER HILINE*72
      INTEGER   I, IL, ITRIM
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       First line is whatever is in
C                                       MSGTXT.
      HILINE = MSGTXT
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IL = ITRIM(EXTTYP)
      IL = MAX(8, IL)
      WRITE (HILINE,1010) TSKNAM, EXTTYP(1:IL)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1020) TSKNAM, NAXISI(2)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1030) TSKNAM, ITNCOL
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IL = ITRIM(EXTNAM)
      IL = MAX(8, IL)
      WRITE (HILINE,1040) TSKNAM, EXTNAM(1:IL)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1050) TSKNAM, EXTVER
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1060) TSKNAM, EXTLEV
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 20 I = 1,ITNCOL
         IF (I.LT.10) THEN
            IL = ITRIM(TTYPE(I))
            IL = MAX(8, IL)
            WRITE (HILINE,1070) TSKNAM, I, TTYPE(I)(1:IL)
            CALL HIADD (HLUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            IL = ITRIM(TUNIT(I))
            IL = MAX(8, IL)
            WRITE (HILINE,1080) TSKNAM, I, TUNIT(I)(1:IL)
            CALL HIADD (HLUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            IL = ITRIM(TTYPE(I))
            IL = MAX(8, IL)
            WRITE (HILINE,1100) TSKNAM, I, TTYPE(I)(1:IL)
            CALL HIADD (HLUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            IL = ITRIM(TUNIT(I))
            IL = MAX(8, IL)
            WRITE (HILINE,1110) TSKNAM, I, TUNIT(I)(1:IL)
            CALL HIADD (HLUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A6,' XTENSION= ''',A,'''')
 1020 FORMAT (A6,' NAXIS2  =',12X,I8)
 1030 FORMAT (A6,' TFIELDS =',12X,I8)
 1040 FORMAT (A6,' EXTNAME = ''',A,'''')
 1050 FORMAT (A6,' EXTVER  =',12X,I8)
 1060 FORMAT (A6,' EXTLEV  =',12X,I8)
 1070 FORMAT (A6,' TTYPE',I1,'  = ''',A,'''')
 1080 FORMAT (A6,' TUNIT',I1,'  = ''',A,'''')
 1100 FORMAT (A6,' TTYPE',I2,' = ''',A,'''')
 1110 FORMAT (A6,' TUNIT',I2,' = ''',A,'''')
      END
