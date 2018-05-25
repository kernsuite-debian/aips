      SUBROUTINE TPHEAD (FDVEC, NWORD, FITS, TBIND, TAPBUF, IERR)
C-----------------------------------------------------------------------
C! reads a tape record, advances over label file, decides if it's FITS
C# Tape
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
C   TPHEAD reads a tape header into TAPBUF.  It tests it for being a
C   FITS header by looking for 'SIMPLE  = '.  If that fails it checks
C   for tape label marks 'VOL1', 'HDR1', etc.  If they are found it
C   advances file and repeats. If not, it tests for an IBM "DECMAP"
C   header by checking the axis dimensions.
C   Inputs:
C      FDVEC   I(50)     TAPIO IO control data
C      NWORD   I         Number of words to read
C      FITS    I         if = 1 and NWORD = 1440, then caller is 
C                        expecting a FITS header record other than the
C                        1st
C   Outputs:
C      FITS    I         1 => it is the first record of a FITS header
C                        -1 => "FITS" but SIMPLE = F or bad length
C                        2 => IBM MAP, 3 => Export format UV data
C      TAPBUF  I(NWORD)  Buffer to receive header
C      IERR    I         error return: 0 = O.K.
C                           Other = return from MINI3 or MDISK
C                           10    = neither FITS nor DEC nor Export
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), NWORD, FITS, TBIND, TAPBUF(*), IERR
C
      CHARACTER CDATA*80, HDRTYP(7)*4
      INTEGER   IEREOF, IEREOM, I(2), IT, LUN, TIND, IFITS, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA IEREOF, IEREOM /4,6/
      DATA HDRTYP /'VOL1','HDR1','HDR2','EOF1','EOF2','EOV1','EOV2'/
C-----------------------------------------------------------------------
C                                       read one record, allow more
      IFITS = FITS
      FDVEC(31) = 0
      FDVEC(32) = 0
      LUN = FDVEC(1)
      TIND = FDVEC(40)
C                                       suppress messages
 10   MSGSUP = 32000
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      MSGSUP = 0
      IF ((IERR.NE.0) .AND. (IERR.NE.10)) GO TO 980
C                                       FITS ?
      CALL ZC8CL (80, 1, TAPBUF(TBIND), CDATA)
      IF (CDATA(:10).EQ.'SIMPLE  = ') THEN
         FITS = 1
         IF (IERR.EQ.10) THEN
            IERR = 0
            FITS = -1
         ELSE
            IT = INDEX (CDATA, '/')
            IF (IT.LE.0) IT = 80
            DO 15 J = 11,IT
               IF (CDATA(J:J).NE.' ') THEN
                  IF (CDATA(J:J).NE.'T') FITS = -1
                  GO TO 970
                  END IF
 15            CONTINUE
            FITS = -1
            END IF
         GO TO 970
         END IF
      IERR = 0
C                                       Check for labeled tape
      DO 20 IT = 1,7
         IF (CDATA(1:4).EQ.HDRTYP(IT)) GO TO 30
 20      CONTINUE
C                                       Must be an IBM header
C                                       Check for Export tape
      CALL ZI16IL (1, 1, TAPBUF(TBIND), I)
      IF ((I(1).LE.1) .OR. (I(1).GT.4008)) GO TO 25
         FITS = 3
         CALL ZC8CL (4, 9, TAPBUF(TBIND), CDATA)
         IF (CDATA(1:4).EQ.'RUN ') GO TO 999
         IF (CDATA(1:4).EQ.'DEF ') GO TO 999
C                                       check bound on DEC dims
 25   FITS = 2
      CALL ZI16IL (2, 17, TAPBUF(TBIND), I)
      IF ((I(1).GE.16) .AND. (I(2).GE.16) .AND. (I(1).LE.4096) .AND.
     *   (I(2).LE.4096)) GO TO 999
         FITS = IFITS
         IF ((NWORD.EQ.1440) .AND. (IFITS.EQ.1)) GO TO 999
         IERR = 10
         MSGTXT = 'TPHEAD: NOT FITS, IBM-MAP, OR UV-EXPORT HEADER'
         GO TO 990
C                                       EOVolume label
 30   IF (IT.GE.6) THEN
         MSGTXT = 'END OF MULTI-TAPE VOLUME FOUND.  MOUNT NEXT TAPE'
         IERR = 6
C                                       Skip label file & retry
      ELSE
         WRITE (MSGTXT,1040) HDRTYP(IT)
         CALL MSGWRT (2)
         DO 45 J = 1,6
            MSGSUP = 32000
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            MSGSUP = 0
            IF (IERR.EQ.IEREOF) GO TO 10
            IF ((IERR.NE.0) .AND. (IERR.NE.10)) GO TO 980
 45         CONTINUE
         MSGTXT = 'TAPE LABEL FILE TOO LONG - ERROR'
         IERR = 10
         END IF
      GO TO 990
C                                        errors
 970  IF (FITS.LT.0) THEN
         WRITE (MSGTXT,1970) FDVEC(42)
         GO TO 990
         END IF
      GO TO 999
 980  IF (IERR.EQ.IEREOF) THEN
         MSGTXT = 'TPHEAD:  END-OF-FILE = END-OF-INFORMATION?'
      ELSE IF (IERR.EQ.IEREOM) THEN
         MSGTXT = 'TPHEAD:  END-OF-INFORMATION'
      ELSE
         WRITE (MSGTXT,1980) IERR
         END IF
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Skipping tape label file of type ',A)
 1970 FORMAT ('NON-STANDARD FITS HEADER OF LENGTH',I7,' BYTES READ')
 1980 FORMAT ('TPHEAD: I/O ERROR = ',I6)
      END
