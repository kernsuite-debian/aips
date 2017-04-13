      SUBROUTINE UVPREP (OPCODE, IODISK, ICNOSC, INLUN, INDSK, NVISIN,
     *   LRECIN, ILENBU, BUFSZ, BUFFER, NIO, IOPTR, MAXBLN, IRET)
C-----------------------------------------------------------------------
C! Prepares UV file for read/write and gets Maximum U on read
C# UV Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2005, 2008
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
C   UVPREP gets either the maximum U from an open database if the BLMAX
C   parameter is not set.
C   Inputs:
C      OPCODE   C*4     Opcode 'READ' or 'WRIT',
C                       'REED' no read for MAXBLN
C      IODISK   I       Input/Output file disk number for cataloged
C                       files, (.LE. 0 => /CFILES/ scratch file.)
C      ICNOSC   I       Input file catalog slot number or /CFILES/
C                       scratch file number.
C      INLUN    I       Logical Unit for I/O
C      INDSK    I       Aips Disk number of UV data
C   Input/Output:
C      NVISIN   I       Number of visibilities
C      LRECIN   I       Number of values in a visibility record.
C      ILENBU   I       Number of visibilities per call to UVDISK.
C                       Determines block size for tape I/O
C                       0 => decide (see note above)
C      BUFSZ    I       Size in bytes of the buffer.
C      BUFFER   R(*)    Buffer
C      NIO      I       No. visibilities read/write.
C                       Max. no. vis. for next write.
C      IOPTR    I       Pointer to start of data in buffer
C      MAXBLN   R       Maximum baseline in database (wavelengths)
C   Output:
C      IRET     I       Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      CHARACTER*4 OPCODE
      INTEGER     IODISK, ICNOSC, INLUN, INDSK, NVISIN, LRECIN,
     *   ILENBU, BUFSZ, NIO, IOPTR, IRET
      REAL        BUFFER(*), MAXBLN
C
      CHARACTER   NAME*48
      INTEGER     VO, BO, VOL, LOCS(2), VALUES(2), KEYTYP(2), IERR,
     *   CATBUF(256), I, INPTR, CNO
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE VO, BO, T, F
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (NVISIN.LE.0) THEN
         MSGTXT = 'UVPREP: NUMBER VISIBILITIES REQUESTED <= 0'
         IRET = 2
         GO TO 995
         END IF
C                                       use scratch file list
      IF (IODISK.LE.0) THEN
         VOL = SCRVOL(ICNOSC)
         CNO = SCRCNO(ICNOSC)
         CALL ZPHFIL ('SC', VOL, CNO, 1, NAME, IRET)
C                                       using an actual file name
      ELSE
         VOL = IODISK
         CNO = ICNOSC
         CALL ZPHFIL ('UV', VOL, CNO, 1, NAME, IRET)
         END IF
      CALL ZOPEN (INLUN, INDSK, VOL, NAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) 'OPEN', IRET
         GO TO 995
         END IF
C                                       Init UV file read.
      IF (OPCODE(:4).EQ.'REED') THEN
         CALL UVINIT ('READ', INLUN, INDSK, NVISIN, VO, LRECIN, ILENBU,
     *      BUFSZ, BUFFER, BO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', IRET
            GO TO 995
            END IF
C                                       Read first visibility record
         CALL UVDISK ('READ', INLUN, INDSK, BUFFER, NIO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IRET
            GO TO 995
            END IF
C                                       Init UV file read.: check MAXBLN
      ELSE IF (OPCODE(:4).EQ.'READ') THEN
         CALL UVINIT ('READ', INLUN, INDSK, NVISIN, VO, LRECIN, ILENBU,
     *      BUFSZ, BUFFER, BO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', IRET
            GO TO 995
            END IF
C                                       Get Max U from header
         CALL CATKEY ('REED', VOL, CNO, 'MAXBLINE', 1, LOCS, VALUES,
     *      KEYTYP, CATBUF, IERR)
C                                       copy value from buffer
         IF ((IERR.EQ.0) .AND. (LOCS(1).GT.0)) THEN
            CALL RCOPY (1, VALUES(LOCS(1)), MAXBLN)
C                                       Else MAXBLINE not set, find it
         ELSE
            MAXBLN = 0.
C                                       Loop Reading until all read
 20         CALL UVDISK ('READ', INLUN, INDSK, BUFFER, NIO, IOPTR, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'READ', IRET
               GO TO 995
               END IF
            INPTR = IOPTR
C                                       Read all vis in buffer
            IF (NIO.GT.0) THEN
               DO 50 I = 1,NIO
                  MAXBLN = MAX (MAXBLN, ABS(BUFFER(INPTR+ILOCU)))
                  INPTR = INPTR + LRECIN
 50               CONTINUE
               GO TO 20
               END IF
C                                       Tell User Max baseline found
            WRITE (MSGTXT,1050) MAXBLN
            CALL MSGWRT (3)
C                                       Record Max Baseline in header
            IF (MAXBLN.GT.0) THEN
               LOCS(1) = 1
               CALL RCOPY (1, MAXBLN, VALUES(1))
               CALL CATKEY ('WRIT', VOL, CNO, 'MAXBLINE', 1, LOCS,
     *            VALUES, 2, CATBUF, IERR)
               END IF
C                                       Re-Init UV file read.
            CALL UVINIT ('READ', INLUN, INDSK, NVISIN, VO, LRECIN,
     *         ILENBU, BUFSZ, BUFFER, BO, IOPTR, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'INIT', IRET
               GO TO 995
               END IF
            END IF
C                                       Read first visibility record
         CALL UVDISK ('READ', INLUN, INDSK, BUFFER, NIO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IRET
            GO TO 995
            END IF
C                                       else init for write
      ELSE
         CALL UVINIT ('WRIT', INLUN, INDSK, NVISIN, VO, LRECIN, ILENBU,
     *      BUFSZ, BUFFER, BO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', IRET
            GO TO 995
            END IF
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPREP: TROUBLE ',A,'ING UV DATA FILE,  IER=',I5)
 1050 FORMAT ('UVPREP: Maximum U Baseline is ',1PE10.3,' lambda.')
      END

