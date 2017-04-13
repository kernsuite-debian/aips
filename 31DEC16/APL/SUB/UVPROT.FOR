      SUBROUTINE UVPROT (OPCODE, IODISK, ICNOSC, INLUN, INDSK, MFIELD,
     *   UMATS, CCROT, SSROT, NVISIN, LRECIN, ILENBU, BUFSZ, BUFFER,
     *   NIO, IOPTR, MAXBLN, DISKO, CNOOUT, IRET)
C-----------------------------------------------------------------------
C! Prepares UV file for read/write, computes maximum U on read
C# UV Util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2000, 2008-2010
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
C   UVPROT gets either the maximum U from an open database even if the
C   BLMAX parameter is set applying 3D or regular rotation.
C   It does not read the data set if MAXABSU appears in the header as a
C   keyword.  This should only be set for temporary files by imaging
C   tasks that have determined the max (abs (u)) over all rotations and
C   reprojections.
C   Inputs:
C      OPCODE   C*4     Opcode 'READ' or 'WRIT'
C      IODISK   I       Input/Output file disk number for cataloged
C                       files, (.LE. 0 => /CFILES/ scratch file.)
C      ICNOSC   I       Input file catalog slot number or /CFILES/
C                       scratch file number.
C      INLUN    I       Logical Unit for I/O
C      INDSK    I       Aips Disk number of UV data
C      MFIELD   I       Number of fields to check
C      UMATS    R(3,3,*)  3D rotation matrix
C      CCROT    R       Cos of rotation
C      SSROT    R       Sin of rotation
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
     *   ILENBU, BUFSZ, NIO, IOPTR, MFIELD, CNOOUT, DISKO, IRET
      REAL        BUFFER(*), MAXBLN, UMATS(3,3,*), CCROT, SSROT
C
      CHARACTER   NAME*48
      INTEGER     VO, BO, VOL, LOCS(2), VALUES(2), IERR, CATBUF(256), I,
     *   INPTR, CNO, KEYTYP(2), IFIELD, VOLO, CNOO
      LOGICAL     T, F, DOROT
      REAL        UU, MAXINU, UVWROT(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      SAVE VO, BO, T, F
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      DOROT = (ABS (SSROT).GT.1.0E-10) .OR. (ABS (CCROT-1.0).GT.1.0E-4)
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
      IF (DISKO.LE.0) THEN
         VOLO = SCRVOL(CNOOUT)
         CNOO = SCRCNO(CNOOUT)
      ELSE
         VOLO = DISKO
         CNOO = CNOOUT
         END IF
      CALL ZOPEN (INLUN, INDSK, VOL, NAME, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) 'OPEN', IRET
         GO TO 995
         END IF
C                                       reading old, or writing new ?
      IF (OPCODE(1:4).EQ.'READ') THEN
C                                       Init UV file read.
         CALL UVINIT ('READ', INLUN, INDSK, NVISIN, VO, LRECIN, ILENBU,
     *      BUFSZ, BUFFER, BO, IOPTR, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'INIT', IRET
            GO TO 995
            END IF
C                                       Get Max U from header
         CALL CATKEY ('REED', VOL, CNO, 'MAXABSU ', 1, LOCS, VALUES,
     *      KEYTYP, CATBUF, IERR)
C                                       copy value from buffer
         MAXBLN = 0.0
         IF ((IERR.EQ.0) .AND. (LOCS(1).GT.0))
     *      CALL RCOPY (1, VALUES(LOCS(1)), MAXBLN)
C                                       Else MAXBLINE not set, find it
C                                       Recompute MAXBLINE
         IF (MAXBLN.LE.0.0) THEN
            MAXBLN = 0.0
            MAXINU = 0.0
C                                       Get current Max U from header
            CALL CATKEY ('REED', VOL, CNO, 'MAXBLINE', 1, LOCS, VALUES,
     *         KEYTYP, CATBUF, IERR)
C                                       copy value from buffer
            IF ((IERR.EQ.0) .AND. (LOCS(1).GT.0)) CALL RCOPY (1,
     *         VALUES(LOCS(1)), MAXBLN)
C                                       Loop Reading until all read
 20         CALL UVDISK ('READ', INLUN, INDSK, BUFFER, NIO, IOPTR, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) 'READ', IRET
               GO TO 995
               END IF
            INPTR = IOPTR
C                                       Jump out on end of data
            IF (NIO.GT.0) THEN
               DO 30 I = 1,NIO
                  UU = ABS (BUFFER(INPTR+ILOCU))
                  MAXINU = MAX (MAXINU, UU)
                  DO 25 IFIELD = 1,MFIELD
                     CALL PRJMUL (1, BUFFER(INPTR+ILOCU),
     *                  UMATS(1,1,IFIELD), UVWROT)
                     UU = ABS (UVWROT(1))
                     MAXBLN = MAX (MAXBLN, UU)
 25                  CONTINUE
                  INPTR = INPTR + LRECIN
 30               CONTINUE
C                                       End of buffer, go get next
               GO TO 20
               END IF
C                                       Tell User Max baseline found
            WRITE (MSGTXT,1050) MAXINU
            CALL MSGWRT (3)
            WRITE (MSGTXT,1051) MAXBLN
            CALL MSGWRT (3)
            IF (MAXBLN.LE.0.9*MAXINU) MAXBLN = MAXINU
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
C                                       if max is set, record
         IF (MAXBLN.GT.0) THEN
            LOCS(1) = 1
            CALL RCOPY (1, MAXBLN, VALUES(1))
C                                       Record Max Baseline in header
            CALL CATKEY ('WRIT', VOL, CNO, 'MAXBLINE', 1, LOCS,
     *         VALUES, 2, CATBUF, IERR)
            CALL CATKEY ('WRIT', VOL, CNO, 'MAXABSU', 1, LOCS,
     *         VALUES, 2, CATBUF, IERR)
            IF ((VOL.NE.VOLO) .OR. (CNO.NE.CNOO)) THEN
               CALL CATKEY ('WRIT', VOLO, CNOO, 'MAXBLINE', 1, LOCS,
     *            VALUES, 2, CATBUF, IERR)
               CALL CATKEY ('WRIT', VOLO, CNOO, 'MAXABSU', 1, LOCS,
     *            VALUES, 2, CATBUF, IERR)
               END IF
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
 1000 FORMAT ('UVPROT: TROUBLE ',A,'ING UV DATA FILE,  IER=',I5)
 1050 FORMAT ('UVPROT: Maximum input   U Baseline is ',1PE12.5,
     *   ' lambda')
 1051 FORMAT ('UVPROT: Maximum rotated U Baseline is ',1PE12.5,
     *   ' lambda')
      END
