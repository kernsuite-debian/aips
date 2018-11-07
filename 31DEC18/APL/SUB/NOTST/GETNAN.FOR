      SUBROUTINE GETNAN (DISK, CNO, CATBLK, LUN, BUFFER, NUMAN, IRET)
C-----------------------------------------------------------------------
C! Find number of antennas and subarrays from AN tables.
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2009, 2016
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
C   Determines the number of subarrays in a data set from the number
C   of AN files and returns the highest antennas number in each subarray
C   If no antennas are found, one subarray with 28 antennas assumed.
C   If an error occurs, information about subarrays from AN files found
C   is returned; although an error code is returned.
C   Inputs:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      CATBLK   I(256)   Catalog header block.
C      LUN      I        Logical unit number to use
C      BUFFER   I(512)   I/O buffer and related storage.
C   Output:
C      NUMAN    I(513)   1st element = no. subarrays followed by
C                        the highest antenna number in each subarray.
C      IRET     I        Return error code, 0 => ok,
C                           else TABINI or TABIO error.
C                           10 = no AN files.
C-----------------------------------------------------------------------
      INTEGER   BUFFER(512), DISK, CNO, CATBLK(256), LUN, NUMAN(*),
     *   IRET
C
      INTEGER   VER, NSUBA, I, NBUFF, II, NUMREC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
C-----------------------------------------------------------------------
C                                       Set default results.
      NUMAN(1) = 1
      NUMAN(2) = 28
C                                       Check CATBLK for AN files.
      NBUFF = 1024
      CALL FNDEXT ('AN', CATBLK, NSUBA)
C                                       No AN files found.
      IF (NSUBA.LE.0) THEN
         IRET = 10
         MSGTXT = 'GETNAN: NO AN FILES FOUND'
         IF (TYPUVD.LE.0) GO TO 990
         GO TO 999
         END IF
C                                       Loop over AN files
 30   DO 200 I = 1,NSUBA
         VER = I
C                                       Open file
         CALL ANTINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, 'READ', VER
            GO TO 990
            END IF
C                                       Get number of antennas in
C                                       subarray.
         NUMREC = BUFFER(5)
         NUMAN(I+1) = 1
         DO 100 II = 1,NUMREC
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) GO TO 150
            NUMAN(I+1) = MAX (NUMAN(I+1), NOSTA)
 100        CONTINUE
C                                       keep number found.
         NUMAN(1) = I
C                                       Close
 150     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IRET)
         IF (IRET.EQ.0) GO TO 200
            WRITE (MSGTXT,1030) IRET, 'CLOS', VER
            GO TO 990
 200     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('GETNAN: ERROR',I3,1X,A4,'ING AN FILE ',I5)
      END
