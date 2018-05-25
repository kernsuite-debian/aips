      SUBROUTINE CALCOP (DISK, CNOSCR, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C! Copies selected uv data with calibration and editing
C# UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.  The input
C   file should have been opened with UVGET.  Both files will be closed
C   on return from CALCOP.  Note: UVGET returns the information
C   necessary to catalog the output file.  The output file will be
C   compressed if necessary at completion of CALCOP.
C   Inputs:
C      DISK     I       Disk number for cataloged output file.
C                       If .LE. 0 then the output file is a /CFILES/
C                       scratch file.
C      BUFSZ    I       Size of BUFFER in bytes.
C   Input via common:
C      LREC     I       (/UVHDR/) length of vis. record in R words.
C      NRPARM   I       (/UVHDR/) number of R random parameters.
C   In/out:
C      CNOSCR   I       Catalog slot number for if cataloged file;
C                       /CFILES/ scratch file number if a scratch file,
C                       IF DISK=CNOSCR=0 then the scratch is created.
C                       On output = Scratch file number if created.
C   In/out via common:
C      CATBLK   I(256)  Catalog header block from UVGET
C                       on output with actual no. records
C      NVIS     I       (/UVHDR/) Number of vis. records.
C   Output:
C      BUFFER   R(*)    Work buffer for writing.
C      IRET     I       Error code: 0 => OK,
C                          > 0 => failed, abort process.
C   Usage notes:
C   (1) UVGET with OPCODE='INIT' MUST be called before CALCOP to setup
C       for calibration, editing and data translation.  If an output
C       cataloged file is to be created this should be done after the
C       call to UVGET.
C   (2) Uses AIPS LUN 24
C-----------------------------------------------------------------------
      INTEGER   DISK, CNOSCR, BUFSZ, IRET
      REAL      BUFFER(*)
C
      CHARACTER NAME*48
      INTEGER   VOL, LUN, FIND, BIND, LENBU, NIO, CATBLK(256), CNO, BO,
     *   VO, I, XCOUNT, ISIZE, ASIZE
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO, VO /24,1,0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
C                                       Determine size.
      CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create output file if necessary
      IF ((DISK.LE.0) .AND. (CNOSCR.LE.0)) THEN
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) THEN
               MSGTXT = 'CALCOP: TOO LITTLE DISK SPACE FOR SCRATCH FILE'
            ELSE
               WRITE (MSGTXT,1000) IRET, 'CREATING SCRATCH FILE'
               END IF
            GO TO 990
            END IF
C                                       Update CATBLK: ignore error
         CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *      'REST', BUFFER, IRET)
         END IF
C                                       Set output file name.
 10   IF (DISK.GT.0) THEN
         VOL = DISK
         CNO = CNOSCR
         CALL ZPHFIL ('UV', VOL, CNO, 1, NAME, IRET)
      ELSE
         VOL = SCRVOL(CNOSCR)
         CNO = SCRCNO(CNOSCR)
         CALL ZPHFIL ('SC', VOL, CNO, 1, NAME, IRET)
         END IF
C                                       Check file size
      CALL ZEXIST (VOL, NAME, ASIZE, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'CALCOP: FILE MISSING ' // NAME(:42)
         ELSE
            WRITE (MSGTXT,1000) IRET, 'SEEING IF FILE EXISTS'
            END IF
         GO TO 990
         END IF
      IF (ASIZE.LT.ISIZE) THEN
         MSGTXT = 'CALCOP: FILE TOO SMALL ' // NAME(:40)
         IRET = 1
         IF (DISK.GT.0) GO TO 990
         CALL MSGWRT (6)
         CALL ZDESTR (VOL, NAME, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DESTROYING OLD SC FILE'
            GO TO 990
            END IF
         CALL ZCREAT (VOL, NAME, ISIZE, .TRUE., ASIZE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATING NEW SC FILE'
            GO TO 990
            END IF
         MSGTXT = 'So - no worries - replaced it with a new SC file'
         CALL MSGWRT (6)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, VOL, NAME, T, F, T, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FILE'
         GO TO 990
C                                       Init vis file for write
 40   CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFFER, BO, BIND, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT FILE'
         GO TO 990
C                                       Copy file
 50   DO 100 I = 1,NVIS
         XCOUNT = I
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
C                                       Write new
         NIO = 1
         CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
         IF (IRET.EQ.0) GO TO 100
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILE'
            GO TO 990
 100     CONTINUE
C                                       Check if last call to UVGET
C                                       returned valid data.
 110  IF (IRET.LT.0) XCOUNT = XCOUNT - 1
C                                       Flush output
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.EQ.0) GO TO 120
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILE'
         GO TO 990
C                                       Close input
 120  CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
C                                       Compress output file
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, VOL, CNO, LUN, CATBLK, IRET)
C                                       Update CATBLK: ignore error
      CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER, IRET)
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALCOP: ERROR ',I5,1X,A)
      END
