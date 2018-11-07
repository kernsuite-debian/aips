      SUBROUTINE APIO (OPCODE, FLIST, APLOC, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Copies image-like data between disk and "AP memory".
C# AP-util IO-util Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2015
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
C   APIO transfers image-like data between disk files and the array
C   processor.  The file open and close and initialization logic are
C   all contained in this routine.  Information about the file and the
C   the desired properties of the I/O are contained in the array FLIST.
C   APIO can access either cataloged 'MA' type files or scratch files
C   using the /CFILES/ common system.
C   APIO can handle arbitrary row lengths with any size buffer larger
C   than one disk block.  This is done by breaking up the logical
C   records.  NOTE: it is important that data read with APIO have been
C   written by APIO using the same buffer if the buffer is shorter than
C   the row size.  The problem is that APIO will break up logical
C   records if they are  longer than the buffer size and MDISK may leave
C   blank space on the disk if the shorter logical record does not fill
C   a  disk sector.
C    Useage notes:
C     1) Opening the file.  If APIO determines that the file is not open
C        it will do so.  The file can be either a cataloged file or a
C        scratch file using the /CFILES/ common system.  If the
C        catalog slot number given in FLIST is 0 or less the file is
C        assumed to be a scratch file.  File open assumes that the file
C        type is 'MA' (if cataloged), file is opened patiently without
C        exclusive use.
C     2) Initialization.  APIO initializes the I/O using the values in
C        FLIST when it opens the file.  It may be initialized again at
C        any time using OPCODE 'INIT'.  Also switching between 'READ'
C        and 'WRIT' will force flushing the buffer ('WRIT') and
C        initialization.  Any initialization when the current operation
C        is 'WRIT' will cause the buffer to be flushed.
C     3) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.  If the file is being written and a 'CLOS' call
C        is issued, APIO will flush the buffer.  This means that if APIO
C        is being used to write to a disk it MUST be called with
C        OPCODE='CLOS','READ', or 'INIT' to flush the buffer.
C        NOTE: All pending AP operations MUST be complete before calling
C        APIO with opcode 'CLOS'.
C     4) AP timing calls.  APIO calls APWD before getting data from or
C        sending data to the AP but does not call APWR.  The calling
C        routine should call APWR as appropriate.
C    Inputs:
C       OPCODE   C*4  Code for the desired operation.
C                     'INIT' forces the initialization of the I/O.
C                     'READ' reads a logical record from the disk and
C                            sends it to the specified AP location.
C                     'WRIT' Gets data from the AP and writes it to
C                            disk.
C                     'CLOS' Closes the file and flushes the buffer if
C                            necessary.
C       FLIST(22) I   An array containing information about the file
C                     and the I/O. Parts are to be filled in by the
C                     calling routine and are for use by APIO.
C                       1 = LUN, must be filled in,
C                       2 = disk number for catalogs files or
C                           /CFILES/ number for scratch files.
C                       3 = catalog slot number for cataloged files,
C                           .LE. 0 indicates that the file is a scratch
C                           file.
C                       4 = Unused
C                       5 = Length of a logical record (row) in pixels.
C                       6 = Number of rows in a plane.
C                       7 = value to be added to 1 for the block offset.
C                       9-12 = the window desired in the image, 0's=>
C                           all of image.  The logical records must fit
C                           in the buffer and be smaller than BUFSZ
C                           bytes to subimage rows.
C                       13 = Buffer size in bytes.
C           Used by APIO:
C                       14 = FTAB pointer
C                       15 = Number of MDISK calls per logical record.
C                       16 = Current OPCODE,
C                            0 = none, INIT on next call
C                            1 = READ
C                            2 = WRITE
C                       17 = actual length of logical row.
C                       18-22 = Spare.
C       APLOC      I   Base address in AP for data.
C       BUFFER(*)  R   Working buffer.
C    Output:
C       IRET       I   Return code, 0 => OK or
C                                   1 = Bad OPCODE,
C                                   2 = Attempt to window too large
C                                       a file.
C                                   3 = Buffer too small (<NBPS bytes)
C                                   MDISK error codes + 10, or
C                                   MINI3 error codes + 20, or
C                                   ZOPEN error codes + 30.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   FLIST(22), APLOC, IRET
      REAL      BUFFER(*)
C
      CHARACTER CODES(4)*4, FNAME*48
      INTEGER   IERR, NCODE, ICODE, I, FIND, IVOL, ISCR, BIND, BO,
     *   IFACT, INDEX, WIN(4), BUFSZ, IL, NIOBIG, BIGNX, BIGNY
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      SAVE NIOBIG, BIGNX, BIGNY
      DATA NCODE, CODES /4, 'READ','WRIT','INIT','CLOS'/
      DATA T,F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Determine OPcode
      ICODE = 0
      DO 10 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) ICODE = I
 10      CONTINUE
C                                       Check for error.
C                                       Unknown OPcode.
      IF (ICODE.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1010) OPCODE
         GO TO 995
         END IF
C                                       Get FTAB pointer for file
      FIND = FLIST(14)
C                                       Check index is valid
      IF ((FIND.GT.0) .AND. (FIND.LE.20480)) THEN
C                                       index is valid, is file open?
         IF (FLIST(1).EQ.FTAB(FIND)) GO TO 100
         END IF
C-----------------------------------------------------------------------
C                                       File not open - Open it
C                                       If OPCODE='CLOS' return.
      IF (ICODE.EQ.4) GO TO 999
C                                       Reset FLIST
         CALL FILL (8, 0, FLIST(14))
C                                       Determine if cat. or scratch.
         IF (FLIST(3).GT.0) THEN
C                                       Cataloged - make name.
            IVOL = FLIST(2)
            CALL ZPHFIL ('MA', IVOL, FLIST(3), 1, FNAME, IERR)
C                                       Open file.
            CALL ZOPEN (FLIST(1), FLIST(14), IVOL, FNAME, T, F, T,
     *         IERR)
            FIND = FLIST(14)
C                                       Check for error
            IF (IERR.NE.0) THEN
               IRET = IERR + 30
               WRITE (MSGTXT,1020) IERR
               GO TO 995
               END IF
         ELSE
C                                       /CFILES/ SCRATCH file - OPEN
            ISCR = FLIST(2)
            CALL ZPHFIL ('SC', SCRVOL(ISCR), SCRCNO(ISCR), 1, FNAME,
     *         IERR)
            CALL ZOPEN (FLIST(1), FLIST(14), SCRVOL(ISCR), FNAME, T, F,
     *         T, IERR)
            FIND = FLIST(14)
C                                       Check for error
            IF (IERR.NE.0) THEN
               IRET = IERR + 30
               WRITE (MSGTXT,1020) IERR
               GO TO 995
               END IF
            END IF
C-----------------------------------------------------------------------
C                                       Flush Buffer if necessary.
C                                       Check if opcode changed.
 100  IF (ICODE.EQ.FLIST(16)) GO TO 500
C                                       Check if currently not WRIT.
      IF (FLIST(16).EQ.2) THEN
C                                       Make sure data in buffer
         IF (ICODE.NE.4) CALL QWD
C                                       Flush buffer
         CALL MDISK ('FINI', FLIST, FIND, BUFFER, BIND, IERR)
C                                       Check for error.
         IF (IERR.NE.0) THEN
            IRET = 10 + IERR
            WRITE (MSGTXT,1100) IERR
            GO TO 995
            END IF
         END IF
C                                       Check if CLOSE requested.
      IF (ICODE.EQ.4) GO TO 800
C-----------------------------------------------------------------------
C                                       Initialize file.
C                                       Reset FLIST
      CALL FILL (7, 0, FLIST(15))
C                                       If OPCODE='INIT' return and wait
C                                       for READ or WRIT request to do
C                                       the call to  MINIT.
      IF (ICODE.EQ.3) GO TO 999
C                                       Check buffer size
      IF (FLIST(13).LE.NBPS) THEN
C                                       Buffer too small
         IRET = 3
         WRITE (MSGTXT,1200) FLIST(13)
         GO TO 995
         END IF
      BUFSZ = FLIST(13)
      NIOBIG = (FLIST(5) / (1.0 * BUFSZ)) * 2 + 0.999
      NIOBIG = MAX (1, NIOBIG)
      BIGNY = FLIST(6) * NIOBIG
      BIGNX = FLIST(5) / NIOBIG
C                                       Put values in FLIST
      FLIST(15) = NIOBIG
      FLIST(17) = BIGNX
C                                       Set window
      CALL COPY (4, FLIST(9), WIN)
C                                       Check if NIOBIG > 1
      IF (NIOBIG.GT.1) THEN
C                                       If 0's leave as is.
         IF (WIN(1).GT.0) THEN
C                                       Cannot select columns but can
C                                       select rows.
            IF ((FLIST(9).GT.1) .OR. (FLIST(11).LT.FLIST(5))) THEN
C                                       Problem - this is not possible.
               IRET = 2
               WRITE (MSGTXT,1210)
               GO TO 995
               END IF
C                                       Select rows
            WIN(2) = WIN(2) * NIOBIG
            WIN(3) = WIN(3) / NIOBIG
            WIN(4) = WIN(4) * NIOBIG
            IF (WIN(2).LT.WIN(4)) WIN(2) = WIN(2) - (NIOBIG - 1)
            IF (WIN(4).LT.WIN(2)) WIN(4) = WIN(4) - (NIOBIG - 1)
            END IF
         END IF
C                                       Initialize I/O - set BO.
      BO = FLIST(7) + 1
      CALL MINIT (CODES(ICODE), FLIST, FIND, BIGNX, BIGNY, WIN,
     *   BUFFER, FLIST(13), BO, IERR)
C                                       Check for errors
      IF (IERR.NE.0) THEN
         IRET = 20 + IERR
         WRITE (MSGTXT,1230) IERR, CODES(ICODE)
         GO TO 995
         END IF
C-----------------------------------------------------------------------
C                                       Do I/O and transfer to/for AP.
C                                       Do transfers
 500  INDEX = APLOC
      NIOBIG = FLIST(15)
      IFACT = 1
C                                       Setup for big, backwards loops.
      IF ((NIOBIG.GT.1) .AND. (FLIST(12).LT.FLIST(10))) THEN
         IFACT = -1
         BIGNX = FLIST(17)
         INDEX = INDEX + (NIOBIG - 1) * BIGNX
         END IF
C                                       Loop for big transfers.
      DO 540 IL = 1,NIOBIG
         CALL MDISK (CODES(ICODE), FLIST, FIND, BUFFER, BIND, IERR)
         IF (IERR.NE.0) THEN
            IRET = 10 + IERR
            WRITE (MSGTXT,1500) IERR, CODES(ICODE)
            GO TO 995
            END IF
         IF (IL.GT.1) INDEX = INDEX + (IFACT * BIGNX)
         CALL QWD
C                                       READ - put into AP
         IF (ICODE.EQ.1) CALL QPUT (BUFFER(BIND), INDEX, BIGNX, 2)
C                                       Write - get from AP
         IF (ICODE.EQ.2) CALL QGET (BUFFER(BIND), INDEX, BIGNX, 2)
 540     CONTINUE
C                                       Save OPCODE in FLIST
      FLIST(16) = ICODE
      GO TO 999
C-----------------------------------------------------------------------
C                                       Close
 800  CALL ZCLOSE (FLIST, FIND, IERR)
C                                       Check for error
      IF (IERR.NE.0) THEN
         IRET = IERR + 10
         WRITE (MSGTXT,1800) IERR
         GO TO 995
         END IF
C                                       Reset FLIST
      CALL FILL (8, 0, FLIST(14))
      GO TO 999
C                                       An error occured.
 995  CALL MSGWRT (8)
C                                       Return
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('APIO: UNKNOWN OPCODE ',A4)
 1020 FORMAT ('APIO: ZOPEN ERROR',I3)
 1100 FORMAT ('APIO: MDISK ERROR',I3,' FLUSHING BUFFER')
 1200 FORMAT ('APIO: BUFFER SIZE TOO SMALL =',I7)
 1210 FORMAT ('APIO: MUST SPLIT LOG. RECORD - CANNOT WINDOW ROWS')
 1230 FORMAT ('APIO: MINIT ERROR',I3,' ON ',A4)
 1500 FORMAT ('APIO: MDISK ERROR',I3,' ON ',A4)
 1800 FORMAT ('APIO: ZCLOSE ERROR',I3)
      END
