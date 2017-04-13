      SUBROUTINE CHNCOP (INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATOLD, CATNEW, BIF, EIF, FREQID,
     *   SFOFF, BUFF1, FOFF, ISBAND, FINC, IRET)
C-----------------------------------------------------------------------
C! Copies selected portions of the IF table
C# EXT-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2012
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
C   CHNCOP copies selected portions of a Channel (IF) table extension
C   file.
C   Inputs:
C      LUNOLD   I        LUN for old file
C      LUNNEW   I        LUN for new file
C      VOLOLD   I        Disk number for old file.
C      VOLNEW   I        Disk number for new file.
C      CNOOLD   I        Catalog slot number for old file
C      CNONEW   I        Catalog slot number for new file
C      BIF      I        First IF to copy to output.
C      EIF      I        Last IF to copy.
C      FREQID   I        FREQ ID to copy.
C                        If an FQ table exists and FREQID .le. 0 then
C                        if there is only one row in the FQ table, that
C                        row is copied; if there are multiple rows an
C                        error message is returned from CHNDAT.
C      CATOLD   I(256)   Catalog header for old file.
C      SFOFF    D(*)     Frequency offsets to be added to FQ table
C                        offsets in the output FQ table.  These are
C                        usually those of the 1 source in the output
C                        file taken from the SU table in the input.
C   In/out:
C      INVER    I        Version number to copy, 0 => copy all.
C      OUTVER   I        Version number on output file, if more than one
C                        copied (INVER=0) this will be the number of the
C                        first file.  If OUTVER = 0, it will be taken as
C                        1 higher than the previous highest version.
C      CATNEW   I(256)   Catalog header for new file.
C   Output:
C      BUFF1    I(512)   Work buffer
C      FOFF     D(*)     Frequency table.  Must be >= MAXIF
C      ISBAND   I(*)     Sideband table.  Must be >= MAXIF
C      FINC     R(*)     Channel increment table  Must be >= MAXIF
C      IRET     I        Return error code  0 => ok
C                           6 => asked for too many IFs.
C                           other = CHNDAT error.
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW,
     *   CNOOLD, CNONEW, BIF, EIF, BUFF1(*), ISBAND(*), CATNEW(256),
     *   CATOLD(256), FREQID, IRET
      DOUBLE PRECISION SFOFF(*), FOFF(*)
      REAL      FINC(*)
      INTEGER   NIF, IEIF, IBIF
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Read all IFs from old
C                                       CH/FQ table
      CALL CHNDAT ('READ', BUFF1, VOLOLD, CNOOLD, INVER, CATOLD, LUNOLD,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Check number
 30   IBIF = MAX (BIF, 1)
      IEIF = MAX (EIF, IBIF)
      IF (NIF.LT.IEIF) THEN
         IRET = 6
         WRITE (MSGTXT,1030) IEIF, NIF
         GO TO 990
         END IF
C                                       Offset
      DO 40 NIF = IBIF,IEIF
         FOFF(NIF) = FOFF(NIF) + SFOFF(NIF)
 40      CONTINUE
C                                       Number of output IFs.
      NIF = IEIF - IBIF + 1
C                                       Write new FQ table for
C                                       selected IFs
      CALL CHNDAT ('WRIT', BUFF1, VOLNEW, CNONEW, OUTVER, CATNEW,
     *   LUNNEW, NIF, FOFF(IBIF), ISBAND(IBIF), FINC(IBIF),
     *   BNDCOD(IBIF), FREQID, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IRET
         GO TO 990
 50   IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHNCOP: ERROR',I5,' READING OLD IF TABLE')
 1030 FORMAT ('CHNCOP: REQUESTED TOO MANY IFS : ',I4,'>',I4)
 1040 FORMAT ('CHNCOP: ERROR',I5,' WRITING NEW IF TABLE')
      END
