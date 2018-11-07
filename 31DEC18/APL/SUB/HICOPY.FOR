      SUBROUTINE HICOPY (LUN1, BUF1, LUN2, BUF2, IERR)
C-----------------------------------------------------------------------
C! copies one history file to the end of a second
C# History
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2017
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
C   HICOPY copies all of history file 1 adding it to the end of history
C   file 2.  Both files must already be opened by HIOPEN.  No closing
C   is done.
C   Inputs:
C      LUN1  I         input LUN
C      LUN2  I         output LUN
C   In/Out:
C      BUF1  I(256)    input HI work buffer
C      BUF2  I(256)    output HI work buffer
C   Output:
C      IERR  I         error code = 0 => ok
C                                   1 => file not open
C                           other (<100) from HIADD
C                           other (>100) from ZFIO+100
C                           0 returned if input file is empty
C-----------------------------------------------------------------------
      INTEGER   LUN1, BUF1(256), LUN2, BUF2(256), IERR
C
      INTEGER   HPT1, HPT2, IER, MREC, IND, NREC, NC, I
      CHARACTER HILINE*72
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        locate files
      IERR = 1
      CALL HILOCT ('SRCH', LUN1, HPT1, IER)
      IF (IER.NE.0) GO TO 999
      CALL HILOCT ('SRCH', LUN2, HPT2, IER)
      IF (IER.NE.0) GO TO 999
      IERR = 0
C                                        loop limits
      MREC = HITAB(HPT1+2)
      IND = HITAB(HPT1+1)
      HPT1 = HPT1 + 6
C                                        loop adding MREC lines
      DO 30 I = 1,MREC
         NREC = (I-1) / NHILPR + 1
C                                        read input
         IF (NREC.EQ.HITAB(HPT1)) GO TO 20
            HITAB(HPT1) = NREC
            CALL ZFIO ('READ', LUN1, IND, NREC, BUF1, IERR)
            IF (IERR.NE.0) GO TO 900
C                                        add output
 20      NC = 5 + NHIWPL * MOD (I-1, NHILPR)
         CALL H2CHR (72, 1, BUF1(NC), HILINE)
         CALL HIADD (LUN2, HILINE, BUF2, IERR)
         IF (IERR.NE.0) GO TO 910
 30      CONTINUE
      GO TO 999
C                                        errors
 900  WRITE (MSGTXT,1900) IERR
      IERR = IERR + 100
      GO TO 980
 910  WRITE (MSGTXT,1910) IERR
 980  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('HICOPY READ ERROR',I7)
 1910 FORMAT ('HICOPY ADD ERROR', I7)
      END
