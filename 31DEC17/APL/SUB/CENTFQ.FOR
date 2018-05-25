      SUBROUTINE CENTFQ (DISK, CNO, DIFPIX, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Correct FQ values for centering frequency axis
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   Corrects FQ table ref pixel frequencies for a change in ref pixel
C   Inputs:
C     DISK     I      Volume number
C     CNO      I      Catalog number
C     DIFPIX   R      Pixel shift
C   Input (via common DUVH, MAPHDR)
C     JLOCF    I      frequency axis pointer
C     CATBLK   I(*)   output image header
C   Outputs:
C     IRET     I      Error code, 0 => OK, > 0 => FQ table error
C-----------------------------------------------------------------------
      INTEGER  DISK, CNO, BUFF1(*), BUFF2(*), IRET
      REAL     DIFPIX
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  NIF, NUMFQE, FQID, FQSID(MAXIF), I, VER, LUNI, LUNO,
     *   FQKOLI(MAXFQC), FQNUMI(MAXFQC), FQKOLO(MAXFQC), FQNUMO(MAXFQC),
     *   IFQRNO, LUNTMP, J, NVER, MSGSAV
      REAL     FQCHB(MAXIF), FQTBW(MAXIF), HEDCHB
      DOUBLE PRECISION FQFRQ(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (DIFPIX.EQ.0.0) GO TO 999
      CALL FNDEXT ('FQ', CATBLK, I)
      IF (I.LE.0) GO TO 500
      HEDCHB = CATR(KRCIC+JLOCF)
C                                       Open FQ table
      VER = 1
      LUNI = LUNTMP (1)
      CALL FQINI ('READ', BUFF1, DISK, CNO, VER, CATBLK, LUNI, IFQRNO,
     *   FQKOLI, FQNUMI, NIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FQ TABLE'
         GO TO 990
         END IF
      NUMFQE = BUFF1(5)
C                                       output
      LUNO = LUNTMP (1)
      NVER = 0
      CALL FQINI ('WRIT', BUFF2, DISK, CNO, NVER, CATBLK, LUNO, IFQRNO,
     *   FQKOLO, FQNUMO, NIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FQ TABLE'
         GO TO 990
         END IF
C
      DO 100 I = 1,NUMFQE
         IFQRNO = I
         CALL TABFQ ('READ', BUFF1, IFQRNO, FQKOLI, FQNUMI, NIF, FQID,
     *      FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT FQ TABLE'
            GO TO 990
            END IF
C                                       adjust
         DO 20 J = 1,NIF
            FQFRQ(J) = FQFRQ(J) + DIFPIX * (FQCHB(J) - HEDCHB)
 20         CONTINUE
C                                       write
         IFQRNO = I
         CALL TABFQ ('WRIT', BUFF2, IFQRNO, FQKOLO, FQNUMO, NIF, FQID,
     *      FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FQ TABLE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, IFQRNO, BUFF1, BUFF1, I)
      CALL TABIO ('CLOS', 0, IFQRNO, BUFF2, BUFF2, I)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'FQ', VER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DELETE ORIGINAL'
         GO TO 990
         END IF
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('FQ', NVER, VER, LUNO, LUNI, DISK, DISK, CNO, CNO,
     *   CATBLK, BUFF1, BUFF2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'COPYING TEMP TO OLD'
         GO TO 990
         END IF
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'FQ', NVER, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'DELETE TEMP'
         GO TO 990
         END IF
C                                       check SU table too
 500  CALL CENTSU (DISK, CNO, DIFPIX, BUFF1, BUFF2, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CENTFQ ERROR:',I5,' ON ',A)
      END
