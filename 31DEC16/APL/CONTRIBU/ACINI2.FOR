      SUBROUTINE ACINI2 (AIOBLK, ACTNAM, ACNPFR, ACNBPS, ACDELE,
     *                   ACHNAM, IERR)
C-----------------------------------------------------------------------
C! initialize accounting, and other, information
C# Service System
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
C   This program makes the accounting initializations needed for a
C   stand-alone task using the AIPS data files, I/O system, message
C   logging, etc.
C-----------------------------------------------------------------------
      CHARACTER PRHSH1*12, PRHSH2*8, PRTEMP*80, ACNPFR*20, ACTNAM*6,
     *          PEN*4, ACHNAM*48, RSP*8
      INTEGER   I, ACNBPS, XLNB, Q3(3), Q4(3), Q5(3), Q6(3), ACDELE,
     *          IERR, IPOL, AIOBLK(256), LIOBLK(256)
      HOLLERITH QWHRT1(3), QWHRT2(3), QWHRT3(3)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /SETSPC/ RSP
      EQUIVALENCE (QWHRT1, Q3), (QWHRT2, Q4)
      EQUIVALENCE (QWHRT1, LIOBLK(229)), (QWHRT2, LIOBLK(232)),
     *            (QWHRT3, LIOBLK(235))
C-----------------------------------------------------------------------
      DO 3 I = 1, 256
         LIOBLK(I) = AIOBLK(I)
 3       CONTINUE
      PRHSH1 = ' '
      PRHSH2 = ' '
      PRHSH1(3:3)='T'
      PRHSH2(1:1)='H'
      PRHSH1(2:2)='I'
      PRHSH1(1:1)='S'
      PRHSH2(4:4)=PRHSH1(3:3)
      PRHSH2(3:3)=PRHSH1(1:1)
      PRHSH2(2:2)='O'
      PRHSH1(4:4)='E'
      CALL ZTRLOG (4, PRHSH1, 80, PRTEMP, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1992)
         GO TO 990
         END IF
      PRHSH1 = PRTEMP(1:12)
      CALL PASENC(PRHSH1, Q5)
      PRTEMP=' '
      CALL ZTRLOG (4, PRHSH2, 80, PRTEMP, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1992)
         GO TO 990
         END IF
      PRHSH1 = ACHNAM(1:12)
      CALL PASENC(PRHSH1, Q6)
      IERR=0
      DO 100 I = 1,3
         IF (Q3(I).NE.Q5(I)) IERR=1
         IF (Q4(I).NE.Q6(I)) IERR=2
 100     CONTINUE
      IPOL = 0
      IF (QWHRT3(2).EQ.QWHRT3(1)) QWHRT3(2) = 0
      IF (QWHRT3(3).EQ.QWHRT3(1)) QWHRT3(3) = 0
      IF (QWHRT3(3).EQ.QWHRT3(2)) QWHRT3(3) = 0
      DO 200 I = 1,3
         CALL H2CHR(4, 1, QWHRT3(I), PEN)
         IF (PEN.EQ.RSP(4:7)) IPOL = I
 200     CONTINUE
      IF (IPOL.EQ.0) IERR = 3
      IF (IERR.EQ.0) GOTO 999
 990  ACNPFR(6:6) = 'G'
      ACNPFR(2:2) = 'N'
      ACNPFR(1:1) = 'U'
      ACNPFR(4:4) = 'R'
      ACNPFR(5:5) = 'E'
      ACNPFR(13:13) = 'D'
      ACNPFR(7:7) = 'I'
      ACNPFR(8:8) = 'S'
      ACNPFR(9:9) = 'T'
      ACNPFR(3:3) = '-'
      ACNPFR(10:10) = ACNPFR(5:5)
      ACNPFR(12:12) = ACNPFR(5:5)
      ACNPFR(11:11) = ACNPFR(4:4)
      DO 991 I = 14,20
         ACNPFR(I:I) = '!'
 991  CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
 1992 FORMAT ('ACINI2: Problems initializing Accounting information')
      END
