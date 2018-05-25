      SUBROUTINE GETSTN (IND2, STNNAM, NSTNS, JERR)
C-----------------------------------------------------------------------
C! Reads the VLB station list opened in VBLIN and VBCIT
C# UV Obselete
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
C   GETSTN reads the VLB station list opened in VBLIN and VBCIT
C   on the file specified by FIL2NAME.
C   J. M. Benson    March 1983
C-----------------------------------------------------------------------
      CHARACTER STNNAM(*)*8, STATN*8
      INTEGER   IND2, NSTNS, JERR, ICNT(50), NSTRT, I, NSTOP, K
      INCLUDE 'INCS:DMSG.INC'
      DATA NSTRT /-4/
C-----------------------------------------------------------------------
      JERR = 0
      NSTNS= 0
      DO 10 I = 1,50
         ICNT(I) = I
 10      CONTINUE
C                                       Read a card in the stations
C                                       list file.
 100  READ (IND2,1000,ERR=120,END=200) STATN
      IF (STATN.EQ.'        ') GO TO 100
      NSTNS = NSTNS + 1
      IF (NSTNS.LE.50) GO TO 110
         WRITE (MSGTXT,1001) NSTNS
         CALL MSGWRT (8)
         JERR = 1
         GO TO 999
 110  STNNAM(NSTNS) = STATN
      GO TO 100
C
 120  CONTINUE
      WRITE (MSGTXT,1200)
      CALL MSGWRT (8)
      JERR = 1
      GO TO 999
C
 200  CONTINUE
      WRITE (MSGTXT,2000) NSTNS
      CALL MSGWRT (8)
      IF (NSTNS.GE.0) GO TO 210
         JERR = 1
         GO TO 999
 210  NSTRT = NSTRT + 5
      NSTOP = NSTRT + 4
      WRITE (MSGTXT,2100) (ICNT(K),STNNAM(K), K=NSTRT,NSTOP)
      CALL MSGWRT (8)
      IF (NSTNS.GT.NSTOP) GO TO 210
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A8)
 1001 FORMAT (I3,' STATIONS IN IN2FILE EXCEEDS VBLIN LIMIT OF 50')
 1200 FORMAT ('GETSTN: READ ERROR IN IN2FILE')
 2000 FORMAT (I3,' VLB stations to AIPS ant extension file')
 2100 FORMAT (5(I2,':',A8))
      END
