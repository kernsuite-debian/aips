      SUBROUTINE DWRITE (D2HEAD)
C-----------------------------------------------------------------------
C! translate "DEC" format map header and display parameters
C# Tape
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
C   Print out the juicy parts of a DEC format tape header
C   Does the byte flip call on the header as well
C   Inputs:
C      D2HEAD  I(128)     DECMAP header as directly read from tape
C   Output:
C      D2HEAD  I(128)     Header converted to local char/integer
C-----------------------------------------------------------------------
      INTEGER   D2HEAD(128)
C
      CHARACTER R4IB8*4, DCLASS(15)*4, DIRTY(2)*5, SOURCE*8
      INTEGER   ZAND, MSQUAL, MNO, DFR1, I, NCLASS, ND, DD, D2NX, D2NY,
     *   MSKCLS
      DOUBLE PRECISION    FREQ
      INCLUDE 'INCS:DMSG.INC'
      DATA NCLASS, D2NX, D2NY, MSQUAL, MNO, DFR1  /7, 17,18, 5,6, 9/
C         Masks to extract CLEAN flag and map class
C     DATA MSKCLS /Z00FF/
      DATA MSKCLS /255/
      DATA DCLASS /'????','BEAM','IPOL','QPOL','UPOL','VPOL','P   ',
     *   'PA  ','M   ','SPIX','CONT','TAU ','VEL ','RM  ','HANG'/
      DATA DIRTY /'DIRTY','CLEAN'/
      DATA R4IB8 /'4IB8'/
C-----------------------------------------------------------------------
C                                       convert buffer
      CALL ZC8CL (8, 1, D2HEAD, SOURCE)
      CALL ZI16IL (124, 5, D2HEAD(1), D2HEAD(5))
C                                       test Clean flag
      ND = D2HEAD(NCLASS)
      DD = 1
      IF (ND.LT.0) DD = 2
C                                       clear high bits, get class
      ND = ZAND (ND, MSKCLS)
      ND = ND + 1
      IF (ND.GT.15) ND = 1
C                                       get frequency
      CALL ZR8P4 (R4IB8, D2HEAD(DFR1), FREQ)
      FREQ = FREQ * 1.D-3
C                                       do print out
      WRITE (MSGTXT,1000) SOURCE, (D2HEAD(I), I = MSQUAL,MNO),
     *   DCLASS(ND), DIRTY(DD), D2HEAD(D2NX), D2HEAD(D2NY), FREQ
      CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TPHEAD: ',A8,1X,I4,1X,I5,1X,A4,1X,A5,2I5,F10.3)
      END
