      SUBROUTINE TABCT (OPCODE, BUFFER, ICTRNO, CTKOLS, CTNUMV, TIME,
     *   UT1UTC, IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI,
     *   DEPS, DDEPS, TRANGE, IERR)
C-----------------------------------------------------------------------
C! I/O to Calc tables (following initialization by CTINI)
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2005, 2009, 2013
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
C   Does I/O to Calc tables. Usually used after setup by CTINI.
C   Inputs:
C      OPCODE   C*4     Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C      BUFFER   I(512)  I/O buffer and related storage, also defines
C                       file if open. Should have been returned by
C                       TABINI.
C      ICTRNO   I       Next scan number to read or write.
C      CTKOLS   I(12)   The column pointer array in order
C      CTNUMV   I(12)   Element count in each column.
C   Input/output: (written to or read from calc file)
C      TIME     D       Time of ceneter of interval since 0h on ref day
C      UT1UTC   D       Difference between UT1 and UTC
C      IATUTC   D       Difference between IAT and UTC
C      A1IAT    D       Difference between A1 and IAT
C      UT1TYP   C*1     E extrapolated, P preliminary, F final
C      WOBXY    D(2)    X,Y polar offsets
C      WOBTYP   C*1     E extrapolated, P preliminary, F final
C      DPSI     D       nutation in longitude
C      DDPSI    D       derivative of DPSI
C      DEPS     D       nutation in olbliquity
C      DDEPS    D       derivative of DEPS
C      TRANGE   D(2)    Time range in data to which these apply
C   Output:
C      ICTRNO   I       Next CT record number.
C      IERR     I       Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but record deselected.
C   Usage NOTE: use the include 'DCTV.INC' for the declarations and
C   common in CTINI and TABCT.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(512), ICTRNO, CTKOLS(*), CTNUMV(*), IERR
      CHARACTER UT1TYP*1, WOBTYP*1
      DOUBLE PRECISION TIME, UT1UTC, IATUTC, A1IAT, WOBXY(2), DPSI,
     *   DDPSI, DEPS, DDEPS, TRANGE(2)
C
      INCLUDE 'INCS:DCTV.INC'
C
      INTEGER   RECI(MXSPCT)
      REAL      RECORD(MXSPCT)
      HOLLERITH RECH(MXSPCT)
      DOUBLE PRECISION RECD(MXDPCT)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECD, RECORD, RECI, RECH)
C-----------------------------------------------------------------------
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, ICTRNO, RECORD, BUFFER, IERR)
C                                       read
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         CALL DPCOPY (CTNUMV(1), TIME, RECD(CTKOLS(1)))
         CALL DPCOPY (CTNUMV(2), UT1UTC, RECD(CTKOLS(2)))
         CALL DPCOPY (CTNUMV(3), IATUTC, RECD(CTKOLS(3)))
         CALL DPCOPY (CTNUMV(4), A1IAT, RECD(CTKOLS(4)))
         CALL CHR2H  (CTNUMV(5), UT1TYP, 1, RECH(CTKOLS(5)))
         CALL DPCOPY (CTNUMV(6), WOBXY, RECD(CTKOLS(6)))
         CALL CHR2H  (CTNUMV(7), WOBTYP, 1, RECH(CTKOLS(7)))
         CALL DPCOPY (CTNUMV(8), DPSI, RECD(CTKOLS(8)))
         CALL DPCOPY (CTNUMV(9), DDPSI, RECD(CTKOLS(9)))
         CALL DPCOPY (CTNUMV(10), DEPS, RECD(CTKOLS(10)))
         CALL DPCOPY (CTNUMV(11), DDEPS, RECD(CTKOLS(11)))
         CALL DPCOPY (CTNUMV(12), TRANGE, RECD(CTKOLS(12)))
C                                       write it
         CALL TABIO ('WRIT', 0, ICTRNO, RECORD, BUFFER, IERR)
         ICTRNO = ICTRNO + 1
C                                       read
      ELSE
         CALL TABIO ('READ', 0, ICTRNO, RECORD, BUFFER, IERR)
         ICTRNO = ICTRNO + 1
         IF (IERR.LE.0) THEN
            CALL DPCOPY (CTNUMV(1), RECD(CTKOLS(1)), TIME)
            CALL DPCOPY (CTNUMV(2), RECD(CTKOLS(2)), UT1UTC)
            CALL DPCOPY (CTNUMV(3), RECD(CTKOLS(3)), IATUTC)
            CALL DPCOPY (CTNUMV(4), RECD(CTKOLS(4)), A1IAT)
            CALL H2CHR  (CTNUMV(5), 1, RECH(CTKOLS(5)), UT1TYP)
            CALL DPCOPY (CTNUMV(6), RECD(CTKOLS(6)), WOBXY)
            CALL H2CHR  (CTNUMV(7), 1, RECH(CTKOLS(7)), WOBTYP)
            CALL DPCOPY (CTNUMV(8), RECD(CTKOLS(8)), DPSI)
            CALL DPCOPY (CTNUMV(9), RECD(CTKOLS(9)), DDPSI)
            CALL DPCOPY (CTNUMV(10), RECD(CTKOLS(10)), DEPS)
            CALL DPCOPY (CTNUMV(11), RECD(CTKOLS(11)), DDEPS)
            CALL DPCOPY (CTNUMV(12), RECD(CTKOLS(12)), TRANGE)
            END IF
         END IF
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABCT: TABIO ERROR',I5)
      END
