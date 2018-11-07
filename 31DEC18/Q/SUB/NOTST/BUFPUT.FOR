      SUBROUTINE BUFPUT (NIO, IPARM, IREC, OREC, IDATA, BUFF, TEMPBF)
C-----------------------------------------------------------------------
C! Places a buffer of visibilities in AP after unpacking data, if needed
C# AP-util UV
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
C   Determines whether UV data must be un-compressed before putting
C   data into AP.
C   Input/Output buffer is assumed to be large enough to hold expanded
C   Visibility
C    Inputs:
C     NIO    I        Number of visiblilities to expand
C     IPARM  I        Input Number of random parameters in visibility
C     IREC   I        Input  Length of compressed vis (1 reals per vis)
C     OREC   I        Output Length of un-comp.   vis (3 reals per vis)
C     IDATA  I        Index into AP array were Visibilities are placed
C     BUFF     R(*)   Compressed/Uncompressed array of visibilities
C    Output
C     TEMPBF   R(*)   Scratch array, large enough to hold expanded vis
C-----------------------------------------------------------------------
      INTEGER   NIO, IPARM, IREC, OREC, IDATA
      REAL      BUFF(*), TEMPBF(*)
C
      INTEGER   WRK
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
C-----------------------------------------------------------------------
C                                       Calc Expanded Vis length
      WRK = NIO * OREC
C                                       IF compressed, 
      IF (SCRCAT(KINAX).EQ.1) THEN
C                                       Copy compressed vis to temp
         CALL COPY (NIO*IREC, BUFF(1), TEMPBF(1))
C                                       Expand all points in buffer
         CALL BUFXPN (NIO, IPARM, IREC, OREC, TEMPBF(1))
C                                       Move expanded Vis into AP
         CALL QPUT (TEMPBF(1), IDATA, WRK, 2)
      ELSE
C                                       Move Vis into AP
         CALL QPUT (BUFF(1), IDATA, WRK, 2)
         END IF
C                                       Wait for AP to finish
      CALL QWD
C
      RETURN
C-----------------------------------------------------------------------
      END

