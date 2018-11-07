      SUBROUTINE BUFGET (NIO, OPARM, IREC, OREC, IDATA, BUFF, TEMPBF)
C-----------------------------------------------------------------------
C! Get a buffer of visibilities from AP, then pack data, if needed
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
C   Get Visibility data from AP then determines whether UV data must 
C   be compressed.
C   Input/Output buffer is assumed to be large enough to hold expanded
C   Visibility
C    Inputs:
C     NIO    I        Number of visiblilities to expand
C     OPARM  I        Output Number of random parameters in visibility
C     IREC   I        Input  Length of compressed vis (1 reals per vis)
C     OREC   I        Output Length of un-comp.   vis (3 reals per vis)
C     IDATA  I        Index into AP array were Visibilities are placed
C    Output
C     BUFF     R(*)   Compressed/Uncompressed array of visibilities
C     TEMPBF   R(*)   Scratch array, large enough to hold expanded vis
C-----------------------------------------------------------------------
      INTEGER   NIO, OPARM, IREC, OREC, IDATA
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
C                                       Read Expanded vis from AP
         CALL QGET (TEMPBF(1), IDATA, WRK, 2)
         CALL QWD
C                                       Compress all points in buff
         CALL BUFPAK( NIO, OPARM, IREC, OREC, TEMPBF(1))
C                                       Copy temp into output buff
         CALL COPY (NIO*IREC, TEMPBF(1), BUFF(1))
      ELSE
C                                       Read record from AP
         CALL QGET (BUFF(1), IDATA, WRK, 2)
C                                       Wait for AP to finish
         CALL QWD
         END IF
C
      RETURN
C-----------------------------------------------------------------------
      END

