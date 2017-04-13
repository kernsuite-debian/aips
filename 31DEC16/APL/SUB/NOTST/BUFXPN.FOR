      SUBROUTINE BUFXPN (NIO, NRPARM, LREC, LLREC, BUFF)
C-----------------------------------------------------------------------
C! Expands serveral packed visibilities and adds weight
C# UV-util
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
C   Routine to expand an array of packed uv data to unpacked form.  
C   One AIPS visibility is processed at a time.
C   Input/Output buffer is assumed to be large enough to hold expanded
C   Visibility
C    Inputs:
C     NIO    I        Number of visiblilities to expand
C     NRPARM I        Number of random parameters in visibility
C     LREC   I        Length of compressed visibility (1 reals per vis)
C     LLREC  I        Length of un-comp.   visibility (3 reals per vis)
C    In/Output
C     BUFF   R(*)     Compressed/Uncompressed array of visibilities
C-----------------------------------------------------------------------
      INTEGER   NIO, NRPARM, LREC, LLREC
      REAL      BUFF(*)
C
      INTEGER   IVIS, INPTR, OUTPTR, NCORR
C                                       ASSUME fewer than 100 random par
      REAL      RANPAR(100)
C-----------------------------------------------------------------------
C                                       Number of correlators
      NCORR  = LREC - NRPARM
C                                       Inpointer and Outpointer
C                                       Start with last visiblity
      INPTR  = (NIO * LREC)  + 1
      OUTPTR = (NIO * LLREC) + 1
C                                       FOR all visibilities loop
      DO 100 IVIS = 1, NIO
C                                       Work backward to first vis.
         INPTR  = INPTR  - LREC
         OUTPTR = OUTPTR - LLREC
C                                       Copy Random parms
         CALL COPY (NRPARM, BUFF(INPTR), RANPAR)
C                                       Process one visibility
         CALL ZUVXPN (NCORR, BUFF(INPTR+NRPARM), RANPAR(NRPARM-1), 
     *      BUFF(OUTPTR+NRPARM-2))
C                                       Move Random parameters
         CALL COPY (NRPARM-2, RANPAR, BUFF(OUTPTR))
C                                       End for all visiblities loop
 100     CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
      END

