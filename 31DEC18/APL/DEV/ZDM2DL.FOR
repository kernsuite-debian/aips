      SUBROUTINE ZDM2DL (NWORDS, INBUF, OUTBUF)
C-----------------------------------------------------------------------
C! convert Modcomp REAL*6 and REAL*8 to local double precision
C# Z Service Format
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   Convert Modcomp R*6 (zero padded to D) or D*8 data into local double
C   precision.
C   Inputs:
C      NWORDS   I      Length of the input buffer in words
C      INBUF    D(*)   Input buffer containing Modcomp R*6 (zero padded
C                      to D) or D*8 data
C   Output:
C      OUTBUF   D(*)  Output buffer containing local D.P. data
C   Notes:
C   (1) The INBUF should have its bytes flipped by a call to ZBYTFL
C       which will leave the values split between 2 local integers.
C       Do not call ZI32IL which may swith 16-bit words, or ZI16Il
C       which will expand each 16-bit part to 32-bits.
C   (2) First 32 bits:
C         Expects, after word flip on VMS only, a sign bit in bit 31
C         (1=>negative), bits 22:30 are the  exponent biased by 512,
C         bits 0:21 are the normalized fraction.  Negative values are
C         obtained by 2's complement of the whole word.
C       Second 32 bits:
C         Just extended precision bits.
C   (3) Should work inplace.
C   Generic version --- stubbed.
C-----------------------------------------------------------------------
      INTEGER   NWORDS
      DOUBLE PRECISION INBUF(*), OUTBUF(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Stubbed pending local
C                                       development.
      WRITE (MSGTXT,9999)
      CALL MSGWRT (8)
      IF (NLUSER.GT.0) STOP 'ZDM2DL NEEDS LOCAL DEVELOPMENT'
C
 999  RETURN
C-----------------------------------------------------------------------
 9999 FORMAT ('ZDM2DL: ROUTINE STUBBED PENDING LOCAL DEVELOPMENT')
      END
