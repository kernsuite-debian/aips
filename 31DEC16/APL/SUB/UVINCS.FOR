      SUBROUTINE UVINCS( INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, 
     *   LRPARM, LLREC)
C-----------------------------------------------------------------------
C! Calculates correlator and visibility length for expanded UV data
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
C UV INCrementS: Return Increments in visibility records for 
C un-compressed UV data.
C Inputs:
C  INCS       I     Length of compressed or un-compressed stokes 
C  INCF       I     Length of compressed or un-compressed Frequency group
C  INCIF      I     Length of compressed or un-compressed IF group
C  NRPARM     I     Length of compressed or un-compressed random parms.
C  LREC       I     Length of compressed or un-compressed Visibility
C Output:
C  JNCS       I     Length of un-compressed stokes 
C  JNCF       I     Length of un-compressed frequency group
C  LRPARM     I     Length of un-compressed random parameter (reals)
C  LLREC      I     Length of un-compressed Visibility
C-----------------------------------------------------------------------
      INTEGER INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, LRPARM, LLREC
C-----------------------------------------------------------------------
      LOGICAL ONZE
      INCLUDE 'INCS:DMSG.INC'
      SAVE    ONZE
      DATA    ONZE/.FALSE./
C-----------------------------------------------------------------------
C                                       Assume Uncompressed Visibility 
      JNCS = INCS
      JNCF = INCF
C                                       If IF changes more rapidly 
C                                       than FREQ use IF for frequency 
      IF ((INCIF.GT.0) .AND. (INCIF.LT.INCF)) JNCF = INCIF
C                                       IF compressed data, prepare
      IF (JNCS.EQ.1) THEN
C                                       Compressed has 2 more Random Par
C                                       than Un-comp. (Weight and Scale)
         LRPARM = NRPARM - 2
C                                       Un-comp. Stokes increment is 3
         JNCS = 3
C                                       Increment between Frequencies
         JNCF = JNCF * JNCS
C                                       LLREC is expanded vis length
C                                       Compressed: 1 real per stokes
C                                       Normal: 3 real per stokes
         LLREC  = ((LREC - NRPARM) * 3) + LRPARM
C                                       Tell User Once
         IF (.NOT. ONZE) THEN
            MSGTXT = 'Processing Compressed Data'
            CALL MSGWRT(3)
            ONZE = .TRUE.
            END IF
      ELSE
C                                       Else uncompressed, same value
         LLREC  = LREC
         LRPARM = NRPARM
         END IF
      RETURN
C-----------------------------------------------------------------------
      END

