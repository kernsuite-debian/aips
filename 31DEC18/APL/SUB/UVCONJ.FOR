      SUBROUTINE UVCONJ (VSOFF, LVIS, NCHGRD, NSTOKE, LSTOKE, BUFF)
C-----------------------------------------------------------------------
C! Conjugates a Vis record
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
C UV CONJugate: FLIPS U, V and W of a Vis point and
C conjugates the Vis records for N Channels. Weight is not checked.
C Band width synthesis is handled by this routine
C Inputs:
C  VSOFF      I     Offset to real part of Visibility record
C  LVIS       I     Length of one UNCOMPRESSED frequency/IF group
C                   = Number of stokes times length of stokes
C  NCHGRD     I     Number of channels (frequencies) to Conjugate
C  NSTOKE     I     Number of Stokes (correlators) to Conjugate
C  LSTOKE     I     Length of UNCOMPRESSED Correlator record
C Output:
C  BUFF       R(*)  Visibility record
C Input via Common DUVH
C  INCS       I     Stokes increment, If 1, then compressed data
C  NRPARM     I     Number of random parameters
C  LREC       I     Length of compressed or uncompressed Visibility
C Note: The Lenght of the visibility is greater than or equal to
C the product of NSTOKE and LSTOKE, because not all Stokes are always
C conjugated.  (ie LVIS >= NSTOKE*LSTOKE)
C-----------------------------------------------------------------------
      INTEGER VSOFF, LVIS, NCHGRD, NSTOKE, LSTOKE
      REAL BUFF(*)
C-----------------------------------------------------------------------
      INTEGER JJ, JS, JNDX, JSNDX
      REAL TEMPBF(3100), WTSCL(2)
      LOGICAL ISCOMP
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSCD.INC'
C-----------------------------------------------------------------------
      ISCOMP = SCRCAT(KINAX) .EQ. 1
C                                       Flip U, V, W
      BUFF(1+ILOCU) = -BUFF(1+ILOCU)
      BUFF(1+ILOCV) = -BUFF(1+ILOCV)
      BUFF(1+ILOCW) = -BUFF(1+ILOCW)
C                                       If Not Compressed
      IF (.NOT.ISCOMP) THEN
C                                       point to first imaginary comp.
C                                       of first channel
         JNDX = VSOFF + 2
C                                       for all channels
         DO 200 JJ = 1,NCHGRD
C                                       set pointer to first stokes
            JSNDX = JNDX
C                                       for all correlators
            DO 100 JS = 1, NSTOKE
C                                       conjugate imaginary comp
               BUFF(JSNDX) = -BUFF(JSNDX)
C                                       index to next stokes
               JSNDX = JSNDX + LSTOKE
 100           CONTINUE
C                                       set index to next channel/IF
            JNDX = JNDX + LVIS
 200        CONTINUE
      ELSE
C                                       Else compressed, unpack first
C                                       Read Wt and scale
         CALL RCOPY (2, BUFF(WTLOC+1), WTSCL)
C                                       Expand
         CALL ZUVXPN (LREC-NRPARM, BUFF(NRPARM+1), WTSCL, 
     *      TEMPBF(NRPARM-1))
C                                       set index to first vis
         JNDX = VSOFF + 2
C                                       for all channels, conjugate
         DO 400 JJ = 1,NCHGRD
C                                       set pointer to first stokes
            JSNDX = JNDX
C                                       for all correlators
            DO 300 JS = 1, NSTOKE
C                                       conjugate imaginary comp
               TEMPBF(JSNDX) = -TEMPBF(JSNDX)
C                                       index to next stokes
               JSNDX = JSNDX + LSTOKE
 300           CONTINUE
C                                       set index to next channel/IF
            JNDX = JNDX + LVIS
 400        CONTINUE
C                                       Pack Visibility
         CALL ZUVPAK (LREC-NRPARM, TEMPBF(NRPARM-1), WTSCL, 
     *      BUFF(NRPARM+1))
C                                       End if Packed
         END IF
      RETURN
C-----------------------------------------------------------------------
      END

