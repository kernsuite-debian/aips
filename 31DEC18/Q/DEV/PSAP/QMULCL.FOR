      SUBROUTINE QMULCL (COMP, FLDECS, ABEAM, BBEAM, NCOMP)
C-----------------------------------------------------------------------
C! Pseudo AP routine: High level Clark CLEAN routine
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Calls B. Clark's QCLNSU routine multiple times to
C   accumulate a number of CLEAN components in one AP call.
C   Inputs:
C      COMP  I   Base address of component vector 4 words per comp.:
C                1 = component intensity.
C                2 = x location (cells)
C                3 = y location (cells)
C                4 = field number
C                NOTE: this vector is the output of this routine.
C      FLDECS I  Field descriptor vector base address.
C                0 = Number of fields (REAL value)
C                And, for each field:
C                0 = Start address of residuals (INTEGER VALUE) (AMAP)
C                    (X,Y,intensity)
C                    NOTE THE INTEGER FORMAT, THIS WORD ONLY!
C                1 = Number of points in the map. (REAL value) (LMAP)
C                2 = Current max. flux density (loc = FCOMP)
C                3 = X position of current max.
C                4 = Y position of current max.
C                5 = CLEAN gain
C      ABEAM  I  Base address of the square (folded) beam patch.
C                The Y direction is most rapid variable:-BY<Y<BY,
C                X varies less rapidly, -1<X<BX
C      BBEAM  I  Beam description vector (BX,BY).
C                In the current QCLNSU BX must equal BY.
C      NCOMP  I  Number of components to CLEAN.
C-----------------------------------------------------------------------
      INTEGER   COMP, FLDECS, ABEAM, BBEAM, NCOMP
C
      INTEGER   AMAP, LMAP, NFIELD, ILOOP, LOOP, JLMAP, IFIELD, JLOOP,
     *   JJNDEX
      LONGINT   JFDV, JCOMP, JNDEX, SAVLOC, JAMAP
      DOUBLE PRECISION AXMAX
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Addresses to 1-rel etc.
      JFDV = FLDECS + PSAPOF
      JCOMP = COMP + PSAPOF
      NFIELD = APCORE(JFDV) + 0.3
C                                        Find maxima in fields
      DO 300 ILOOP = 1,NFIELD
C                                        index to 1st residual in field
         JNDEX = JFDV + ((ILOOP-1)*6) + 1
C                                        location intensity of 1st res.
         JAMAP = APCORI(JNDEX) + 2 + PSAPOF
         SAVLOC = JAMAP
C                                        Assume 1st resid is max
         AXMAX = ABS (APCORE(SAVLOC))
C                                        get number of residuals
         JLMAP = APCORE(JNDEX+1) + 0.3
C                                        If this field has residuals
         IF (JLMAP.GT.1) THEN
C                                        Loop thru residuals
            DO 200 LOOP = 2,JLMAP
               JAMAP = JAMAP + 3
C                                        IF point is greater than max
C                                        Save new maximum.
               IF (AXMAX.LT.ABS(APCORE(JAMAP))) THEN
                  AXMAX = ABS (APCORE(JAMAP))
                  SAVLOC = JAMAP
                  END IF
 200           CONTINUE
C                                        End if field has residuals
            END IF
C                                        Save max intensity, X,Y value
         APCORE(JNDEX+2) = APCORE(SAVLOC)
         APCORE(JNDEX+3) = APCORE(SAVLOC-2)
         APCORE(JNDEX+4) = APCORE(SAVLOC-1)
C                                        End for all fields loop
 300     CONTINUE
C                                        Loop, For all requested comps
      DO 600 JLOOP = 1,NCOMP
C                                        Assume first field has max
         IFIELD = 1
C                                        If more than 1 field
         IF (NFIELD.GT.1) THEN
C                                        Find field with max.
            AXMAX = -10.0
C                                        For all fields
            DO 500 ILOOP = 1,NFIELD
C                                        index to 1st residual in field
               JNDEX = JFDV + ((ILOOP-1)*6) + 1
C                                        If field has residuals
               IF (APCORE(JNDEX+1).GT.0.3) THEN
C                                        If field max is brightest max
                  IF (ABS(APCORE(JNDEX+2)).GT.AXMAX) THEN
C                                        Save new max
                     AXMAX  = ABS(APCORE(JNDEX+2))
                     IFIELD = ILOOP
                     END IF
                  END IF
C                                        End all fields loop
 500           CONTINUE
C                                        End if more than 1 field
            END IF
C                                        index AP field descriptor
         JNDEX = JFDV + ((IFIELD-1)*6) + 1
C                                        Copy max to output CC list
         APCORE(JCOMP  ) = APCORE(JNDEX+2)
         APCORE(JCOMP+1) = APCORE(JNDEX+3)
         APCORE(JCOMP+2) = APCORE(JNDEX+4)
         APCORE(JCOMP+3) = IFIELD
C                                        Set AP loc. of X pos of max
         AMAP = APCORI(JNDEX)
C                                        Number of field residuals in AP
         LMAP = APCORE(JNDEX+1) + 0.3
C                                        Subtract / find max.
         JJNDEX = JNDEX - PSAPOF + 2
         CALL QCLNSU (JJNDEX, AMAP, LMAP, ABEAM, BBEAM)
C                                        Update AP location of next CC
         JCOMP = JCOMP + 4
C                                        End all clean components loop
 600     CONTINUE
C
 999  RETURN
      END
